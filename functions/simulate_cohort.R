
simulate_cohort <- function(n,
  seed = NULL, 
  max_follow, 
  baseline_variables, 
  baseline_hook = NULL, 
  initial_visit_values = NULL, 
  visit_schedule, 
  visit_events, 
  visit_measurements, 
  post_visit_hook = NULL, 
  intermediate_events, 
  intermediate_events_hook = NULL, 
  absorbing_events, 
  absorbing_events_hook = NULL, 
  parameter_values, 
  intervene = FALSE, 
  intervene_variables = NULL, 
  intervene_values = NULL, 
  regime = NULL){ 

  if (!is.null(seed)) set.seed(seed) 

  ##
  ## Helper function which removes variables
  ## that we want to draw from X (lava::sim just passes these as they are)
  ##
  simX <- function(x,X,variables = NULL,only_variables = length(variables)>0,keep_id = FALSE,...){
    on.exit({
      if(!is.null(X)) data.table::setDT(X)
    })
    if (length(x$M)>0){
      if (sum(x$M)>0 && NROW(X)>0){
        data.table::setDF(X)
        if (any(variables%in%names(X))){ ## Why if-statement needed?
          X = X[,setdiff(names(X),variables),drop = FALSE]
        }
        d <- data.table::setDT(lava::sim(x = x,X = X,...))
        if("id"%in%names(X) && keep_id) ## Could remove this, not used in the end...
          d$id <- X$id
      }else{
        d <- data.table::setDT(lava::sim(x = x,...))
      }
      if (only_variables[[1]] == TRUE){ ## Why only_variables as a separate argument?
        d = d[,variables,with = FALSE]
      }
    }else{
      d <- data.table()
    }
    return(d)
  }

  ## Remove dropout if interventional distribution is sought
  if (intervene){
    absorbing_events$dropout <- NULL
    if (!is.null(intervene_variables) && !is.null(intervene_values)){
      for (i in seq_len(length(intervene_variables))){
        visit_event <- intervene_variables[i]
        visit_events[[visit_event]] <- "constant"
        parameter_values[[paste0("intercept_",visit_event)]] <- intervene_values[i]
      }
    } else {
      stop("If intervene = TRUE, then intervene_variables and intervene_values must be specified.")
    }
  }

  ##
  ## Baseline
  ##
  if (is.function(baseline_hook)){
    baseline_model <- do.call(baseline_hook,list(baseline_model = baseline_model))
  }
  baseline_model <- make_regression_model(baseline_variables,parameter_values)
  X_baseline <- simX(x = baseline_model,X = NULL,n = n)
  X_baseline[, id := 1:.N]

  ## initialize time dependent variables
  for (v in c(names(intermediate_events),names(visit_measurements))){
    data.table::set(X_baseline,j = v,value = 0)
  }

  # scheduled measurements
  visit_measurements_model <- make_regression_model(outcome_variables = visit_measurements,
                                                    parameter_values = parameter_values)
  # treatment related events
  visit_event_model <- make_regression_model(outcome_variables = visit_events,
                                             parameter_values = parameter_values)
  # event hazard rate model
  intermediate_events_model <- make_regression_model(outcome_variables = intermediate_events,
                                                     parameter_values = parameter_values)
  absorbing_events_model <- make_regression_model(outcome_variables = absorbing_events,
                                                  parameter_values = parameter_values)

  # initialize event_history at time 0
  init_visit_events <- data.table(as.data.frame(as.list(sapply(names(visit_events),function(v){0}))))
  event_history <- cbind(data.table(time = 0,event = "baseline"),X_baseline,init_visit_events)

  if (length(initial_visit_values)>0){
    for (v in names(initial_visit_values)){
      set(event_history,j = v,value = rep(initial_visit_values[[v]],NROW(event_history)))
    }
  }

  ## PATCH: index event_history by id (helps later joins/appends)
  data.table::setindex(event_history, id)

  ##
  ## Loop until an absorbing event
  ##

  ## PATCH: keep a rolling state of at-risk subjects (latest row per id)
  last_entry <- data.table::copy(event_history)  # at baseline, 1 row per id, time=0
  data.table::setkey(last_entry, id)

  while (nrow(last_entry)>0) {

    ## draw time of next scheduled visit, allowing for skipped visits
    nrisk <- nrow(last_entry)  ## PATCH: avoid shadowing 'n' parameter
    skipped_visits <- rbinom(n = nrisk, 3, visit_schedule[["skip"]])
    next_visit <- skipped_visits*visit_schedule[["mean"]] +
                  pmax(rnorm(n = nrisk, mean = visit_schedule[["mean"]], sd = visit_schedule[["sd"]]), 0.1)

    ## apply hook for absorbing events
    if (is.function(absorbing_events_hook)){
      absorbing_events_model <- do.call(absorbing_events_hook,
        list(absorbing_event_model = absorbing_events_model,
             event_history = last_entry))
    }

    ## apply hook for intermediate events
    if (is.function(intermediate_events_hook)){
      intermediate_events_model <- do.call(intermediate_events_hook,
        list(intermediate_events_model = intermediate_events_model,
             event_history = last_entry))
    }

    ## draw next event time as min of visit/intermediate/absorbing
    lt1 <- simX(intermediate_events_model,
                variables = names(intermediate_events),
                n = nrisk,
                X = last_entry,
                p = parameter_values)
    lt2 <- simX(absorbing_events_model,
                variables = names(absorbing_events),
                n = nrisk,
                X = last_entry,
                p = parameter_values)

    # ensure matrices
    lt1 <- as.matrix(lt1)
    lt2 <- as.matrix(lt2)

    latent_times <- matrix(NA_real_, nrow = nrisk, ncol = ncol(lt1) + ncol(lt2) + 1)
    latent_times[, seq_len(ncol(lt1))] <- lt1
    latent_times[, ncol(lt1) + seq_len(ncol(lt2))] <- lt2
    latent_times[, ncol(lt1) + ncol(lt2) + 1] <- next_visit
    colnames(latent_times) <- c(colnames(lt1), colnames(lt2), "visit")

    idx <- max.col(-latent_times, ties.method = "first")
    current_event <- data.table(
      id = last_entry[, id],
      event = colnames(latent_times)[idx],
      time = last_entry[, time] + latent_times[cbind(seq_len(nrow(latent_times)), idx)]
    )

    ## Divide subjects at risk into two categories
    visit_inter_ids <- current_event[event %chin% c("visit",names(intermediate_events)), .(id, time, event)]
    absorbed_ids <- current_event[event %chin% names(absorbing_events), .(id, time, event)]

    ## Cases of non-absorbed events
    if(nrow(visit_inter_ids)>0){

      ## Keep last_entry rows for those ids (to compute histories)
      last_entry_visit <- last_entry[visit_inter_ids, on = "id", nomatch = 0L]

      ## Start from last_entry and overwrite time/event to the NEW ones
      update_visit <- last_entry[visit_inter_ids, on = "id", nomatch = 0L]
      if (nrow(update_visit)) {
        data.table::setkey(update_visit, id)
        data.table::setkey(visit_inter_ids, id)
        update_visit[visit_inter_ids, `:=`(time = i.time, event = i.event)]
      }

      ## Increase count of intermediate events
      for(vv in names(intermediate_events)){
        update_visit[event == vv, (vv) := get(vv) + 1L]
      }

      ## draw visit measurements conditional on updated history
      update_measurements <- simX(visit_measurements_model,
                                  n = nrow(update_visit),
                                  p = parameter_values,
                                  variables = names(visit_measurements),
                                  X = update_visit)
      for (new in names(update_measurements)){
        data.table::set(update_visit, j = new, value = update_measurements[[new]])
      }

      ## draw visit treatment actions conditional on history
      update_treatment <- simX(visit_event_model,
                               n = nrow(update_visit),
                               p = parameter_values,
                               variables = names(visit_events),
                               X = update_visit)

      ## post-visit hook
      if (is.function(post_visit_hook)){
        update_visit <- post_visit_hook(update_event_history = update_visit,
                                        update_treatment = update_treatment,
                                        update_measurements = update_measurements,
                                        event_history = event_history)
      }

      ## adding the updated treatment status
      for (xx in names(update_treatment)){
        if (length(histxx <- strsplit(xx,"^History_")[[1]])>1){
          ## update treatment history (cumulative)
          data.table::set(update_visit, j = xx,
                          value = last_entry_visit[[xx]] + update_treatment[[histxx[[2]]]])
        } else {
          data.table::set(update_visit, j = xx, value = update_treatment[[xx]])
        }
      }
    } else {
      update_visit <- data.table(time = numeric(), id = numeric())
    }

    ## Prepare absorbed updates (overwrite time/event to absorbing ones)
    update_absorbed <- last_entry[absorbed_ids, on = "id", nomatch = 0L]
    if (nrow(update_absorbed)) {
      data.table::setkey(update_absorbed, id)
      data.table::setkey(absorbed_ids, id)
      update_absorbed[absorbed_ids, `:=`(time = i.time, event = i.event)]
    }

    ## Update full event_history (as before)
    event_history <- data.table::rbindlist(
      list(event_history, update_visit, update_absorbed), use.names = TRUE, fill = TRUE
    )

    ## PATCH: Update the rolling risk set (last_entry) in place — no join with event_history
    if (nrow(update_visit)) {
      data.table::setkey(last_entry, id)
      data.table::setkey(update_visit, id)
      cols <- intersect(names(last_entry), names(update_visit))
      last_entry[update_visit, (cols) := mget(paste0("i.", cols))]
    }
    if (nrow(update_absorbed)) {
      ## Remove absorbed ids from risk set
      last_entry <- last_entry[!update_absorbed, on = "id"]
    }
    ## Enforce max follow-up for next iteration's risk set
    last_entry <- last_entry[time <= max_follow]
  }

  ## Remove events after max_follow
  event_history[time>max_follow, c("time","event") := list(max_follow,"dropout")]
  setcolorder(event_history,"id")
  event_history[, c("i.time", "i.event") := NULL]  # Remove any leftover columns from joins
  return(event_history[])
}



initialize_parameter_values <- function(baseline_variables,
                                        baseline_visit = NULL,
                                        visit_schedule,
                                        visit_events,
                                        visit_measurements,
                                        intermediate_events,
                                        absorbing_events,
                                        intercept_value = 0,
                                        variance_value = 1,
                                        scale_value = 1/100,
                                        effect_value = 0){

    vars <- c(baseline_variables,
              baseline_visit,
              visit_measurements,
              visit_events)
    normal <- vars[vars == "normal"]
    variances <- sapply(normal, function(v){variance_value})
    names(variances) <- paste0("variance_", names(variances))
    intercepts <- sapply(vars,function(v){intercept_value})
    names(intercepts) <- paste0("intercept_",names(intercepts))
    events <- c(absorbing_events,intermediate_events)
    scales <- sapply(events,function(v){scale_value})
    names(scales) <- paste0("scale_",names(scales))

    baseline_variables <- names(baseline_variables)
    baseline_visit <- names(baseline_visit)
    visit_schedule <- names(visit_schedule)
    visit_events <- names(visit_events)
    visit_measurements <- names(visit_measurements)
    intermediate_events <- names(intermediate_events)
    absorbing_events <- names(absorbing_events)

    construct_effects <- function(vector1,vector2,effect_value){
        combinations <- data.table::setDT(expand.grid(vector1, vector2,stringsAsFactors = FALSE))
        # remove self-effects
        combinations <- combinations[Var1 != Var2]
        setNames(rep(effect_value, nrow(combinations)), paste0("effect_",apply(combinations, 1, paste, collapse = "_")))
    }
    effects_baseline_baseline <- construct_effects(baseline_variables,baseline_variables,effect_value = effect_value)
    if (length(baseline_visit)>0){
        effects_baseline_baseline_visit <- construct_effects(baseline_variables,baseline_visit,effect_value = effect_value)
    }

    effects_baseline_events <- construct_effects(baseline_variables,
                                                 events,
                                                 effect_value = effect_value)
    if (length(visit_events)>0){
        effects_baseline_visit <- construct_effects(baseline_variables,
                                                    c(visit_events,visit_measurements),
                                                    effect_value = effect_value)
    }else{
        effects_baseline_visit <- NULL
    }
    if (length(intermediate_events)>0){
        effects_timevar <- construct_effects(c(intermediate_events,visit_measurements,visit_events),
                                             events,
                                             effect_value = effect_value)
    }else{
        effects_timevar <- NULL
    }
    as.list(c(intercepts,
              variances,
              scales,
              effects_baseline_baseline,
              effects_baseline_baseline_visit,
              effects_baseline_visit,
              effects_baseline_events,
              effects_timevar))
}

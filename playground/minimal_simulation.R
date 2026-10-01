### minimal_simulation.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: jun 18 2026 (13:21) 
## Version: 
## Last-Updated: okt  1 2026 (12:31) 
##           By: SADS0006
##     Update #: 31
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

#------------------------Different-hooks---------------------------

# Randomized baseline treatment
randomize_baseline_treatment <- function(X){
    X[,lira := 1*(randomized_treatment == 1)]
    X[,placebo := 1*(randomized_treatment == 0)]
    #X[,randomized_treatment := NULL]
}

#------------------------Function-defining-setting-list------------
get_minimal_setting <- function(){
    #browser()
    max_follow <- 1.9

    baseline_variables <- list(
        sex = "binomial",
        age = "normal",
        hba1c = "normal"
    )

    baseline_visit <- list(
        randomized_treatment = "binomial"
    )

    absorbing_events <- list(
        death = "Weibull",
        dropout = "Weibull"
    )
    
    intermediate_events <- list(
        nausea.and.vomiting.symptoms = "Weibull" 
    )

    visit_measurements <- list(
        hba1c_change = "normal"
    )

    visit_events <- list(
        lira = "const",
        placebo = "const"
    )

    visit_schedule <- list(
        mean = 1, sd = 0, skip = 0,
        schedule = seq(from = 0, to = 10, by = 1),
        minimum_time_between_visits = 0
    )

    ipv = initialize_parameter_values(
        baseline_variables = baseline_variables,
        absorbing_events = absorbing_events,
        baseline_visit = baseline_visit,
        intermediate_events = intermediate_events,
        visit_measurements = visit_measurements,
        visit_events = visit_events,
        visit_schedule = visit_schedule)

    list(
        max_follow = max_follow,
        baseline_variables = baseline_variables,
        baseline_visit = baseline_visit,
        absorbing_events = absorbing_events,
        intermediate_events = intermediate_events,
        visit_measurements = visit_measurements,
        visit_events = visit_events,
        visit_schedule = visit_schedule,
        parameter_values = ipv
    )
}


#------------------------Analysis/Results--------------------------

if (FALSE){
    # Need the initialization steps for the sim.
    # tar_load_globals outside or here?
    
    r <- list.files("c:/Users/sads0006/Desktop/rtmle/R/",
                    pattern =  "\\.[Rr]$",
                    full.names = TRUE)
    invisible(lapply(r, source))
    source("c:/Users/sads0006/Desktop/followme-SAMS/playground/minimal_simulation.R")
    source("c:/Users/sads0006/Desktop/followme-SAMS/functions/initialize_parameter_values.R")
    
    library(data.table)
    
    # Get setting and set parameter values
    p0 <- get_minimal_setting()
    p0$parameter_values <- modifyList(
        p0$parameter_values,
        list(
            scale_death = 0.1,
            scale_dropout = 0,
            scale_nausea.and.vomiting.symptoms = 0,
            effect_lira_nausea.and.vomiting.symptoms = 0,
            effect_lira_dropout = 0,
            effect_lira_death = -1
        ))

    # Simulate cohort
    d <- do.call(
        simulate_cohort,
        c(
            list(
                n = 1,
                post_baseline_visit_hook = randomize_baseline_treatment
            ),
            p0
        )
    )

    rd <- register_format(d,
                          treatment_variables = c("lira", "placebo")
                          )
    
    # Naive estimates. Does it change things if deaths occur after the
    # specified time grid for rtmle. Yes, now made sure they are not counted
    # so risk of specific outcome associated with a treatment:
    # number of outcome with treatment by time t / number randomized with said treatment
    data <- d[, .(time =  max(time),
                  death = as.integer(any(event == "death")),
                  treatment = fifelse(first(lira) == 1, "lira", "placebo")
                  ),
              by = id]
    drate_lira <- data[treatment == "lira" & death == 1 & time <= 1.5, .N]/data[treatment == "lira",.N]
    drate_placebo <- data[treatment == "placebo" & death == 1 & time <= 1.5, .N]/data[treatment == "placebo",.N]
    drate_all <- data[death == 1 & time <= 1.5,.N]/data[,.N]
    diff <- drate_all - drate_lira

    # Kaplan-Meier risk estimate
    grid <- seq(0, 1.5, .5)
    fit <- prodlim::prodlim(
                        prodlim::Hist(time,death) ~ treatment,
                        data = data
                    )
    risk <- predict(
                         fit,
                         newdata = data.frame(treatment =  c("lira","placebo")),
                         times = grid,
                         type = "risk")
    
    # rTMLE estimates
    x <- rtmle_init(
        time_grid = seq(0, 1.5, .5),
        name_id = "id",
        name_outcome = "death",
        name_competing =  NULL,
        name_censoring = "dropout"
    )
    x <- add_baseline_data(x, rd$baseline_data)
    x <- add_long_data(x,outcome_data = rd$timevar_data$death,
                       competing_data = NULL,
                       censored_data = rd$timevar_data$dropout,
                       timevar_data = rd$timevar_data[c("hba1c_change","lira","nausea.and.vomiting.symptoms","placebo")])
    x <- discretize_data(x,
                         start_followup_date = 0)
    
    # Setup protocols
    x <- protocol(x,
                  name = "lira",
                  intervention = data.table(time = x$intervention_nodes,
                                            "lira" = factor(rep(1,3),levels = 0:1)))
    x <- protocol(x,
                  name = "placebo",
                  intervention = data.table(time = x$intervention_nodes,
                                            "placebo" = factor(rep(1,3),levels = 0:1)))
    x <- prepare_rtmle_data(x)
    x <- model_formula(x, exclusion_rules = list("placebo" = "lira_0", "lira" = "placebo_0"))
    x <- target(x,name = "Treatment",regimes = c("placebo","lira"))
    x <- run_rtmle(x, time_horizon = 3, learner = "learn_glmnet")
    
}

######################################################################
### minimal_simulation.R ends here

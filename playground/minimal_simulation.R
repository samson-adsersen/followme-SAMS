### minimal_simulation.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: jun 18 2026 (13:21) 
## Version: 
## Last-Updated: okt 22 2026 (13:35) 
##           By: sads0006
##     Update #: 39
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

#------------------------Helper-functions--------------------------

naive_risk_est <- function(cohort, t0,
                           competing_treatments,
                           outcome){

    d <- cohort[, .(time = max(time),
                    outcome = as.integer(any(event == outcome)),
                    treatment = factor(
                        if (first(get(competing_treatments[[1]])) == 1) {
                            competing_treatments[[1]]
                        } else {
                            competing_treatments[[2]]
                        },
                        levels = competing_treatments
                    )),
                    by = id]

    rate1 <- d[treatment == competing_treatments[[1]] &
                outcome == 1 & time <= t0, .N]/d[treatment == competing_treatments[[1]], .N]
    rate2 <- d[treatment == competing_treatments[[2]] &
                outcome == 1 & time <= t0, .N]/d[treatment == competing_treatments[[2]], .N]
    rate_all <- d[outcome == 1 & time <= t0, .N]/d[,.N]
    diff <- abs(rate1 - rate2)

    out <- c(
        setNames(
            list(rate1, rate2),
            competing_treatments
        ),
        list(
            rate_all = rate_all,
            difference = diff))
    out[]
}

KM_risk_est <- function(cohort, t0, dt, competing_treatments, outcome){

    d <- cohort[, .(time = max(time),
                    outcome = as.integer(any(event == outcome)),
                    treatment = factor(
                        if (first(get(competing_treatments[[1]])) == 1) {
                            competing_treatments[[1]]
                        } else {
                            competing_treatments[[2]]
                        },
                        levels = competing_treatments
                    )),
                by = id]

    grid <- seq(0, t0, dt)
    fit <- prodlim::prodlim(
                        prodlim::Hist(time, outcome) ~ treatment,
                        data = d
                    )
    risk <- predict(
                         fit,
                         newdata = data.frame(treatment = competing_treatments),
                         times = grid,
        type = "risk")

    risk <- setNames(
            risk,
            competing_treatments
        )
    
    out <- c(
        risk,
        diff = abs(risk[[competing_treatments[1]]][as.integer(1+t0/dt)]-
                   risk[[competing_treatments[2]]][as.integer(1+t0/dt)])
    )
    out[]
}



### TJEK TJEK - SOMETHING WRONG WITH PROTOCOLS
rtmle_risk_est <- function(cohort, t0, dt, competing_treatments,
                           outcome, censoring, learner){

    # Initializing
    rd <- register_format(cohort,
                          treatment_variables = competing_treatments)
    x <- rtmle_init(
        time_grid = seq(0, t0, dt),
        name_id = "id",
        name_outcome = outcome,
        name_competing =  NULL,
        name_censoring = censoring
    )
    x <- add_baseline_data(x, rd$baseline_data)
    x <- add_long_data(x,outcome_data = rd$timevar_data[[outcome]],
                       competing_data = NULL,
                       censored_data = rd$timevar_data[[censoring]],
                       # TJEK: hvad sker der her
                       timevar_data = rd$timevar_data[c("hba1c_change","lira","nausea.and.vomiting.symptoms","placebo")])
    x <- discretize_data(x,
                         start_followup_date = 0)
    
    # Setup protocols
    ## x <- protocol(x,
    ##               name = competing_treatments[1],
    ##               intervention = data.table(time = x$intervention_nodes,
    ##                                         treatment = factor(
    ##                                             rep(1,3), levels = 0:1))
    ##               |> data.table::setnames("treatment", competing_treatments[1]))
    ## x <- protocol(x,
    ##               name = competing_treatments[2],
    ##               intervention = data.table(time = x$intervention_nodes,
    ##                                         treatment = factor(
    ##                                             rep(0,3),levels = 0:1))
    ##               |> data.table::setnames("treatment", competing_treatments[2]))
    x <- protocol(
        x,
        name = competing_treatments[1],
        intervention = c(1, 0),
        treatment_variables = competing_treatments,
        multiple_treatment_factorization = "sequential"
    )

    x <- protocol(
        x,
        name = competing_treatments[2],
        intervention = c(0, 1),
        treatment_variables = competing_treatments,
        multiple_treatment_factorization = "sequential"
    )
    
    # Setup model and run rtmle
    x <- prepare_rtmle_data(x)
    x <- model_formula(x,
                       exclusion_rules = setNames(
                           list(
                               paste0(competing_treatments[1], "_0"),
                               paste0(competing_treatments[2], "_0")
                           ),
                           c(competing_treatments[2], competing_treatments[1])))
    x <- target(x, name = "Treatment", regimes = competing_treatments)
    x <- run_rtmle(x, time_horizon = 3, learner = learner)

    rtmle_diff <- abs(x$estimate$Main_analysis$Estimate[1]-x$estimate$Main_analysis$Estimate[2])
    
    out <- data.table(
        main_analysis = x$estimate$Main_analysis,
        diff = rtmle_diff
    )
    out[]
    
}

#------------------------Analysis/Results--------------------------

if (FALSE){

    # Example of one simulation run
    
    # Need the initialization steps for the sim.
    r <- list.files("c:/Users/sads0006/Desktop/rtmle/R/",
                    pattern =  "\\.[Rr]$",
                    full.names = TRUE)
    invisible(lapply(r, source))
    source("c:/Users/sads0006/Desktop/followme-SAMS/playground/minimal_simulation.R")
    source("c:/Users/sads0006/Desktop/followme-SAMS/functions/initialize_parameter_values.R")
    
    library(data.table)

    p0 <- get_minimal_setting()
    p0$parameter_values <- modifyList(
        p0$parameter_values,
        list(
            scale_death = 0.1,
            scale_dropout = 0,
            effect_lira_death = -1
        ))

    d0 <- do.call(
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

    naive_risk <- naive_risk_est(d, 1.5, c("lira", "placebo"), "death")

    KM_risk <- KM_risk_est(d, 1.5, 0.5, c("lira", "placebo"), "death")

    rtmle_risk <- rtmle_risk_est(d, 1.5, 0.5, c("lira", "placebo"), "death", "dropout", "learn_glmnet")
    
}


# 






######################################################################
### minimal_simulation.R ends here

### minimal_simulation.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: jun 18 2026 (13:21) 
## Version: 
## Last-Updated: sep 15 2026 (14:38) 
##           By: SADS0006
##     Update #: 14
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
    X[,randomized_treatment := NULL]
}

# Post visit medication stop
randomized_medication_stop <- function(X){
    #X[]
}


#------------------------Function-defining-setting-list------------
get_minimal_setting <- function(){
    #browser()
    max_follow <- 1.5

    baseline_variables <- list(
        sex = "binomial",
        age = "normal"
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
        metformin = "binomial"
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
    r <- list.files("c:/Users/sads0006/Desktop/rtmle-SAMS/R/",
                    pattern =  "\\.[Rr]$",
                    full.names = TRUE)
    invisible(lapply(r, source))
    source("c:/Users/sads0006/Desktop/followme-SAMS/playground/minimal_simulation.R")
    source("c:/Users/sads0006/Desktop/followme-SAMS/functions/register_format.R")
    
    # Get setting and set parameter values
    p0 <- get_minimal_setting()
    p0$parameter_values <- modifyList(
        p0$parameter_values,
        list(
            scale_death = 0.1,
            scale_dropout = 0,
            scale_nausea.and.vomiting.symptoms = 0,
            effect_lira_nausea.and.vomiting.symptoms = 1,
            effect_lira_dropout = 0.2,
            effect_lira_death = -1
        ))

    d <- do.call(
        simulate_cohort,
        c(
            list(
                n = 10000,
                post_baseline_visit_hook = randomize_baseline_treatment
            ),
            p0
        )
    )

    subject.data <- d[, .(lira =  first(lira),
                          placebo =  first(placebo),
                          death = as.integer(any(event == "death"))),
                      by = id]
    naive <- subject.data[, .(risk = mean(death)), by = lira]
    est <- naive$risk[2] - naive$risk[1]
    
    rd <- register_format(d)
    

    # rTMLE estimates
    x <- rtmle_init(
        time_grid = seq(0, 1.5, .5),
        name_id = "id",
        name_outcome = "death",
        name_competing =  NULL,
        name_censoring = "dropout"
    )
    x <- add_baseline_data(x, rd$baseline_data)
    x <- do.call(add_long_data, c(list(x), rd[-1]))
    # Setup protocols
    x <- protocol(x,
                  name = "use_lira",
                  intervention = data.table(time = x$intervention_nodes,
                                            "lira" = factor(1,0:1)))
    x <- protocol(x,
                  name = "placebo",
                  intervention = data.table(time = x$intervention_nodes,
                                            "placebo" = factor(1,0:1)))
    x <- long_to_wide(x)
    x <- prepare_rtmle_data(x)
    x <- model_formula(x, exclusion_rules = list("placebo" = "lira_0", "lira" = "placebo_0"))
    x <- run_rtmle(x, time_horizon = 3, learner = learner)
    
    
    # Counts of events / total for naive estimate
    d[lira == 1 & event == "death", .N]/d[lira == 1 & event == "baseline", .N]
    d[placebo == 1 & event == "death", .N]/d[placebo == 1 & event == "baseline", .N]
    
}






######################################################################
### minimal_simulation.R ends here

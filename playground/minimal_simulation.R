### minimal_simulation.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: jun 18 2026 (13:21) 
## Version: 
## Last-Updated: jun 18 2026 (13:59) 
##           By: SADS0006
##     Update #: 6
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
        death <- "Weibull"
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

    p0 <- get_minimal_setting()

    p0$parameter_values <- modifyList(
        p0$parameter_values,
        list(
            scale_death = 10000000000,
            scale_nausea.and.vomiting.symptoms = 1/5,
            effect_lira_hba1c_change = -0.8,
            effect_lira_nausea.and.vomiting.symptoms = 0.7,
            effect_lira_dropout = 0.2,
            effect_lira_death = -0.1,
            effect_placebo_hba1c_change = 0,
            effect_placebo_nausea.and.vomitting.symptoms = 0,
            effect_placebo_death = 0
        )
    )
    
    d <- do.call(
        simulate_cohort,
        c(
            list(
                n = 1000,
                post_baseline_visit_hook = randomize_baseline_treatment
            ),
            p0
        )
    )

    d[lira == 1 & event == "death", .N]

    d[lira == 1, by = id, .N]
    
}






######################################################################
### minimal_simulation.R ends here

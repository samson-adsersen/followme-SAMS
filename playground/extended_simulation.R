### extended_simulation.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: okt  1 2026 (10:17) 
## Version: 
## Last-Updated: okt  8 2026 (14:53) 
##           By: sads0006
##     Update #: 21
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:


# Randomized baseline treatment
randomize_baseline_treatment <- function(X){
    X[,lira := 1*(randomized_treatment == 1)]
    X[,placebo := 1*(randomized_treatment == 0)]
    #X[,randomized_treatment := NULL]
}

# Post visit medication stop
update_data <- function(update_information, visit_measurements, baseline_variables){

    # Want to incorporate changes in baseline variables from visit measurements (changes)
    for (vm in names(visit_measurements)) {
        for (bv in names(baseline_variables)) {
            if (startsWith(vm, bv)) {
                set(update_information,j = bv, value = update_information[[bv]]+update_information[[vm]])
            }
        }}

    update_information[]
}


#------------------------Function-defining-setting-list------------
get_extended_setting <- function(){
    
    max_follow <- 10

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
        # right now does not really make sense for a medical study
        # but i think in principle could just have multiple different
        # update variables affected by different parameters.
        lira = "binomial",
        placebo = "binomial"
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
    p <- get_extended_setting()
    p$parameter_values <- modifyList(
        p$parameter_values,
        list(
            scale_death = 0.1,
            scale_dropout = 0,
            intercept_update_treatment = 0.1,
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
                n = 10000,
                post_baseline_visit_hook = randomize_baseline_treatment,
                pre_treatment_update_hook = randomized_treatment_changes
            ),
            p
        )
    )

    rd <- register_format(d,
                          treatment_variables = c("lira", "placebo")
                          )
    
    
}

######################################################################
### extended_simulation.R ends here

### test_setting.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: jun  4 2026 (13:53) 
## Version: 
## Last-Updated: jun  4 2026 (15:01) 
##           By: SADS0006
##     Update #: 3
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

test_setting <- function() {

    # Maximum amount of followup events in the time-series
    max_follow <- 20

    # Baseline variables and their distributions
    baseline_variables <- list(
        age = "normal",
        weight = "normal",
        sex = "binomial"
    )

    # List of events ending the time-series (death/dropout/censoring)
    absorbing_events <- list(
        death = "Weibull",
        dropout = "Weibull" # reasonable that some people dont have continuous entries
    )

    # List of other possible events between visits.
    intermediate_events <- list(        
        foot = "binomial"
    )

    # What is measured at a visit
    visit_measurements <- list(
        non_diabetic_diagnosis = "binomial",
        diabetic_diagnosis = "binomial" # Think work out probabilities for T1 and T2 in post
    )

    # What info is given at baseline - age weight?
    baseline_visit <- list(
        age = "normal",
        weight = "normal",
        sex = "binomial"
    )

    # What could happen based on the visit (Medication/treatment)
    visit_events <- list(
        non_diabetic_treatment = "binomial",
        insulins = "binomial",
        oads = "binomial"
    )

    # Distribution of visits.
    visit_schedule <- list(
        mean = 36, sd = 12, skip = 0
    )

    # Generate initial parameter values list
    #if (FALSE){
    ipv = initialize_parameter_values(
        baseline_variables = baseline_variables,
        absorbing_events = absorbing_events,
        baseline_visit = baseline_visit,
        intermediate_events = intermediate_events,
        visit_measurements = visit_measurements,
        visit_events = visit_events,
        visit_schedule = visit_schedule)
    dput(ipv)
    #}
    
    # Output
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

if (FALSE) {

    # Initialize parameters
    
    
    # tune parameters here
    p$parameter_values$scale_dropout <- 0
    p$parameter_values$intercept_age <- 45
    p$parameter_values$variance_age <- 20
    p$parameter_values$intercept_weight <- 80
    p$parameter_values$variance_weight <- 20
    
    d <- do.call(
        simulate_cohort,
        c(list(n = 1000),#, potential hooks),
          p)
        )
    
}


######################################################################
### test_setting.R ends here

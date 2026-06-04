### LMDB_setting.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: jun  4 2026 (12:41) 
## Version: 
## Last-Updated: jun  4 2026 (13:17) 
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

LMDB_setting <- function(){
    
    # Maximum amount of followup events in the time-series
    max_follow <- 24

    # Baseline variables and their distributions
    baseline_variables <- list(
        diagnosis = "lognormal"
    )

    # List of events ending the time-series (death/dropout/censoring)
    absorbing_events <- list(
        death = "Weibull",
        dropout = "Weibull"
    )

    # List of other possible events between visits.
    intermediate_events <- list(        
        foot = "binomial"
    )

    # What is measured at a visit
    visit_measurements <- list(
        
    )

    # What info is given at baseline
    baseline_visit <- list(
        
    )

    # What could happen based on the visit (Medication/treatment)
    visit_events <- list(

    )

    # Distribution of visits.
    visit_schedule <- list(
        mean = 12, sd = 3, skip = 0
    )

    
}

######################################################################
### LMDB_setting.R ends here

### rtmle_example_data.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: mar 20 2026 (06:32) 
## Version: 
## Last-Updated: apr  1 2026 (09:36) 
##           By: Thomas Alexander Gerds
##     Update #: 6
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
tar_load_globals()
p <- get_rtmle_example_setting()
initialize_treatment <- function(X){
    X[,A:=1*(randomized_treatment == 1)]
    X[,B:=1*(randomized_treatment == 0)]
    X[,randomized_treatment := NULL]
    X[]
}
d <- do.call(simulate_cohort,
             c(list(n = 10,post_baseline_visit_hook = initialize_treatment),p))
bsl <- d[time == 0,c("id","sex","age","BMI","SBP","Diabetes","A","B")]


######################################################################
### rtmle_example_data.R ends here

### rtmle_example_data.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: mar 20 2026 (06:32) 
## Version: 
## Last-Updated: apr  2 2026 (07:34) 
##           By: Thomas Alexander Gerds
##     Update #: 7
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
long_to_list(d,bsl_vars = c("sex","age","BMI"),
             treatment_vars = c("A","B"),
             measurement_vars = c("SBP"),
             outcome = "Diabetes")

bsl <- d[time == 0,c("id","sex","age","BMI","SBP","Diabetes","A","B")]


######################################################################
### rtmle_example_data.R ends here

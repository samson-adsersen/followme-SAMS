### rtmle_example_data.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: mar 20 2026 (06:32) 
## Version: 
## Last-Updated: apr 29 2026 (09:35) 
##           By: Thomas Alexander Gerds
##     Update #: 19
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
p$parameter_values <- modifyList(p$parameter_values,
                                 list(effect_auto_A_A = 5,effect_auto_B_B = 2.5,scale_dropout = 0,scale_diabetes = 0.01))
d <- do.call(simulate_cohort,c(list(n = 10000,post_baseline_visit_hook= initialize_treatment),p))
rd <- register_format(d,bsl_vars = c("sex","age","BMI"),
                      treatment_vars = c("A","B"),
                      measurement_vars = c("SBP"),
                      outcome = "Diabetes")
x <- rtmle_init(time_grid = seq(0,5,.5),name_id = "id",name_outcome = "diabetes",name_competing = "death",name_censoring = "cens")
x <- add_baseline_data(x,rd$baseline_data)
x <- do.call(add_long_data,c(list(x),rd[-1]))
x <- protocol(x,
              name = "use_A",
              intervention = data.table(time = x$intervention_nodes,
                                        "A" = factor(1,0:1)))
x <- protocol(x,
              name = "use_A_not_B",
              intervention = data.table(time = x$intervention_nodes,
                                        "A" = factor(1,0:1),
                                        "B" = factor(0,0:1)))
x <- protocol(x,
              name = "use_B",
              intervention = data.table(time = x$intervention_nodes,
                                        "B" = factor(1,0:1)))
x <- target(x,"diabetes_risk",protocols = c("use_A","use_B","use_A_not_B"))
x <- long_to_wide(x)
x <- prepare_rtmle_data(x)
x <- model_formula(x,exclusion_rules = list("A" = "B_0","B" = "A_0"))
x <- run_rtmle(x,time_horizon = 1:5,learner = "learn_glmnet")
summary(x)
plot_adherence(x)


######################################################################
### rtmle_example_data.R ends here

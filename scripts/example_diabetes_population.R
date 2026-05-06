library(targets)
tar_load_globals()
tar_load_everything()




X <- interventional_risks_diabetes_population
X[time_horizon == 6]

#r <- summary(rtmle_diabetes_population)[Target_parameter == "Risk",c("Protocol","Time_horizon","Estimate (CI_95)","Reference")]
#r[Time_horizon == 6]







### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: okt 23 2025 (15:22) 
## Version: 
## Last-Updated: mar 20 2026 (15:22) 
##           By: Thomas Alexander Gerds
##     Update #: 144
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
### _targets.R --- 
library(targets)
tar_source("functions")
tar_option_set(packages = c("lava","survival","data.table","prodlim","rtmle","foreach","ggplot2","plotly", "contICEIPCW"))
## Install:
## devtools::install_github("jsohlendorff/contICEIPCW")

list(
    tar_target(name = diabetes_polypharmacy_setting,{
        command ={
            dps <- get_diabetes_polypharmacy_setting()
            dps$parameter_values <- modifyList(dps$parameter_values,
                                               list(effect_GLP1_MACE = -1,
                                                    effect_SGLT2_MACE = -2,
                                                    scale_MACE = 0.002,
                                                    scale_death = 0.001))
            dps
        }
    }),
    tar_target(name = diabetes_population,{ 
        command = {
            cohort <- do.call("simulate_cohort",
                              c(list(n = 3000),
                                diabetes_polypharmacy_setting))
        }
    },cue = tar_cue(mode = "thorough")),
    tar_target(
        name = interventional_risks_diabetes_population,
        calculate_interventional_risks(n = 1000000,
                                       diabetes_polypharmacy_setting = diabetes_polypharmacy_setting,
                                       intervention = list("GLP1" = 1, "SGLT2" = 1, "DPP4" = 1),
                                       time_horizons = seq(0,60,6)[-1],
                                       primary_event = "MACE")
    ),
    tar_target(name = aalen_johansen_diabetes_population,
               command = {
                   run_aalen_johansen_diabetes_population(diabetes_population = diabetes_population,time_horizon = 30)
               }),

    tar_target(name = rtmle_diabetes_population,
               command = {
                   run_rtmle_diabetes_population(diabetes_population = diabetes_population,
                                                 time_horizons = 30,
                                                 intervals = seq(0,60,6),
                                                 learner = "learn_glmnet")
               }),
    tar_target(name = ice_ipcw_diabetes_population,
               command = {
                   run_ice_ipcw(data = diabetes_population,
                                time_horizon = 30,
                                regimens = c("GLP1", "SGLT2", "DPP4"),
                                model_pseudo_outcome = "oipcw_expit", ## specify learner for iterative regression; example learner from package: learn_glm_logistic
                                penalize_pseudo_outcome = FALSE, ## uses cross-validation if TRUE
                                primary_event = "MACE",
                                verbose = FALSE)
               }),
    
    tar_target(diabetes_sim_data, {
        make_and_write_diabetes_data(file_name = "data/diabetes_sim_data.csv", diabetes_polypharmacy_setting = diabetes_polypharmacy_setting)
    }, format = "file")
)



######################################################################
### _targets.R ends her

### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: okt 23 2025 (15:22) 
## Version: 
## Last-Updated: Mar 19 2026 (10:33) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 137
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
library(lava)
library(data.table)
tar_source("functions")
tar_option_set(packages = c("lava","survival","data.table","prodlim","rtmle","foreach","ggplot2","plotly", "contICEIPCW"))
## Install:
## devtools::install_github("jsohlendorff/contICEIPCW")

list(
    tar_target(name = diabetes_polypharmacy_setting,{
        command =
            get_diabetes_polypharmacy_setting()
    }),
    tar_target(name = dps,
               command = {
                   dps <- diabetes_polypharmacy_setting
                   dps$parameter_values <- modifyList(dps$parameter_values,
                                                      list(effect_GLP1_MACE = -1,
                                                           effect_SGLT2_MACE = -2,
                                                           scale_MACE = 0.002,
                                                           scale_death = 0.001))
                   dps}),
    tar_target(name = diabetes_population,{ 
        command = {
            simulate_diabetes_population(diabetes_polypharmacy_setting = dps,
                                         initial_treatment = list(GLP1 = 8010,SGLT2 = 5770,DPP4 = 13040))
        }
    },cue = tar_cue(mode = "thorough")),
    tar_target(
        name = true_values_MACE,
        diabetes_true_values(n = 1000000,
                             diabetes_polypharmacy_setting = dps,
                             treatments = c("GLP1", "SGLT2", "DPP4"),
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
                                                 intervals = seq(0,60,6))
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
    
    tar_target(diabetes_data, {
        make_and_write_diabetes_data(file_name = "data/diabetes_population.csv", diabetes_polypharmacy_setting = diabetes_polypharmacy_setting)
    }, format = "file")
)



######################################################################
### _targets.R ends her

### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: okt 23 2025 (15:22) 
## Version: 
## Last-Updated: mar 18 2026 (12:07) 
##           By: Thomas Alexander Gerds
##     Update #: 74
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
    ## Example for generating data from interventional distribution:
    ## Treatments: GLP1, SGLT2, DPP4
    ## All start on GLP1 at baseline.
    ## Afterwards, the treatment regimen states that patients
    ## stay on GLP1, but may not take SGLT2 at any time;
    ## Values of DPP4 are not intervened upon.
    tar_target(name = diabetes_polypharmacy_setting_interventional_data,{
        command = {
            diabetes_polypharmacy_setting_intervene <- diabetes_polypharmacy_setting
            diabetes_polypharmacy_setting_intervene$intervene <- TRUE
            diabetes_polypharmacy_setting_intervene$intervene_variables <- c("GLP1", "SGLT2") 
            diabetes_polypharmacy_setting_intervene$intervene_values <- c(1, 0)
            simulate_diabetes_population(diabetes_polypharmacy_setting = diabetes_polypharmacy_setting_intervene,
                                         initial_treatment = list(GLP1 = 2801,SGLT2 = 1577,DPP4 = 3304))
        }
            
    }),
    tar_target(name = diabetes_population,{ 
        command ={
            dps <- diabetes_polypharmacy_setting
            dps$parameter_values <- modifyList(dps$parameter_values,
                                               list(effect_GLP1_MACE = -1,
                                                    effect_SGLT2_MACE = -2,
                                                    scale_MACE = 0.002,
                                                    scale_death = 0.001))
            simulate_diabetes_population(diabetes_polypharmacy_setting = dps,
                                         initial_treatment = list(GLP1 = 801,SGLT2 = 577,DPP4 = 1304))
        }
    },cue = tar_cue(mode = "thorough")),
    tar_target(name = aalen_johansen_diabetes_population,
               command = {
                   run_aalen_johansen_diabetes_population(diabetes_population = diabetes_population,time_horizon = 30)
               }),

    tar_target(name = rtmle_diabetes_population,
               command = {
                   run_rtmle_diabetes_population(diabetes_population = diabetes_population,time_horizon = 5)
               }),
    tar_target(name = ice_ipcw_diabetes_population,
               command = {
                   run_ice_ipcw(data = diabetes_population,
                                time_horizon = 30,
                                regimens = c("GLP1", "SGLT2", "DPP4"))
               })
)



######################################################################
### _targets.R ends her

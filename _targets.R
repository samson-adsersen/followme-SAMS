### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: okt 23 2025 (15:22) 
## Version: 
## Last-Updated: mar 16 2026 (08:06) 
##           By: Thomas Alexander Gerds
##     Update #: 24
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
tar_option_set(packages = c("lava","survival","data.table","prodlim","rtmle","foreach","ggplot2","plotly"))
list(
    tar_target(name = diabetes_polypharmacy_setting,{
        command =
            get_diabetes_polypharmacy_setting()
    }),
    tar_target(name = diabetes_population,{ 
        command ={
            simulate_diabetes_population(diabetes_polypharmacy_setting = diabetes_polypharmacy_setting,
                                         initial_treatment = list(GLP1 = 2801,SGLT2 = 1577,DPP4 = 3304))
        }
    },cue = tar_cue(mode = "thorough")),
    tar_target(name = rtmle_diabetes_population,
               command = {
                   run_rtmle_diabetes_population(diabetes_population = diabetes_population)
               })
)



######################################################################
### _targets.R ends here

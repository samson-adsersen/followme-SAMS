### ice_ipcw.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 16 2026 (11:52) 
## Version: 
## Last-Updated: mar 18 2026 (12:07) 
##           By: Thomas Alexander Gerds
##     Update #: 98
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

run_ice_ipcw <- function(data, time_horizon, regimens = c("GLP1", "SGLT2", "DPP4")){
    ## require(contICEIPCW) ##devtools::install_github("jsohlendorff/contICEIPCW")
    model_hazard <- "learn_coxph"
    setkeyv(data, c("id", "time"))
    baseline_data <- data[time == 0, c("id", "sex", "age", "HbA1c", "U", regimens), with = FALSE]
    setnames(baseline_data, regimens, paste0(regimens, "_0"))
    timevar_data <- data[time > 0, c("id", "time", "event", "changeHbA1c", regimens), with = FALSE]
    ## Change labels visit, MACE, death, dropout to A, Y, D, C
    timevar_data[event == "visit", event := "A"]
    timevar_data[event == "MACE", event := "Y"]
    timevar_data[event == "death", event := "D"]
    timevar_data[event == "dropout", event := "C"]
    ## Remove events after event==Y;
    ## Only first MACE event matters for the analysis.
    ## MACE cannot occur after first event if it was not already mace?
    terminal_time <- timevar_data[event%in% c("C", "D", "Y")][, .(terminal_time = min(time)), by = "id"]
    timevar_data <- merge(timevar_data, terminal_time, by = "id", all.x = TRUE)
    timevar_data <- timevar_data[time <= terminal_time]
    timevar_data[, terminal_time:= NULL]
    timevar_data[, event := as.factor(event)]

    res <- list()
    ## Run ICE-IPCW estimator across all regimens
    for (regimen in regimens){
        other_regimens <- setdiff(regimens, regimen)
        other_regimens_baseline <- paste0(other_regimens, "_0")
        data_regimen <- copy(timevar_data)[, !(other_regimens), with = FALSE]
        baseline_regimen <- copy(baseline_data)[, !(other_regimens_baseline), with = FALSE]
        setnames(data_regimen, regimen, "A")
        setnames(baseline_regimen, paste0(regimen, "_0"), "A_0")
        prep_data <- contICEIPCW::prepare_data(
            data = list(baseline_data = baseline_regimen,
                        timevarying_data = data_regimen),
            max_time_horizon = time_horizon,
            time_covariates = c("changeHbA1c", "A"),
            baseline_covariates =  c("age", "A_0", "sex", "HbA1c", "U"),
            marginal_censoring = TRUE
        )
        prop_scores <- contICEIPCW::propensity_scores(
            prepared_data = prep_data,
            model_treatment = "learn_glm_logistic",
            penalize_treatment = TRUE,
            model_hazard = "learn_coxph"
        )
        est <- contICEIPCW::debias_ice_ipcw(
            prepared_data = prop_scores,
            time_horizon = time_horizon,
            model_pseudo_outcome = "lm",
            penalize_pseudo_outcome = TRUE,
            model_hazard = NULL,
            penalize_hazard = FALSE,
            conservative = TRUE,
            static_intervention = 1,
            semi_tmle = TRUE,
            verbose = FALSE
        )
        est$intervention <- paste0("stay_on_", regimen)
        res[[regimen]] <- est
    }
    res
}

######################################################################
### ice_ipcw.R ends here

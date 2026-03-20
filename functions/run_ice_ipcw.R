### ice_ipcw.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 16 2026 (11:52) 
## Version: 
## Last-Updated: Mar 20 2026 (16:54) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 176
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

run_ice_ipcw <- function(data,
                         time_horizon,
                         primary_event = "MACE",
                         regimens = c("GLP1", "SGLT2", "DPP4"),
                         contrasts = TRUE,
                         contrasts_reference = "SGLT2",
                         verbose = FALSE, ...){  ## arguments to be passed to debias_ice_ipcw
    ## Check if contICEIPCW is installed, if not install it from GitHub
    if (!requireNamespace("contICEIPCW", quietly = TRUE)) {
        ## Check that version of "contICEIPCW" is new enough
        if (packageVersion("contICEIPCW") <= "0.0.9000") {
            requireNamespace("devtools", quietly = TRUE)
            message("Installing contICEIPCW from GitHub...")
            devtools::install_github("jsohlendorff/contICEIPCW")
        }
    }
    ## require(contICEIPCW) ##devtools::install_github("jsohlendorff/contICEIPCW")
    setkeyv(data, c("id", "time"))
    baseline_data <- data[time == 0, c("id", "sex", "age", "HbA1c", "U", regimens), with = FALSE]
    setnames(baseline_data, regimens, paste0(regimens, "_0"))
    timevar_data <- data[time > 0, c("id", "time", "event", "changeHbA1c", regimens), with = FALSE] #paste0("History_", regimens)
    ## Change labels visit, MACE, death, dropout to A, Y, D, C
    timevar_data[event == "visit", event := "A"]
    if (primary_event == "MACE") {
        timevar_data[event == "MACE", event := "Y"]
        timevar_data[event == "death", event := "D"]
    } else if (primary_event == "death"){
        timevar_data[event == "MACE", event := "D"]
        timevar_data[event == "death", event := "Y"]
    } else {
        stop("Unknown primary event")
    }
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
        data_regimen <- copy(timevar_data)
        baseline_regimen <- copy(baseline_data)
        setnames(data_regimen, regimen, "A")
        setnames(baseline_regimen, paste0(regimen, "_0"), "A_0")
        prep_data <- contICEIPCW::prepare_data(
            data = list(baseline_data = baseline_regimen,
                        timevarying_data = data_regimen),
            max_time_horizon = time_horizon,
            time_covariates = c("changeHbA1c", "A", other_regimens), # c(paste0("History_", regimens)
            baseline_covariates =  c("age", "A_0", "sex", "HbA1c", "U"),
            marginal_censoring = TRUE,
            verbose = verbose
        )
        prop_scores <- contICEIPCW::propensity_scores(
            prepared_data = prep_data,
            model_treatment = "learn_glm_logistic",
            penalize_treatment = TRUE,
            model_hazard = "learn_coxph",
            verbose = verbose,
            exclude_latest_covariate = other_regimens ## Time-ordering of these variable and the treatment is unclear, so remove the latest values c(paste0("History_", regimens)
        )
        est <- contICEIPCW::debias_ice_ipcw(
            prepared_data = prop_scores,
            time_horizon = time_horizon,
            static_intervention = 1,
            tmle_update = TRUE,
            return_ic = TRUE, # For effects
            verbose = verbose,
            conservative = TRUE,
            ...
        )
        result_dt <- as.data.table(est$result)
        result_dt[, c("time_horizon", "treatment", "treatment_reference", "ice_ipcw_estimate", "target_parameter", "p_value") := list(time_horizon, regimen, NA, NULL, "risk", NA)]
        est$result <- result_dt
        res[[regimen]] <- est
    }
    results <- rbindlist(lapply(res, function(x) x$result))
    
    if (contrasts){
        res_contrasts <- list()
        for (regimen in regimens){
            if (regimen == contrasts_reference) next
            estimate <- res[[regimen]]$result$estimate - res[[contrasts_reference]]$result$estimate
            se <- sd(res[[regimen]]$ic - res[[contrasts_reference]]$ic) / sqrt(length(res[[regimen]]$ic))
            res_contrasts[[regimen]] <- data.frame(
                estimate = estimate,
                se = se,
                lower = estimate - 1.96 * se,
                upper = estimate + 1.96 * se,
                ipw = res[[regimen]]$result$ipw - res[[contrasts_reference]]$result$ipw,
                time_horizon = time_horizon,
                treatment = regimen,
                treatment_reference = contrasts_reference,
                target_parameter = "risk_difference",
                p_value = 2 * (1 - pnorm(abs(estimate / se)))
            )
        }
        res_contrasts <- rbindlist(res_contrasts)
    } else {
        res_contrasts <- NULL
    }
    results <- rbind(results, res_contrasts)
    return(results)
}

######################################################################
### ice_ipcw.R ends here

### diabetes_true_values.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 18 2026 (16:33) 
## Version: 
## Last-Updated: Mar 18 2026 (16:34) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 2
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
diabetes_true_values <- function(n, diabetes_polypharmacy_setting, treatments, time_horizons, primary_event) {
    terminal_events <- c("death", "MACE", "dropout")
    out <- list()
    for (treatment in treatments) {
        data_interventional <- simulate_diabetes_population(
            diabetes_polypharmacy_setting = diabetes_polypharmacy_setting,
            initial_treatment = list(GLP1 = ifelse(treatment == "GLP1", n, 0),
                                     SGLT2 = ifelse(treatment == "SGLT2", n, 0),
                                     DPP4 = ifelse(treatment == "DPP4", n, 0)),
            stay_on = treatment
        )
        data_interventional[, stay_on := treatment]
        out[[treatment]] <- data_interventional
    }
    d <- rbindlist(out)
    setkeyv(d, c("stay_on", "id", "time"))
    data_terminal_events <- d[event %in% terminal_events, .(time = time[1], event = event[1]), by = c("id", "stay_on")]
    out <- list()
    for (time_horizon in time_horizons) {
        if (primary_event == "MACE") {
            true_values <- data_terminal_events[, .(risk = mean(time <= time_horizon & event == "MACE")), by = "stay_on"]
        } else if (primary_event == "death") {
            true_values <- data_terminal_events[, .(risk = mean(time <= time_horizon & event == "death")), by = "stay_on"]
        } else {
            stop("Unknown primary event")
        }
        true_values[, time_horizon := time_horizon]
        out[[as.character(time_horizon)]] <- true_values
    }
    
    return(rbindlist(out))
}

######################################################################
### diabetes_true_values.R ends here

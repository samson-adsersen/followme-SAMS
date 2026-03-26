### plot_estimate.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 26 2026 (11:16) 
## Version: 
## Last-Updated: Mar 26 2026 (11:35) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 30
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
## plot_estimate <- function(estimates_rtmle,
##                           estimates_ice_ipcw,
##                           intervals = seq(0,60,6),
##                           true_values) {
##     estimates_rtmle <- estimates_rtmle$estimate$Main_analysis[, c("Protocol", "Time_horizon", "Estimate", "Lower", "Upper")]
##     intervals <- intervals[-1]
##     estimates_rtmle[, Time_horizon := intervals[Time_horizon]]
##     estimates_rtmle[, Type := "RTMLE"]
##     estimates_ice_ipcw <- estimates_ice_ipcw$results[, c("estimate", "lower", "upper", "time_horizon", "treatment_name")]
##     setnames(estimates_ice_ipcw, c("Estimate", "Lower", "Upper", "Time_horizon", "Protocol"))
##     estimates_ice_ipcw[, Protocol := paste0("Always_", Protocol)]
##     estimates_ice_ipcw[, Type := "ICE-IPCW"]
##     true_values[, intervention := paste0("Always_", intervention)]
##     true_values[, c("Lower", "Upper") := list(NA, NA)]
##     setnames(true_values, c("intervention", "time_horizon", "risk"), c("Protocol", "Time_horizon", "Estimate"))
##     true_values[, Type := "True value"]
##     ## bind them
##     plot_data <- rbind(estimates_rtmle, estimates_ice_ipcw, true_values, fill = TRUE)
##     ggplot(
##         plot_data,
##         aes(x = Time_horizon, y = Estimate, color = Type)
##     )  + 
##     geom_line() +
##     facet_grid(~Protocol)  +
##     geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
##     theme_bw()
## }

plot_estimate <- function(estimates_rtmle,
                          estimates_ice_ipcw,
                          intervals = seq(0, 60, 6),
                          true_values) {

  # --- RTMLE ---
  rtmle_dt <- estimates_rtmle$estimate$Main_analysis[
    , .(Protocol, Time_horizon, Estimate, Lower, Upper)
  ]

  # Map time indices to actual intervals (skip 0)
  time_map <- intervals[-1]
  rtmle_dt[, Time_horizon := time_map[Time_horizon]]
  rtmle_dt[, Type := "RTMLE"]

  # --- ICE-IPCW ---
  ice_dt <- estimates_ice_ipcw$results[
    , .(Estimate = estimate,
        Lower = lower,
        Upper = upper,
        Time_horizon = time_horizon,
        Protocol = treatment_name)
  ]

  ice_dt[, Protocol := paste0("Always_", Protocol)]
  ice_dt[, Type := "ICE-IPCW"]

  # --- True values ---
  true_dt <- copy(true_values)

  true_dt[, Protocol := paste0("Always_", intervention)]
  true_dt[, `:=`(
    Lower = NA_real_,
    Upper = NA_real_,
    Type  = "True value"
  )]

  true_dt <- true_dt[
    , .(Protocol,
        Time_horizon = time_horizon,
        Estimate = risk,
        Lower,
        Upper,
        Type)
  ]

  # --- Combine ---
  plot_data <- rbindlist(list(rtmle_dt, ice_dt, true_dt), fill = TRUE)

  # --- Plot ---
  ggplot(plot_data, aes(x = Time_horizon, y = Estimate, color = Type)) +
    geom_line() +
    geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Type), alpha = 0.2, color = NA) +
    facet_grid(~Protocol) +
    theme_bw()
}
######################################################################
### plot_estimate.R ends here

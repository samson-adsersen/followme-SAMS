### _targets.R --- 
#----------------------------------------------------------------------
## Author: Sams
## Created: okt  8 2026 (13:03) 
## Version: 
## Last-Updated: okt  8 2026 (15:28) 
##           By: sads0006
##     Update #: 10
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

library(targets)
library(tarchetypes)

targets::tar_source(
    c("c:/Users/sads0006/Desktop/rtmle/R/",
      "c:/Users/sads0006/Desktop/followme-SAMS/playground/minimal_simulation.R",
      "c:/Users/sads0006/Desktop/followme-SAMS/functions/initialize_parameter_values.R"
      ))

targets::tar_option_set(packages = c("data.table"))

list(
    tar_target(
        name = simplest_setting,
        command = {
            p <- get_minimal_setting()
            p$parameter_values <- modifyList(
                p$parameter_values,
                list(
                    scale_death = 0.1,
                    scale_dropout = 0,
                    effect_lira_dropout = 0,
                    effect_lira_death = -1
                ))
        }
    ),

    tar_target(
        name = cohort1,
        command = {
            d <- do.call(
                simulate_cohort,
                c(
                    list(
                        n = 100000,
                        post_baseline_visit_hook = randomize_baseline_treatment
                    ),
                    simplest_setting
                ))
        })#,

    ## tar_target(
    ##     name = naive_est1,
    ##     command = {
    ##         # crude risk estimation
    ##         data <- cohort1[, .(time =  max(time),
    ##                       death = as.integer(any(event == "death")),
    ##                       treatment = fifelse(first(lira) == 1, "lira", "placebo")
    ##                       ),
    ##                   by = id]
    ##         drate_lira <- data[treatment == "lira" & death == 1 & time <= 1.5, .N]/data[treatment == "lira",.N]
    ##         drate_placebo <- data[treatment == "placebo" & death == 1 & time <= 1.5, .N]/data[treatment == "placebo",.N]
    ##         drate_all <- data[death == 1 & time <= 1.5,.N]/data[,.N]
    ##         diff <- drate_placebo - drate_lira
    ##     }),

    ## tar_target(
    ##     name = kaplan_meier1,
    ##     command = {
    ##         # Kaplan-Meier risk estimate
    ##         data <- cohort1[, .(time =  max(time),
    ##                       death = as.integer(any(event == "death")),
    ##                       treatment = fifelse(first(lira) == 1, "lira", "placebo")
    ##                       ),
    ##                   by = id]
    ##         grid <- seq(0, 1.5, .5)
    ##         fit <- prodlim::prodlim(
    ##                             prodlim::Hist(time,death) ~ treatment,
    ##                             data = data
    ##                         )
    ##         risk <- predict(
    ##             fit,
    ##             newdata = data.frame(treatment =  c("lira","placebo")),
    ##             times = grid,
    ##             type = "risk")
    ##     }),

    ## tar_target(
    ##     name = rtmle1,
    ##     command = {
    ##         # rtmle estimates
    ##         rd <- register_format(cohort1)
    ##         x <- rtmle_init(
    ##             time_grid = seq(0, 1.5, .5),
    ##             name_id = "id",
    ##             name_outcome = "death",
    ##             name_competing =  NULL,
    ##             name_censoring = "dropout"
    ##         )
    ##         x <- add_baseline_data(x, rd$baseline_data)
    ##         x <- add_long_data(x,outcome_data = rd$timevar_data$death,
    ##                            competing_data = NULL,
    ##                            censored_data = rd$timevar_data$dropout,
    ##                            timevar_data = rd$timevar_data[c("hba1c_change","lira","nausea.and.vomiting.symptoms","placebo")])
    ##         x <- discretize_data(x,
    ##                              start_followup_date = 0)
    ##         # Setup protocols
    ##         x <- protocol(x,
    ##                       name = "lira",
    ##                       intervention = data.table(time = x$intervention_nodes,
    ##                                                 "lira" = factor(rep(1,3),levels = 0:1)))
    ##         x <- protocol(x,
    ##                       name = "placebo",
    ##                       intervention = data.table(time = x$intervention_nodes,
    ##                                                 "lira" = factor(rep(0,3),levels = 0:1)))
    ##         x <- prepare_rtmle_data(x)
    ##         x <- model_formula(x, exclusion_rules = list("placebo" = "lira_0", "lira" = "placebo_0"))
    ##         x <- target(x,name = "Treatment",regimes = c("placebo","lira"))
    ##         x <- run_rtmle(x, time_horizon = 3, learner = "learn_glmnet")
    ##     })

)









######################################################################
### _targets.R ends here

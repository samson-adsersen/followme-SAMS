### _targets.R --- 
#----------------------------------------------------------------------
## Author: Sams
## Created: okt  8 2026 (13:03) 
## Version: 
## Last-Updated: Oct 25 2026 (13:50) 
##           By: Samson Alfred Adsersen
##     Update #: 58
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

## targets::tar_source(
##     c("c:/Users/sads0006/Desktop/rtmle/R/",
##       "c:/Users/sads0006/Desktop/followme-SAMS/playground/minimal_simulation.R",
##       "c:/Users/sads0006/Desktop/followme-SAMS/functions/initialize_parameter_values.R"
##       ))

targets::tar_source(
             c("~/Desktop/Steno/rtmle/R/",
               "~/Desktop/Steno/followme-SAMS/playground/minimal_simulation.R",
               "~/Desktop/Steno/followme-SAMS/functions/initialize_parameter_values.R"))



#-----------------------------------------------------------------------------
#-----------------------------Helper-functions--------------------------------
#-----------------------------------------------------------------------------

simple_run <- function(setting, n, selector, alpha){
    browser()
    learner <- list(
        name = paste("glmnet", selector, alpha, sep = "_"),
        fun = "learn_glmnet",
        alpha = alpha,
        selector = selector)
    
    d <- do.call(
        simulate_cohort,
        c(
            list(
                n = n,
                post_baseline_visit_hook = randomize_baseline_treatment
            ),
            setting
        )
    )

    naive_risk <- naive_risk_est(d, 1.5, c("lira", "placebo"), "death")
    KM_risk <- KM_risk_est(d, 1.5, 0.5, c("lira", "placebo"), "death")
    rtmle_risk <- rtmle_risk_est(d, 1.5, 0.5, c("lira", "placebo"), "death", "dropout", learner)

    out <- data.table(
        naive = naive_risk$diff,
        KM = KM_risk$diff,
        rTMLE = rtmle_risk$diff)
    
    out[]
}

simple_run <- function(setting, n, selector, alpha){
    learner <- list(
        name = paste("glmnet", selector, alpha, sep = "_"),
        fun = "learn_glmnet",
        alpha = alpha,
        selector = selector)
    
    d <- do.call(
        simulate_cohort,
        c(
            list(
                n = n,
                post_baseline_visit_hook = randomize_baseline_treatment
            ),
            setting
        )
    )

    naive_risk <- naive_risk_est(d, 1.5, c("lira", "placebo"), "death")
    KM_risk <- KM_risk_est(d, 1.5, 0.5, c("lira", "placebo"), "death")
    rtmle_risk <- rtmle_risk_est(d, 1.5, 0.5, c("lira", "placebo"), "death", "dropout", learner)

    out <- data.table(
        naive = naive_risk$diff,
        KM = KM_risk$diff,
        rTMLE = rtmle_risk$diff)
    
    out[]
}


#-----------------------------------------------------------------------------
#-----------------------------Main-Analysis-----------------------------------
#-----------------------------------------------------------------------------


targets::tar_option_set(packages = c("data.table"))

list(

    #-------------------------------------------------------------------------
    #-------------------------Minimal-simulation-setup------------------------
    #-------------------------------------------------------------------------

    tar_target(
        name = simple_setting,
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
            p[]
        }
    ),

    tarchetypes::tar_map_rep(
                     name = simple_sim_results,
                     command = {
                         simple_run(simple_setting, n, selector, alpha)
                     },
                     values = tidyr::expand_grid(
                                          n = c(1000, 2000),
                                          selector = c("min",
                                                       "1se",
                                                       "undersmooth"),
                                          alpha = c(0, 0.5, 1)),
                     reps = 2
                 ),

    tar_target(
        name = simple_sim_boxplot,
        command = {
            data <- data.table::as.data.table(simple_sim_results)
            ggplot2::ggplot(
                         data,
                         ggplot2::aes(
                                      x = factor(n),
                                      y = (rTMLE - naive)/(naive),
                                      fill = paste("glmnet",
                                                   selector,
                                                   alpha,
                                                   sep = "_"))) +
                ggplot2::geom_boxplot(
                             position = ggplot2::position_dodge(width = 0.8)) +
                ggplot2::geom_hline(
                             yintercept = 0,
                             linetype = "dashed") +
                ggplot2::labs(
                             x = "Sample size",
                             y = "Estimated difference in risk of death \nfor treatment protocols LIRA and PLACEBO",
                             fill = "Learner",
                             title = "Distribution of normalized difference in estimates across simulation runs") +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                             plot.title = ggplot2::element_text(hjust = 0.5)) +
                ggplot2::scale_y_continuous(
                         limits = c(-0.1, 0.1),
                         breaks = seq(-0.1, 0.1, by = 0.01)
                     )
        }
    )

    # Potentially could look at RMSE for single models run on multiple different seeds
    # for increasing population size (nice plot, but maybe a bit too obvious that the error
    # goes down for increasing sample size - however nice to compare models).

    #-------------------------------------------------------------------------
    #-------------------------Adding-censoring--------------------------------
    #-------------------------------------------------------------------------







    #-------------------------------------------------------------------------
    #-------------------------Adding-cofounding-variable(s)-------------------
    #-------------------------------------------------------------------------
    

)









######################################################################
### _targets.R ends here

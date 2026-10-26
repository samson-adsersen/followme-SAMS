### _targets.R --- 
#----------------------------------------------------------------------
## Author: Sams
## Created: okt  8 2026 (13:03) 
## Version: 
## Last-Updated: Oct 26 2026 (18:29) 
##           By: Samson Alfred Adsersen
##     Update #: 107
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

plot_rmse <- function(data, x, y, names, title) { 
ggplot2::ggplot(
                         data,ggplot2::aes(
                                                   x = .data[[x]],
                                                   y = .data[[y]],
                                                   color = interaction(selector, alpha)
                                               )
                     ) +
                ggplot2::geom_line() +
                ggplot2::geom_point() +
                ggplot2::geom_hline(
                             yintercept = 0,
                             linetype = "dashed"
                         ) +
                ggplot2::labs(
                             x = names[1],
                             y = names[2],
                             color = "Learner",
                             title = title
                         ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                             plot.title = ggplot2::element_text(hjust = 0.5)
                         )       
}

plot_boxplot_comparison <- function(data, x, y, names, title, limits, sep) {
            ggplot2::ggplot(
                         data,
                         ggplot2::aes(
                                      x = factor(.data[[x]]),
                                      y = .data[[y]],
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
                             x = names[1],
                             y = names[2],
                             fill = "Learner",
                             title = title) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                             plot.title = ggplot2::element_text(hjust = 0.5)) +
                ggplot2::scale_y_continuous(
                         limits = limits,
                         breaks = seq(limits[1], limits[2], by = sep)
                     )
}


#-----------------------------------------------------------------------------
#-----------------------------Main-Analysis-----------------------------------
#-----------------------------------------------------------------------------


targets::tar_option_set(packages = c("data.table"))

list(

    #-------------------------------------------------------------------------
    #-------------------------Minimal-simulation-setup------------------------
    #-------------------------------------------------------------------------

    # Defining the setting / parameter distributions
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

    # Reference risk estimate (large cohort)
    tar_target(
        name = simple_risk_reference,
        command = {
            d <- do.call(
                simulate_cohort,
                c(
                    list(
                        n = 1000000,
                        post_baseline_visit_hook = randomize_baseline_treatment
                    ),
                    simple_setting
                ))
            est <- naive_risk_est(d, 1.5, c("lira", "placebo"), "death")
            est$diff
        }),

    # Simple sim looking at bias vs sample size for different
    tarchetypes::tar_map_rep(
                     name = simple_bias_results,
                     command = {
                         results <- simple_run(
                             simple_setting, n, selector, alpha
                         )
                         reference <- as.numeric(simple_risk_reference)
                         data.frame(
                             normsq_diff = ((results$rTMLE - reference)/reference)^2
                         )
                     },
                     values = tidyr::expand_grid(
                                         n = c(500, 1000, 2000, 5000, 10000, 100000),
                                         selector = c("min", "1se", "undersmooth"),
                                         alpha = c(0, 0.5, 1)
                                     ),
                     reps = 10
                 ),

    # Graph for bias vs samplesize
    tar_target(
        name = simple_bias_plot,
        command = {
            data <- data.table::as.data.table(simple_bias_results)
            summary_data <- data[,
                                 .(rmse = sqrt(mean(normsq_diff, na.rm = TRUE))
                                   ),
                                 by = .(n, selector, alpha)]
            plot_rmse(summary_data, "n", "rmse", c("Sample size", "RMSE"),
                      "RMSE for increasing sample sizes")
        }
    ),

    # Simple simulation comparing model bias with sample sizes
    tarchetypes::tar_map_rep(
                     name = simple_sim_results,
                     command = {
                         simple_run(simple_setting, n, selector, alpha)
                     },
                     values = tidyr::expand_grid(
                                          n = c(10000, 100000),
                                          selector = c("min",
                                                       "1se",
                                                       "undersmooth"),
                                          alpha = c(0, 0.5, 1)),
                     reps = 2
                 ),

    # Boxplot comparing bias for different model/samplesize combinations
    tar_target(
        name = simple_sim_boxplot,
        command = {
            data <- data.table::as.data.table(simple_sim_results)
            reference <- as.numeric(simple_risk_reference)
            data[, norm_diff := (rTMLE - reference)/reference]
            plot_boxplot_comparison(data, "n", "norm_diff", c("Sample sizes", "Normalized difference wrt. reference"),
                                    "Display of bias by different learner specs and sample sizes", c(-1,1), 0.1)
    }),

    
    #-------------------------------------------------------------------------
    #-------------------------Adding-censoring--------------------------------
    #-------------------------------------------------------------------------

    # Same as minimal setting but with scale of dropout non-zero
    tar_target(
        name = setting_with_censoring,
        command = {
            p <- get_minimal_setting()
            p$parameter_values <- modifyList(
                p$parameter_values,
                list(
                    scale_death = 0.1,
                    scale_dropout = 0.1,
                    effect_lira_dropout = 0,
                    effect_lira_death = -1
                ))
            p[]
        }
    ),


    # Reference risk estimate (large cohort)
    tar_target(
        name = with_censoring_risk_reference,
        command = {
            d <- do.call(
                simulate_cohort,
                c(
                    list(
                        n = 1000000,
                        post_baseline_visit_hook = randomize_baseline_treatment
                    ),
                    simple_setting
                ))
            est <- naive_risk_est(d, 1.5, c("lira", "placebo"), "death")
            est$diff
        }),

    # Boxplots
    tarchetypes::tar_map_rep(
                     name = with_censoring_sim_results,
                     command = {
                         simple_run(setting_with_censoring, n, selector, alpha)
                     },
                     values = tidyr::expand_grid(
                                          n = c(1000, 2000),
                                          selector = c("min",
                                                       "1se",
                                                       "undersmooth"),
                                          alpha = c(0, 0.5, 1)),
                     reps = 2
                 )




    #-------------------------------------------------------------------------
    #-------------------------Adding-confounding-variable(s)------------------
    #-------------------------------------------------------------------------

    # Thought is right now to introduce different post_baseline_visit_hook
    # setting the treatment based on variables that also have affect on death

    

)









######################################################################
### _targets.R ends here

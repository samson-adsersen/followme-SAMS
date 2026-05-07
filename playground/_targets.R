### _targets.R --- 
#----------------------------------------------------------------------
## Author: Samson Alfred Adsersen
## Created: May  6 2026 (14:22) 
## Version: 
## Last-Updated: maj  7 2026 (14:58) 
##           By: SADS0006
##     Update #: 72
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

# Packages (remember to set working directory)
library(targets)
library(tarchetypes)

# Load dependencies of outside pipeline
oldwd <- getwd()
setwd("..")
targets::tar_load_globals()
setwd(oldwd)

# Load run_one_sim function
tar_source("run_one.R")

# List of targets
list(
    tar_target(
        name = setting,
        command = {
            p <- get_rtmle_example_setting()
            p$parameter_values <- modifyList(
                p$parameter_values,
                list(
                    effect_auto_A_A = 5,
                    effect_auto_B_B = 2.5,
                    scale_dropout = 0,
                    scale_diabetes = 0.01))
            p}),

    ## tarchetypes::tar_map_rep(
    ##                  name = sim_results,
    ##                  command = {

    ##                      learner <- list(
    ##                          name = paste("glmnet", selector, alpha, sep = "_"),
    ##                          fun = "learn_glmnet",
    ##                          alpha = alpha,
    ##                          selector = selector
    ##                      )

    ##                      seed_snapshot <- .Random.seed

    ##                      tryCatch(

    ##                          run_one_simulation(
    ##                              setting,
    ##                              learner = learner,
    ##                              n = n
    ##                          ),

    ##                          error = function(e) {

    ##                              cat("\n====================\n")
    ##                              cat("TARGET FAILED\n")
    ##                              cat("n:", n, "\n")
    ##                              cat("selector:", selector, "\n")
    ##                              cat("alpha:", alpha, "\n")
    ##                              cat("learner:", learner$name, "\n")
    ##                              cat("seed:\n")
    ##                              dput(seed_snapshot)
    ##                              cat("\n====================\n")

    ##                              stop(e)
    ##                          }
    ##                      )
    ##                  },

    ##                  values = tidyr::expand_grid(
    ##                                      n = c(100, 200),
    ##                                      selector = c("min", "1se", "undersmooth"),
    ##                                      alpha = c(0, 0.5, 1)
    ##                                  ),

    ##                  reps = 10
    ##              ),
    
    tarchetypes::tar_map_rep(
                     name =  sim_results,
                     command = {
                         learner <- list(
                             name = paste("glmnet", selector, alpha, sep = "_"),
                             fun = "learn_glmnet",
                             alpha = alpha,
                             selector = selector)
                         message(learner$name)
                         run_one_simulation(
                             setting,
                             learner = learner,
                             n = n)},
                     values =  tidyr::expand_grid(
                                          n = c(1000, 2000),
                                          selector = c("min",
                                                       "1se",
                                                       "undersmooth"),
                                          alpha = c(0, 0.5, 1)),
                     reps = 10),

    tar_target(
        boxplot_estimates,
        {
            ggplot2::ggplot(
                      sim_results,
                      ggplot2::aes(
                                   x = factor(n),
                                   y = difference,
                                   fill = paste("glmnet",
                                                selector,
                                                alpha,
                                                sep = "_"))) +
                ggplot2::geom_boxplot(
                             position = ggplot2::position_dodge(width = 0.8)) +
                ggplot2::geom_hline(
                             yintercept = setting$parameter_values$effect_A_Diabetes - setting$parameter_values$effect_B_Diabetes,
                             linetype = "dashed") +
                ggplot2::labs(
                             x = "Sample size",
                             y = "Estimated difference in diabetes risk \nfor treatments A and B",
                             fill = "Learner",
                             title = "Distribution of estimates across simulation runs") +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                             plot.title = ggplot2::element_text(hjust = 0.5))
        }),
  
    tar_target(
        save_boxplot,
        {
            ggplot2::ggsave(
                         filename = "boxplot_estimates.png",
                         plot = boxplot_estimates,
                         width = 6,
                         height = 4
                     )
            "boxplot_estimates.png"},
        format = "file")

)

######################################################################
### _targets.R ends here

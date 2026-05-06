### _targets.R --- 
#----------------------------------------------------------------------
## Author: Samson Alfred Adsersen
## Created: May  6 2026 (14:22) 
## Version: 
## Last-Updated: May  6 2026 (15:58) 
##           By: Samson Alfred Adsersen
##     Update #: 15
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
  tar_target(seed, seq_len(100)),

  tar_target(
    sim_result,
    run_one_simulation(seed),
    pattern = map(seed)
  ),

  tar_target(
  boxplot_estimates,
  {
    ggplot2::ggplot(sim_result, ggplot2::aes(y = estimate)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(
        y = "Estimated diabetes risk",
        title = "Distribution of estimates across simulation runs"
      ) +
      ggplot2::theme_minimal()
  }
  ),

  tar_target(
      save_boxplot,
      {
    ggplot2::ggsave(
      filename = "boxplot_estimates.png",
      plot = boxplot_estimates,
      width = 6,
      height = 4
    )

    "boxplot_estimates.png"
  },
  format = "file"
  )
  
)

######################################################################
### _targets.R ends here

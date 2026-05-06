### run_one.R --- 
#----------------------------------------------------------------------
## Author: Samson Alfred Adsersen
## Created: May  6 2026 (14:16) 
## Version: 
## Last-Updated: May  6 2026 (15:49) 
##           By: Samson Alfred Adsersen
##     Update #: 19
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

initialize_treatment <- function(X){
    X[,A:=1*(randomized_treatment == 1)]
    X[,B:=1*(randomized_treatment == 0)]
    X[,randomized_treatment := NULL]
    X[]
}

run_one_simulation <- function(seed, n = 10000) {
  set.seed(seed)

  p <- get_rtmle_example_setting()

  p$parameter_values <- modifyList(
    p$parameter_values,
    list(
      effect_auto_A_A = 5,
      effect_auto_B_B = 2.5,
      scale_dropout = 0,
      scale_diabetes = 0.01
    )
  )

  d <- do.call(
    simulate_cohort,
    c(
      list(
        n = n,
        post_baseline_visit_hook = initialize_treatment
      ),
      p
    )
  )

  rd <- register_format(
    d,
    bsl_vars = c("sex", "age", "BMI"),
    treatment_vars = c("A", "B"),
    measurement_vars = c("SBP"),
    outcome = "Diabetes"
  )

  x <- rtmle_init(
    time_grid = seq(0, 5, .5),
    name_id = "id",
    name_outcome = "diabetes",
    name_competing = "death",
    name_censoring = "cens"
  )

  x <- add_baseline_data(x, rd$baseline_data)
  x <- do.call(add_long_data, c(list(x), rd[-1]))

  # Setup protocols
  x <- protocol(x,
                name = "use_A",
                intervention = data.table(time = x$intervention_nodes,
                                          "A" = factor(1,0:1)))
  x <- protocol(x,
                name = "use_A_not_B",
                intervention = data.table(time = x$intervention_nodes,
                                          "A" = factor(1,0:1),
                                          "B" = factor(0,0:1)))
  x <- protocol(x,
                name = "use_B",
                intervention = data.table(time = x$intervention_nodes,
                                          "B" = factor(1,0:1)))

  x <- target(x, "diabetes_risk",
              protocols = c("use_A",
                            "use_B"))
                            #"use_A_not_B"))
  x <- long_to_wide(x)
  x <- prepare_rtmle_data(x)
  x <- model_formula(x, exclusion_rules = list("A" = "B_0", "B" = "A_0"))
  x <- run_rtmle(x, time_horizon = 3, learner = "learn_glmnet")

  out <- data.table::data.table(
                         estimate = summary(x)$Estimate,
                         seed = seed)
  out[]
  
}


######################################################################
### run_one.R ends here

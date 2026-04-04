run_rtmle_diabetes_population <- function(diabetes_population, time_grid, time_horizons,...){
    if (FALSE){
        tar_load_globals()
        tar_load(diabetes_population)
        time_horizons = 12
        time_grid = seq(0,12,1)
        learner = "learn_glmnet"
    }
    dd <- register_format(diabetes_population,
                          bsl_vars = c("sex","age"),
                          measurement_vars = "HbA1c",
                          treatment_vars = c("GLP1","SGLT2","DPP4"),
                          event_vars = NULL,
                          outcome = "MACE")
    x <- rtmle::rtmle_init(time_grid = time_grid,
                           name_id = "id",
                           name_outcome = "MACE",
                           name_competing = "death",
                           name_censoring = "Censored",
                           censored_label = "censored")
    ## diabetes_population[last == 1,table(event)]
    x <- do.call(rtmle::add_long_data,c(list(x = x),dd[-1]))
    x <- rtmle::add_baseline_data(x,data=dd[[1]])
    x <- rtmle::long_to_wide(x,start_followup_date = 0,HbA1c = list(method = "measurement",fun_aggregate = "last"))
    x <- rtmle::protocol(x,name = "Always_SGLT2",
                         intervention = data.table(time = x$intervention_nodes,"SGLT2" = factor("1",levels = c("0","1"))))
    x <- rtmle::protocol(x,name = "Always_DPP4",
                         intervention = data.table(time = x$intervention_nodes,"DPP4" = factor("1",levels = c("0","1"))))
    x <- rtmle::protocol(x,name = "Always_GLP1",
                         intervention = data.table(time = x$intervention_nodes,"GLP1" = factor("1",levels = c("0","1"))))
    x <- rtmle::target(x,name = "Outcome_risk",
                       estimator = "tmle",
                       protocols = c("Always_SGLT2","Always_GLP1","Always_DPP4"))
    x <- rtmle::prepare_rtmle_data(x)
    x <- rtmle::model_formula(x,exclusion_rules = list("SGLT2_0" = c("GLP1_0","DPP4_0"),
                                                       "GLP1_0" = c("SGLT2_0","DPP4_0"),
                                                       "DPP4_0" = c("SGLT2_0","GLP1_0")))
    x <- run_rtmle(x, time_horizon = time_horizons, ...)
    return(x)
}

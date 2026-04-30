get_diabetes_polypharmacy_setting <- function(){
    max_follow <- 60
    baseline_variables <- list("U" = "normal",
                               "sex" = "binomial",
                               "age" = "normal",
                               "HbA1c" = "normal")
    absorbing_events <- list("death" = "Weibull","dropout" = "Weibull")
    intermediate_events <- list("MACE" = "Weibull")
    visit_measurements <- list(
        "changeHbA1c" = "normal"
    )
    baseline_visit <- list("start_GLP1" = "binomial",
                           "start_SGLT2" = "binomial")
    visit_events <- list("GLP1" = "binomial",
                         "SGLT2" = "binomial",
                         "DPP4" = "binomial")
    visit_schedule <- list("mean" = 6,"sd" = 1,skip = 0)
    if (FALSE){
    ipv = initialize_parameter_values(baseline_variables = names(baseline_variables),
                                      absorbing_events = names(absorbing_events),
                                      baseline_visit = names(baseline_visit),
                                      intermediate_events = names(intermediate_events),
                                      visit_measurements = names(visit_measurements),
                                      visit_events = names(visit_events))
    
    dput(ipv)
    }
    ipv <- list(intercept_U = 0, intercept_sex = 0, intercept_age = 70, intercept_HbA1c = 50,
                var_age = 6,
                var_HbA1c = 3,
                intercept_start_GLP1 = logit(0.33), intercept_start_SGLT2 = logit(0.5), intercept_changeHbA1c = 0, 
                intercept_GLP1 = 0, intercept_SGLT2 = 0, intercept_DPP4 = 0, 
                scale_death = 0.0001, scale_dropout = 0.0001, scale_MACE = 0.005, 
                effect_sex_U = 0, effect_age_U = 0, effect_HbA1c_U = 0, effect_U_sex = 0, 
                effect_age_sex = 0, effect_HbA1c_sex = 0, effect_U_age = 0, 
                effect_sex_age = 0, effect_HbA1c_age = 0, effect_U_HbA1c = 0, 
                effect_sex_HbA1c = 0, effect_age_HbA1c = 0, effect_U_start_GLP1 = 0, 
                effect_sex_start_GLP1 = 0, effect_age_start_GLP1 = 0, effect_HbA1c_start_GLP1 = 0, 
                effect_U_start_SGLT2 = 0, effect_sex_start_SGLT2 = 0, effect_age_start_SGLT2 = 0, 
                effect_HbA1c_start_SGLT2 = 0, effect_U_GLP1 = 0, effect_sex_GLP1 = 0, 
                effect_age_GLP1 = 0, effect_HbA1c_GLP1 = 0, effect_U_SGLT2 = 0, 
                effect_sex_SGLT2 = 0, effect_age_SGLT2 = 0, effect_HbA1c_SGLT2 = 0, 
                effect_U_DPP4 = 0, effect_sex_DPP4 = 0, effect_age_DPP4 = 0, 
                effect_HbA1c_DPP4 = 0, effect_U_changeHbA1c = 0, effect_sex_changeHbA1c = 0, 
                effect_age_changeHbA1c = 0, effect_HbA1c_changeHbA1c = 0, 
                effect_U_death = 0, effect_sex_death = 0, effect_age_death = 0, 
                effect_HbA1c_death = 0, effect_U_dropout = 0, effect_sex_dropout = 0, 
                effect_age_dropout = 0, effect_HbA1c_dropout = 0, effect_U_MACE = 0, 
                effect_sex_MACE = 0, effect_age_MACE = 0, effect_HbA1c_MACE = 0, 
                effect_MACE_death = 0, effect_changeHbA1c_death = 0, effect_GLP1_death = 0, 
                effect_SGLT2_death = 0, effect_DPP4_death = 0, effect_MACE_dropout = 0, 
                effect_changeHbA1c_dropout = 0, effect_GLP1_dropout = 0, 
                effect_SGLT2_dropout = 0, effect_DPP4_dropout = 0, effect_changeHbA1c_MACE = 0, 
                effect_GLP1_MACE = 0, effect_SGLT2_MACE = 0, effect_DPP4_MACE = 0)
    randomize_baseline_treatment <- function(X){
        X[,GLP1:=1*(start_GLP1 == 1)]
        X[,SGLT2:=1*(start_SGLT2 == 1 & start_GLP1 == 0)]
        X[,DPP4:=1*(start_SGLT2 == 0 & start_GLP1 == 0)]
        X[,start_GLP1 := NULL]
        X[,start_SGLT2 := NULL]
        X[]
    }
    list(max_follow = max_follow,
         baseline_variables = baseline_variables,
         baseline_visit = baseline_visit,
         post_baseline_visit_hook = randomize_baseline_treatment,
         absorbing_events = absorbing_events,
         intermediate_events = intermediate_events,
         visit_measurements = visit_measurements,
         visit_events = visit_events,
         visit_schedule = visit_schedule,
         parameter_values = ipv)
}

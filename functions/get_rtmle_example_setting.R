get_rtmle_example_setting <- function(){
    max_follow <- 6
    baseline_variables <- list("U" = "normal",
                               "sex" = "binomial",
                               "age" = "normal",
                               "BMI" = "normal",
                               "SBP" = "normal")
    baseline_visit <- list("randomized_treatment" = "binomial")
    absorbing_events <- list("death" = "Weibull","dropout" = "Weibull")
    intermediate_events <- list("Diabetes" = "Weibull")
    visit_measurements <- list(
        "change_SBP" = "normal"
    )
    visit_events <- list("A" = "binomial","B" = "binomial")
    visit_schedule <- list("mean" = 1,"sd" = 1,skip = 0)
    if (FALSE){
        ipv <- initialize_parameter_values(baseline_variables = names(baseline_variables),
                                           baseline_visit = names(baseline_visit),
                                           absorbing_events = names(absorbing_events),
                                           intermediate_events = names(intermediate_events),
                                           visit_measurements = names(visit_measurements),
                                           visit_events = names(visit_events))
    
        dput(ipv)
    }
    ipv <- list(intercept_U = 0, intercept_sex = 0.4, intercept_age = 60, intercept_BMI = 28, 
                intercept_SBP = 150, intercept_change_SBP = 0, intercept_randomized_treatment = 0.5, intercept_A = 0,
                intercept_B = 0, scale_death = 0.01, scale_dropout = 0.01, 
                scale_Diabetes = 0.02, effect_sex_U = 0, effect_age_U = 0, 
                effect_BMI_U = 0, effect_SBP_U = 0, effect_U_sex = 0, effect_age_sex = 0, 
                effect_BMI_sex = 0, effect_SBP_sex = 0, effect_U_age = 0, 
                effect_sex_age = 0, effect_BMI_age = 0, effect_SBP_age = 0, 
                effect_U_BMI = 0, effect_sex_BMI = 0, effect_age_BMI = 0, 
                effect_SBP_BMI = 0, effect_U_SBP = 0, effect_sex_SBP = 0, 
                effect_age_SBP = 0, effect_BMI_SBP = 0, effect_U_A = 0, effect_sex_A = 0, 
                effect_age_A = 0, effect_BMI_A = 0, effect_SBP_A = 0, effect_U_B = 0, 
                effect_sex_B = 0, effect_age_B = 0, effect_BMI_B = 0, effect_SBP_B = 0, 
                effect_U_change_SBP = 0, effect_sex_change_SBP = 0, effect_age_change_SBP = 0, 
                effect_BMI_change_SBP = 0, effect_SBP_change_SBP = 0, effect_U_death = 0, 
                effect_sex_death = 0, effect_age_death = 0, effect_BMI_death = 0, 
                effect_SBP_death = 0, effect_U_dropout = 0, effect_sex_dropout = 0, 
                effect_age_dropout = 0, effect_BMI_dropout = 0, effect_SBP_dropout = 0, 
                effect_U_Diabetes = 0, effect_sex_Diabetes = 0, effect_age_Diabetes = 0, 
                effect_BMI_Diabetes = 0, effect_SBP_Diabetes = 0, effect_Diabetes_death = 0, 
                effect_change_SBP_death = 0, effect_A_death = 0, effect_B_death = 0, 
                effect_Diabetes_dropout = 0, effect_change_SBP_dropout = 0, 
                effect_A_dropout = 0, effect_B_dropout = 0, effect_change_SBP_Diabetes = 0, 
                effect_A_Diabetes = 0, effect_B_Diabetes = 0)
    list(max_follow = max_follow,
         baseline_variables = baseline_variables,
         baseline_visit = list(randomized_treatment = "binomial"),
         absorbing_events = absorbing_events,
         intermediate_events = intermediate_events,
         visit_measurements = visit_measurements,
         visit_events = visit_events,
         visit_schedule = visit_schedule,
         parameter_values = ipv)
}
5

make_and_write_diabetes_data <- function(file_name, diabetes_polypharmacy_setting){
    dps <- diabetes_polypharmacy_setting
    dps$parameter_values <- modifyList(dps$parameter_values,list(effect_GLP1_MACE = -0.7,
                                                                 effect_History_GLP1_MACE = -5,
                                                                 scale_MACE = 0.002))
    d <- simulate_diabetes_population(diabetes_polypharmacy_setting = dps,
                                      initial_treatment = list(GLP1 = 801,SGLT2 = 1077,DPP4 = 1304))

    d[, U := NULL]
    setkey(d,id,time,event)    
    fwrite(d, file = file_name)
    file_name
}

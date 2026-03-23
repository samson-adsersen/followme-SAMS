make_and_write_diabetes_data <- function(file_name, diabetes_polypharmacy_setting){
    dps <- diabetes_polypharmacy_setting
    dps$parameter_values <- modifyList(dps$parameter_values,list(effect_GLP1_MACE = -0.7,
                                                                 effect_History_GLP1_MACE = -5,
                                                                 scale_MACE = 0.002))
    d <- do.call("simulate_cohort",
                 c(list(n = 3000),diabetes_polypharmacy_setting))
    d[, U := NULL]
    setkey(d,id,time,event)    
    fwrite(d, file = file_name)
    file_name
}

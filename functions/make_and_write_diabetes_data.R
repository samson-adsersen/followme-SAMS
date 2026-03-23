make_and_write_diabetes_data <- function(file_name, diabetes_polypharmacy_setting){
    dps <- diabetes_polypharmacy_setting
    dps$parameter_values <- modifyList(dps$parameter_values,list(effect_GLP1_MACE = -0.7,
                                                                 effect_History_GLP1_MACE = -5,
                                                                 scale_MACE = 0.002))
    d <- do.call("simulate_cohort",
                 c(list(n = 3000),diabetes_polypharmacy_setting))
    setkey(d,id,time,event)
    ## Setup to look more realistic
    d[, U := NULL]
    d[, HbA1c := HbA1c + cumsum(changeHbA1c), id]
    d = d[, .(id, sex, age, event, time, HbA1c, GLP1, SGLT2, DPP4, MACE)]

    fwrite(d, file = file_name)
    file_name
}

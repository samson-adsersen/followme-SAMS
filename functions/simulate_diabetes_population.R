simulate_diabetes_population <- function(diabetes_polypharmacy_setting,initial_treatment){
    if(FALSE){
        tar_load(diabetes_polypharmacy_setting)
        initial_treatment <- list(GLP1 = 577,SGLT2 = 801,DPP4 = 1304)
    }
    update_list <- function(a,b){
        a[[names(b)]] <- b[[1]]
        a
    }
    intervene <- diabetes_polypharmacy_setting$intervene
    intervene_values <- diabetes_polypharmacy_setting$intervene_values
    intervene_variables <- diabetes_polypharmacy_setting$intervene_variables
    if (!is.null(intervene)) {
        if (intervene) {
            if (sum(intervene_values) != 1) {
                stop("intervene_values should specify which treatment to intervene on at baseeline; cannot start on multiple treatments")
            } else {
                ## Find the values with value 1 in intervene_values and set those to 1 in the initial visit values
                intervention_variable <- intervene_variables[intervene_values == 1]
                ## Initial treatment where it is not intervention_variable set to 0
                sample_size <- sum(unlist(initial_treatment))
                initial_treatment[[intervention_variable]] <- sample_size
                initial_treatment[setdiff(names(initial_treatment), intervention_variable)] <- 0
            }
        }
    } 

    untreat <- list(GLP1 = 0,SGLT2 = 0,DPP4 = 0)
    id_start <- cumsum(c(0,initial_treatment[-length(initial_treatment)]))
    cohort <- rbindlist(
        lapply(1:length(initial_treatment),function(j){
            init_treat <- names(initial_treatment)[[j]]
            if (initial_treatment[[init_treat]] == 0) return(NULL)
            d <- do.call("simulate_cohort",c(list(n = initial_treatment[[init_treat]]),
                                             update_list(a = diabetes_polypharmacy_setting,
                                                         b = list(initial_visit_values = update_list(untreat,setNames(list(1),init_treat))))))
            d[,id := id_start[[j]]+id]
            d[]
        }))
    setkey(cohort, id, time)
    cohort
}


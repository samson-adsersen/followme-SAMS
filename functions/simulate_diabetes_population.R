simulate_diabetes_population <- function(diabetes_polypharmacy_setting,initial_treatment){
    if(FALSE){
        tar_load(diabetes_polypharmacy_setting)
        initial_treatment <- list(GLP1 = 577,SGLT2 = 801,DPP4 = 1304)
    }
    update_list <- function(a,b){
        a[[names(b)]] <- b[[1]]
        a
    }
    treat_list <- function(name,value){
        setNames(list(value),name)
    }
    untreat <- list(GLP1 = 0,SGLT2 = 0,DPP4 = 0)
    id_start <- cumsum(c(0,initial_treatment[-length(initial_treatment)]))
    cohort <- rbindlist(
        lapply(1:length(initial_treatment),function(j){
            init_treat <- names(initial_treatment)[[j]]
            d <- do.call("simulate_cohort",c(list(n = initial_treatment[[init_treat]]),
                                             update_list(a = diabetes_polypharmacy_setting,
                                                         b = list(initial_visit_values = update_list(untreat,setNames(list(1),init_treat))))))
            d[,id := id_start[[j]]+id]
            d[]
        }))
}

simulate_diabetes_population <- function(diabetes_polypharmacy_setting,
                                         n,
                                         intervention = NULL){
    if (length(intervention)>0){
        set_intervention <- function(X){
            X <- data.table::setDT(setNames(rep(intervention[[1]],NROW(X)),names(intervention)))
            print(X)
            X
        }
        diabetes_polypharmacy_setting$post_baseline_visit_hook <- set_intervention
    }
    cohort <- do.call("simulate_cohort",
                      c(list(n = n),
                        diabetes_polypharmacy_setting))
    setkey(cohort, id, time)
    cohort[]
}


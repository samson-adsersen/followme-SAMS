run_rtmle_diabetes_population <- function(diabetes_population, intervals = seq(0, 60, 6), time_horizons = 30,...){
    if (FALSE){
        tar_load_globals()
        tar_load(diabetes_population)
    }
    setkey(diabetes_population,id,time,event)
    time_horizons <- match(time_horizons, intervals) - 1
    if (any(is.na(time_horizons))){
        stop("time_horizons must be a subset of intervals")
    }
    if (any(time_horizons == 0)){
        stop("time_horizons must not include 0")
    }
    x <- rtmle::rtmle_init(intervals = length(intervals)-1,
                           name_id = "id",
                           name_outcome = "MACE",
                           name_competing = "death",
                           name_censoring = "Censored",
                           censored_label = "censored")
    diabetes_population[, last := as.integer(.I == .I[.N]), by = id]
    diabetes_population[, first := as.integer(.I == .I[1L]), by = id]
    ## diabetes_population[last == 1,table(event)]
    tv_covs <- c("HbA1c","changeHbA1c","GLP1","SGLT2","DPP4")
    tv <- lapply(tv_covs, function(tv){
        if (tv %in% c("GLP1","SGLT2","DPP4")){
            d <- diabetes_population[,c("id","time",tv),with = FALSE]
            setnames(d,c("id","start_exposure","value"))
            d[,end_exposure := shift(x = start_exposure,n = 1,type = "lead"),by = id]
            # when treatment starts at the last time point in a time-series
            # then the duration is unknown
            d <- d[!is.na(end_exposure)]
            d <- d[value != 0]
            d[,value := NULL]
        }else{
            d <- diabetes_population[event%in%c("baseline","MACE","visit"),c("id","time",tv),with = FALSE]
            setnames(d,c("id","date","value"))
        }
        d[]
    })    
    names(tv) <- tv_covs
    x <- rtmle::add_long_data(x,
                              outcome_data=diabetes_population[event == "MACE",.(id,date = time)][!duplicated(id)],
                              censored_data=diabetes_population[last == 1 &event %in% c("visit","dropout"),.(id,date = time)],
                              competing_data=diabetes_population[event == "death",.(id,date = time)],
                              timevar_data=tv)
    x <- rtmle::add_baseline_data(x,data=diabetes_population[first == 1,.(id,sex,age)])
    x <- rtmle::long_to_wide(x,breaks = intervals,start_followup_date = 0)
    x <- rtmle::protocol(x,name = "Always_SGLT2",
                         intervention = data.table(time = x$intervention_nodes,"SGLT2" = factor(rep("1",length(intervals)-1),levels = c("0","1"))))
    x <- rtmle::protocol(x,name = "Always_DPP4",
                         intervention = data.table(time = x$intervention_nodes,"DPP4" = factor(rep("1",length(intervals)-1),levels = c("0","1"))))
    x <- rtmle::protocol(x,name = "Always_GLP1",
                         intervention = data.table(time = x$intervention_nodes,"GLP1" = factor(rep("1",length(intervals)-1),levels = c("0","1"))))
    x <- rtmle::target(x,name = "Outcome_risk",
                       estimator = "tmle",
                       protocols = c("Always_SGLT2","Always_GLP1","Always_DPP4"))
    x <- rtmle::prepare_data(x)
    x <- rtmle::model_formula(x,exclusion_rules = list("SGLT2_0" = c("GLP1_0","DPP4_0"),
                                                       "GLP1_0" = c("SGLT2_0","DPP4_0"),
                                                       "DPP4_0" = c("SGLT2_0","GLP1_0")))
    x <- run_rtmle(x, time_horizon = time_horizons, ...)
    return(x)
}

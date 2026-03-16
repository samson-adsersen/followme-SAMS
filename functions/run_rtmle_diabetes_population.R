run_rtmle_diabetes_population <- function(diabetes_population,time_horizon){
    if (FALSE){
        library(rtmle)
        library(targets)
        library(data.table)
        tar_load(diabetes_population)
    }
    setkey(diabetes_population,id,time)
    bsl <- diabetes_population[event == "baseline"]
    setkey(diabetes_population,id,time,event)
    # find censored
    diabetes_population[, has_event := any(event%in%c("MACE","death")),by = id]
    event_time_data <- rbind(diabetes_population[event%in%c("MACE","death"),.(time = time[1],event = event[1]),by = id],
                             diabetes_population[!(has_event),.(time = max(time),event = "censored"),by = id])
    bsl[,time := NULL]
    bsl[,event := NULL]
    bsl <- event_time_data[bsl,on = "id"]
    bsl[,treatment := factor(GLP1+2*SGLT2+3*DPP4,levels = 1:3,labels = c("GLP1","SGLT2","DPP4"))]
    ggplot(bsl,aes(x = time,event = event,color = treatment))+geom_prodlim()+xlim(c(0,50))
    intervals <- seq(0,60,6)
    x <- rtmle_init(intervals = length(intervals)-1,
                    name_id = "id",
                    name_outcome = "MACE",
                    name_competing = "death",
                    name_censoring = "Censored",
                    censored_label = "censored")
    diabetes_population[, last := as.integer(.I == .I[.N]), by = id]
    diabetes_population[, first := as.integer(.I == .I[1L]), by = id]
    ## diabetes_population[last == 1,table(event)]
    tv_covs <- c("HbA1c","changeHbA1c","GLP1","SGLT2","DPP4","History_GLP1","History_SGLT2","History_DPP4")
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
    x <- add_long_data(x,
                       outcome_data=diabetes_population[event == "MACE",.(id,date = time)][!duplicated(id)],
                       censored_data=diabetes_population[last == 1 &event %in% c("visit","dropout"),.(id,date = time)],
                       competing_data=diabetes_population[event == "death",.(id,date = time)],
                       timevar_data=tv)
    x <- add_baseline_data(x,data=diabetes_population[first == 1,.(id,sex,age)])
    x <- long_to_wide(x,breaks = intervals,start_followup_date = 0)
    x <- protocol(x,name = "Always_SGLT2",
                  intervention = data.table(time = x$intervention_nodes,"SGLT2" = factor(rep("1",length(intervals)-1),levels = c("0","1"))))
    x <- protocol(x,name = "Always_DPP4",
                  intervention = data.table(time = x$intervention_nodes,"DPP4" = factor(rep("1",length(intervals)-1),levels = c("0","1"))))
    x <- protocol(x,name = "Always_GLP1",
                  intervention = data.table(time = x$intervention_nodes,"GLP1" = factor(rep("1",length(intervals)-1),levels = c("0","1"))))
    x <- target(x,name = "Outcome_risk",
                estimator = "tmle",
                protocols = c("Always_SGLT2","Always_GLP1","Always_DPP4"))
    x <- prepare_data(x)
    x <- model_formula(x)
    x <- run_rtmle(x,
                   time_horizon = time_horizon,
                   learner = "learn_glm")
    return(x)
}

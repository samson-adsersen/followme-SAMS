run_aalen_johansen_diabetes_population <- function(diabetes_population,time_horizon){
    d <- copy(diabetes_population)
    setkey(d,id,time,event)
    # find censored
    d[, has_event := any(event%in%c("MACE","death")),by = id]
    event_time_data <- rbind(d[event%in%c("MACE","death"),.(time = time[1],event = event[1]),by = id],d[!(has_event),.(time = max(time),event = "censored"),by = id])
    bsl <- d[event == "baseline"]
    bsl[,time := NULL]
    bsl[,event := NULL]
    bsl <- event_time_data[bsl,on = "id"]
    bsl[,treatment := factor(GLP1+2*SGLT2+3*DPP4,levels = 1:3,labels = c("GLP1","SGLT2","DPP4"))]
    bsl[,event:=factor(event,levels=c("censored","MACE","death"),labels=c("censored","MACE","death"))]
    bsl[,status := as.numeric(factor(event))-1]
    fit <- prodlim(Hist(time,event,cens.code = "censored")~treatment,data = bsl)
    summary(fit,times = time_horizon)
}

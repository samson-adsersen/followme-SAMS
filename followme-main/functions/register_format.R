### register_format.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: apr  2 2026 (07:01) 
## Version: 
## Last-Updated: apr  2 2026 (07:41) 
##           By: Thomas Alexander Gerds
##     Update #: 14
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
register_format <- function(data,
                         bsl_vars = NULL,
                         treatment_vars = NULL,
                         measurement_vars = NULL,
                         event_vars = NULL,
                         outcome){
    setkey(data,id,time,event)
    data[, last := as.integer(.I == .I[.N]), by = id]
    data[, first := as.integer(.I == .I[1L]), by = id]
    if (length(treatment_vars)>0){
        treatments <- lapply(treatment_vars, function(tv){
            d <- data[,c("id","time",tv),with = FALSE]
            setnames(d,c("id","start_date","value"))
            d[,end_date := shift(x = start_date,n = 1,type = "lead"),by = id]
            # when treatment starts at the last time point in a time-series
            # then the duration is unknown
            d <- d[!is.na(end_date)]
            d <- d[value != 0]
            d[,value := NULL]
            d[]
        })
        names(treatments) <- treatment_vars
    }else{
        treatments <- NULL
    }
    change_vars <- grep("^change",names(data),value = TRUE)
    if (length(measurement_vars)>0){
        measurements <- lapply(measurement_vars, function(tv){
            d <- data[event%in%c("baseline",outcome,"visit"),c("id","time",tv),with = FALSE]
            if (length((change <- grep(tv,change_vars,value = TRUE)) == 1)){
                set(d,j = tv,value = d[[tv]]+data[event%in%c("baseline",outcome,"visit")][[change]])
            }
            setnames(d,c("id","date","value"))
            d[]
        })
        names(measurements) <- measurement_vars
    }else{
        measurements <- NULL
    }
    if (length(event_vars)>0){
        events <- lapply(event_vars,function(e){
            data[event == e,.(id,date = time)]
        })
        names(events) <- event_vars
    }else{
        events <- NULL
    }
    list(
        baseline_data = if(length(bsl_vars)>0){data[first == TRUE,c("id",bsl_vars),with = FALSE]}else{NULL},
        outcome_data = data[event == outcome,.(id,date = time)][!duplicated(id)],
        censored_data = data[last == 1 &event %in% c("visit","dropout"),.(id,date = time)],
        competing_data = data[event == "death",.(id,date = time)],
        timevar_data = c(treatments,measurements,events)
    )
}


######################################################################
### register_format.R ends here

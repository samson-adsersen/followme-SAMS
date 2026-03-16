library(targets)
library(plotly)
tar_load_globals()
tar_load_everything()
dps <- diabetes_polypharmacy_setting
dps$parameter_values <- modifyList(dps$parameter_values,list(effect_GLP1_MACE = -0.7,effect_History_GLP1_MACE = -5,scale_MACE = 0.002))
d <- simulate_diabetes_population(diabetes_polypharmacy_setting = dps,initial_treatment = list(GLP1 = 801,SGLT2 = 1077,DPP4 = 1304))
setkey(d,id,time,event)
# find censored
d[, has_event := any(event%in%c("MACE","death")),by = id]
event_time_data <- rbind(d[event%in%c("MACE","death"),.(time = time[1],event = event[1]),by = id],d[!(has_event),.(time = max(time),event = "censored"),by = id])
bsl <- d[event == "baseline"]
bsl[,time := NULL]
bsl[,event := NULL]
bsl <- event_time_data[bsl,on = "id"]
bsl[,treatment := factor(GLP1+2*SGLT2+3*DPP4,levels = 1:3,labels = c("GLP1","SGLT2","DPP4"))]
ggplot(bsl,aes(x = time,event = event,color = treatment))+geom_prodlim(cause = "MACE",cens.code = "censored",conf_int = 0L)+xlim(c(0,50))


x <- run_rtmle_diabetes_population(diabetes_population = d,time_horizon = 3)
x$models$time_1$outcome$MACE_2$fit$Always_GLP1

x$models$time_0$Always_GLP1

plot_model_coefficients(x,plot_style = "manhattan",time_horizon = 3,protocol = "Always_GLP1",manhattan_color_by = "node",show_x_labels = FALSE)

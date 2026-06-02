### get_LEADER_setting.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: jun  2 2026 (12:44) 
## Version: 
## Last-Updated: jun  2 2026 (14:19) 
##           By: SADS0006
##     Update #: 4
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

get_LEADER_setting <- function(){

    #-----------------------Parameters-----------------------------

    # Maximum amount of followup events in the time-series
    max_follow <- 24

    # Baseline variables and their distributions
    baseline_variables <- list(
        # demographics
        sex = "binomial",
        age = "normal",
        diabetesduration = "binomial",
        
        # baseline biomarkers
        egfr = "normal",
        bmi = "normal",
        
        # baseline concomitant medication
        mineralcor = "binomial",
        bb = "binomial",
        rasi = "binomial",
        loop = "binomial",
        ccb = "binomial",
        thiazide = "binomial",
        sasid = "binomial",
        statin = "binomial",
        
        # baseline comorbidities
        hypertension = "binomial",
        diabetic.nephropathy = "binomial",
        stenosis = "binomial",
        disorders.of.rhythm.or.conduction = "binomial",
        left.ventricular.systolic.dysfunction = "binomial",
        gallstone.disease = "binomial",
        pancreatitis = "binomial",
        diabetic.foot.ulcer = "binomial",
        diabetic.neuropathy = "binomial",
        transient.ischaemic.attack = "binomial"
    )

    # List of events ending the time-series (death/dropout/censoring)
    absorbing_events <- list(
        cv.death = "Weibull",
        all.cause.mortality = "Weibull",
        dropout = "Weibull"
    )

    # List of other possible events between visits.
    intermediate_events <- list(
        # chronic events
        mi = "Weibull",
        stroke = "Weibull",
        uap = "Weibull",
        revasc = "Weibull",
        heart.failure = "Weibull",
        
        # periodic adverse events
        atrial.fibrillation = "Weibull",
        nausea.and.vomiting.symptoms = "Weibull",
        diarrhoea = "Weibull"
    )

    # What is measured at a visit
    visit_measurements <- list(
        hba1c_level = "normal",
        hba1c_change = "normal"
    )

    # What info is given at baseline
    baseline_visit <- list(
        start_GLP1RA = "binomial",
        start_SGLT2i = "binomial"
    )

    # What could happen based on the visit (Medication/treatment)
    visit_events <- list(
        # glucose-lowering medication states/actions
        metformin = "binomial",
        sulfonylurea = "binomial",
        insulin = "binomial",
        alfa_g_inhib = "binomial",
        thiazo = "binomial",
        insulin_one = "binomial",
        dpp4 = "binomial"
    )

    # Thomas says here 0,1,3,6,12 and so on. Should it be random?
    # Change later he says. Have made 6 month grid. Add 1,3 in post?
    visit_schedule <- list(
        mean =  6, sd = 0, skip = 0
    )


    
    #-----------------------Setting-List---------------------------

    # Generate initial parameter values list
    #if (FALSE){
    ipv = initialize_parameter_values(
        baseline_variables = names(baseline_variables),
        absorbing_events = names(absorbing_events),
        baseline_visit = names(baseline_visit),
        intermediate_events = names(intermediate_events),
        visit_measurements = names(visit_measurements),
        visit_events = names(visit_events))
    #}

    # Randomized baseline treatment
    randomize_baseline_treatment <- function(X){
        X[,GLP1RA := 1*(start_GLP1RA == 1)]
        X[,SGLT2i := 1*(start_SGLT2i == 1 & start_GLP1RA == 0)]
        X[,DPP4:=1*(start_SGLT2 == 0 & start_GLP1 == 0)]
        X[,start_GLP1 := NULL]
        X[,start_SGLT2 := NULL]
        X[]
    }

    # Output
    list(
         max_follow = max_follow,
        baseline_variables = baseline_variables,
        baseline_visit = list(randomized_treatment = "binomial"),
        absorbing_events = absorbing_events,
        intermediate_events = intermediate_events,
        visit_measurements = visit_measurements,
        visit_events = visit_events,
        visit_schedule = visit_schedule,
        parameter_values = ipv
    )
    
}

if (FALSE) {
      d <- do.call(
    simulate_cohort,
    c(
      list(
        n = n,
        post_baseline_visit_hook = randomize_baseline_treatment
      ),
      p
    )
  )
}









######################################################################
### get_LEADER_setting.R ends here

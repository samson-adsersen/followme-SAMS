### get_LEADER_setting.R --- 
#----------------------------------------------------------------------
## Author: 
## Created: jun  2 2026 (12:44) 
## Version: 
## Last-Updated: jun 18 2026 (13:12) 
##           By: SADS0006
##     Update #: 49
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:


# Grouping
group_variables <- function(X){
    X[, ':=' (
        age60below = as.integer(age < 60),
        age60to80 = as.integer(age >= 60 & age < 80),
        age80to90 = as.integer(age >= 80 & age < 90),
        age90above = as.integer(age >= 90)
        )]
    X[, diabetesduration_group := cut(
            diabetesduration,
            breaks = c(1,5,10,15,20,Inf),
            labels = c("1-5 years",
                       "5-10 years",
                       "10-15 years",
                       "15-20 years",
                       "more than 20 years")
        )]
}

# Randomized baseline treatment
randomize_baseline_treatment <- function(X){
    X[,lira := 1*(randomized_treatment == 1)]
    X[,placebo := 1*(randomized_treatment == 0)]
    X[]
}

update_parameter_hook <- function(){
    
}




get_LEADER_setting <- function(){

    #-----------------------Parameters-----------------------------

    # Maximum time value in the series.
    max_follow <- 60

    # Baseline variables and their distributions
    baseline_variables <- list(
        # demographics
        sex = "binomial",
        age = "normal",
        diabetesduration = "lognormal",
        
        # baseline biomarkers
        egfr = "lognormal",
        bmi = "normal",
        hba1c_level = "lognormal",
        
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
        # Redundant to have both. Can calc one from other.
        ####hba1c_level = "lognormal",
        hba1c_change = "normal",
        # Maybe introduce changes in other baseline biomarkers
        bmi_change = "normal",
        egfr_change = "normal"
    )

    # What info is given at baseline
    baseline_visit <- list(
        randomized_treatment = "binomial"
    )

    # What could happen based on a visit (Medication/treatment)
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
        mean = 6, sd = 0, skip = 0,
        schedule = c(1,3,seq(from = 6, to = max_follow, by = 6)),
        minimum_time_between_visits = 1
    )


    
    #-----------------------Setting-List---------------------------

    # Generate initial parameter values list
    if (FALSE){
    ipv = initialize_parameter_values(
        baseline_variables = baseline_variables,
        absorbing_events = absorbing_events,
        baseline_visit = baseline_visit,
        intermediate_events = intermediate_events,
        visit_measurements = visit_measurements,
        visit_events = visit_events,
        visit_schedule = visit_schedule)
    dput(ipv)
    }

    # Temporarily done every time - REMEMBER: use dput when done
    ipv = initialize_parameter_values(
        baseline_variables = baseline_variables,
        absorbing_events = absorbing_events,
        baseline_visit = baseline_visit,
        intermediate_events = intermediate_events,
        visit_measurements = visit_measurements,
        visit_events = visit_events,
        visit_schedule = visit_schedule)

    # Output
    list(
        max_follow = max_follow,
        baseline_variables = baseline_variables,
        baseline_visit = baseline_visit,
        absorbing_events = absorbing_events,
        intermediate_events = intermediate_events,
        visit_measurements = visit_measurements,
        visit_events = visit_events,
        visit_schedule = visit_schedule,
        parameter_values = ipv
    )
    
}

if (FALSE) {

    # LEADER setting generates essentially empty list
    # Effects/intercept/variance all 0 - scale = 0.01 for all.
    # For this example start filling out intercept/variance/scale

    # Baseline distributions
    p$parameter_values$intercept_age <- 60
    p$parameter_values$var_age <- 10
    p$parameter_values$intercept_bmi <- 32
    p$parameter_values$var_bmi <- 4
    p$parameter_values$intercept_egfr <- log(70)
    p$parameter_values$var_egfr <- 0.4
    p$parameter_values$intercept_diabetesduration <- log(10) 
    p$parameter_values$var_diabetesduration <- 0.3
    p$parameter_values$intercept_hba1c_level <- log(48)
    p$parameter_values$var_hba1c_level <- 0.4

    # Measurement distributions
    p$parameter_values$intercept_hba1c_change <- 0
    p$parameter_values$var_hba1c_change <- 10
    # Potentially add the other changing measurements.
    p$parameter_values$intercept_bmi_change <- 0
    p$parameter_values$var_bmi_change <- 1
    p$parameter_values$intercept_egfr_change <- 0
    p$parameter_values$var_egfr_change <- 10

    # Scale of events
    p$parameter_values$scale_dropout <- 0
    p$parameter_values$scale_all.cause.mortality <- 1/1000
    p$parameter_values$scale_cv.death <- 1/1000
    p$parameter_values$scale_stroke <- 1/1000

    # Important effects (Covariates on ...)
    p$parameter_values$effect_age_all.cause.mortality <- 1/100
    p$parameter_values$effect_diabetesduration_all.cause.mortality <- 1/100
    p$parameter_values$effect_age_cv.death <- 1/100
    p$parameter_values$effect_diabetesduration_cv.death <- 1/100

    # Important effects (Events on ...)
    p$parameter_values$effect_stroke_cv.death <- 1/100
    p$parameter_values$effect_heart.failure_cv.death <- 1/100

    # Visit schedule
    p$visit_schedule$sd <- 1/10
    p$visit_schedule$minimum_time_between_visits <- 1/10

    # Generate time-series based on setting - & potentially hooks
    d <- do.call(
        simulate_cohort,
        c(
            list(
                n = 1000,#,
                post_baseline_variables_hook = group_variables,
                post_baseline_visit_hook = randomize_baseline_treatment
            ),
            p
        )
    )

    # OKAY maybe do more functional setup for the following
    # Some of the baseline variables change over time.
    # Maybe ask Kathrine about what makes sense to have change.

    d[,age := age + (time/12)]
    d[,diabetesduration := diabetesduration + (time/12)]
    d[,hba1c_level := hba1c_level+hba1c_change]
    d[,bmi := bmi+bmi_change]
    d[,egfr := egfr+egfr_change]


    # Getting mean values from the effects object
    x <- data.table::as.data.table(
                         read.csv("c:/Users/sads0006/Desktop/followme-SAMS/playground/Data/u.csv")
                     )
    x[, outcome_base := sub("_[0-9]+$", "", outcome)]
    
    x_mean <- x[,
                .(mean_beta = mean(beta),
                  sd_beta = sd(beta),
                  n_time = .N),
                by = .(protocol, node, outcome_base, terms)
                ]
    x_mean <- x_mean[n_time == 9]

    x_mean[protocol == "Lira" & outcome_base == "primary.outcome"]
    
}







######################################################################
### get_LEADER_setting.R ends here

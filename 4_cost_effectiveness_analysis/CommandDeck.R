
require(readr); require(ggplot2); require(tidyverse)

source(paste(getwd(),"/(function)_outcomesAverted_estimator.R",sep=""),local=TRUE)
source(paste(getwd(),"/(function)_interventionCost_estimator.R",sep=""),local=TRUE)

LIST_CEA_settings = list("TLS")
CEA_risk_group = "adults_with_comorbidities"

TOGGLE_uncertainty = "fixed"
TOGGLE_numberOfRuns = 1
TOGGLE_discounting_rate = 0.03
TOGGLE_longCOVID = "off"
if (TOGGLE_uncertainty == "fixed"){TOGGLE_numberOfRuns = 1}



for (ticket in 1:TOGGLE_numberOfRuns){
  
  ###(1/3) Load antiviral model runs
  
  ###(2/3) Calculate QALYs, intervention costs, and healthcare costs averted
    
  outcomesAvertedEstimation <- outcomesAverted_estimator(LIST_CEA_settings,
                                                toggle_longCOVID = TOGGLE_longCOVID,
                                                 toggle_discounting_rate = TOGGLE_discounting_rate)
  #list including QALY_breakdown by setting,outcome_source,booster_vax_scenario,intervention,intervention_target_group, and 
  #               outcomes_averted by setting,outcome,booster_vax_scenario,intervention,intervention_target_group
    
  interventionCost_estimates <- interventionCost_estimator(LIST_CEA_settings,
                             antiviral_cost_scenario = "low_generic_cost",
                             wastage_rate_antiviralSchedule = 0)
  

  
  ###(3/3) CEA per setting
  #include plot of QALY breakdown by outcome_source
  
  
}



require(readr); require(ggplot2); require(tidyverse)
source(paste(getwd(),"/(function)_Translator.R",sep=""),local=TRUE)
source(paste(getwd(),"/(function)_QALY_estimator.R",sep=""),local=TRUE)
source(paste(getwd(),"/(function)_interventionCost_estimator.R",sep=""),local=TRUE)

LIST_CEA_settings = list("FJI")
CEA_risk_group = "adults_with_comorbidities"

TOGGLE_uncertainty = "fixed"
TOGGLE_numberOfRuns = 1
TOGGLE_discounting_rate = 0.03
if (TOGGLE_uncertainty == "fixed"){TOGGLE_numberOfRuns = 1}



for (ticket in 1:TOGGLE_numberOfRuns){
  
  ###(1/3) Load antiviral model runs
  TRANSLATED_antiviral_simulations <- Translator(LIST_CEA_settings,this_risk_group = CEA_risk_group)  
  # -> sample this run antiviral estimates?
  
  ###(2/3) Calculate QALYs, intervention costs, and healthcare costs averted
  QALY_estimates <- QALY_estimator(LIST_CEA_settings,discounting_rate = TOGGLE_discounting_rate)
  #NB: No uncertainty in these estimates as data sources are expert point estimates of:
  #       population (UN), life expectancy (UN), HRQoL (Robinson, Eber & Hammitt), and age_severity_specific_QALYs (Robinson, Eber & Hammitt)
  
  
  interventionCost_estimates <- interventionCost_estimator(LIST_CEA_settings,
                             antiviral_cost_scenario = "low_generic_cost",
                             wastage_rate_antiviralSchedule = 0)
  
  
  #rnorm(count = number of outcome, mean = est, sd = )
  
  

  
  

  
  ###(3/3) CEA per setting
  
  
  
}


### The 'CommandDeck' runs all sub-scripts of the COVID-19 transmission model to
### complete one standard 'run' of the disease model.



#### SETUP ##################################################################################
#load libraries
library(readr)
library(deSolve)
library(rvest)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(tidyverse)

rootpath = str_replace(getwd(), "GitHub_vaxAllocation","") #Note: x_results not stored within GitHub repository

# Load functions
source(paste(getwd(),"/04_functions/(function)_COVID_ODE.R",sep=""))
source(paste(getwd(),"/04_functions/(function)_VE_time_step.R",sep=""))
source(paste(getwd(),"/04_functions/(function)_rho_time_step.R",sep=""))
source(paste(getwd(),"/04_functions/(function)_vax_strategies.R",sep=""))
source(paste(getwd(),"/04_functions/(function)_vax_strategies_risk.R",sep=""))
if (exists("VE_estimates_imputed") == FALSE){load(file='01_inputs/VE_estimates_imputed.Rdata')}
#_____________________________________________________________________________________________


#### FUNCTION BODY ###########################################################################
CommandDeck <- function(
    
  ticket = 1,
  
  date_start = fitted_max_date,  
  model_weeks = 5,
  
  outbreak_timing = "off",
  strain_inital = 'omicron',     
  
  vax_strategy_toggle = "off",
  vax_risk_strategy_toggle = "off",
  risk_group_toggle = "on", 
  risk_group_name = "adults_with_comorbidities", #options: pregnant_women, adults_with_comorbidities
  RR_estimate  = 2,
  risk_group_prioritisation_to_date = NA,
  default_prioritisation_proportion = 0.5,
  risk_group_lower_cov_ratio = NA,
  sensitivity_analysis_toggles = list(),
  
  waning_toggle_acqusition = TRUE,
  waning_toggle_severe_outcome = TRUE, 
  waning_toggle_rho_acqusition = TRUE,
  
  setting = "SLE",
  
  fitting = "off",
  fitting_details = "off" #track Reff, rho, VE over time
  
){
  
  
  ### load refit, or refit if not recent enough
  if (fitting == "on"){
    warning('Fitting is on')
  } else if ( ! 'vax_hesistancy_risk_group' %in% names(sensitivity_analysis_toggles)){
    
    #load latest model run in known dates
    list_poss_Rdata = list.files(path="01_inputs/fit/",pattern = paste("fitted_results_",setting,"*",sep=""))
    list_poss_Rdata_details = double()
    for (i in 1:length(list_poss_Rdata)){
      list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                      file.info(paste("01_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
    }
    latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
    load(paste(rootpath,'x_results/',latest_file,sep=''))
    #___________________________________
    
    if('additional_doses' %in% names(sensitivity_analysis_toggles)){
      if (sensitivity_analysis_toggles$additional_doses == 'start_2022'){
        load(file = '01_inputs/fitted_results_SA_2022.Rdata')
      }
    }
    
    if (risk_group_toggle == "off"){
      this_risk_group_scenario = "no risk group"
    } else {
      this_risk_group_scenario = risk_group_name
    }
    
    parameters = fitted_results[[1]] %>% 
      filter(fitted_risk_group_scenario == this_risk_group_scenario)
    fitted_next_state = fitted_results[[2]] %>% 
      filter(fitted_risk_group_scenario == this_risk_group_scenario)
    fitted_incidence_log_tidy = fitted_results[[3]] %>% 
      filter(fitted_risk_group_scenario == this_risk_group_scenario)
    fitted_incidence_log = fitted_results[[4]] %>% 
      filter(fitted_risk_group_scenario == this_risk_group_scenario)
    rm(fitted_results)
    
    fitted_incidence_log_tidy = fitted_incidence_log_tidy %>% filter(date <= date_start) 
    fitted_incidence_log = fitted_incidence_log %>% filter(date <= date_start)
    
    if (risk_group_toggle == "on"){
      if ((is.na(risk_group_prioritisation_to_date) == FALSE) ){
        stop('no fitted result avaliable for this risk group characteristic')
      }
    }
  } else if('vax_hesistancy_risk_group' %in% names(sensitivity_analysis_toggles)){
    
    if (! risk_group_name == 'pregnant_women'){stop('havent configured vax hesistance sensitivity analysis for another risk group')}
    
    load(file = '01_inputs/SA_vaxHest_fitted_results.Rdata')
    
    parameters = SA_vaxHest_fitted_results[[1]] %>% filter(country == setting)
    fitted_next_state = SA_vaxHest_fitted_results[[2]]  %>% filter(country == setting)
    fitted_incidence_log_tidy = SA_vaxHest_fitted_results[[3]]  %>% filter(country == setting)
    fitted_incidence_log = SA_vaxHest_fitted_results[[4]]  %>% filter(country == setting)
    rm(SA_vaxHest_fitted_results)
    
    fitted_incidence_log_tidy = fitted_incidence_log_tidy %>% filter(date <= date_start) # CHECKED last of fitted log = first of new log
    fitted_incidence_log = fitted_incidence_log %>% filter(date <= date_start)
  } 
  
  if ( debug == "on" | fitting_details == "on"){
    Reff_tracker = data.frame()
    rho_tracker_dataframe = data.frame()
    VE_tracker_dataframe = data.frame()
  }
  #__________________________________________________________________
  
  
  
  #       (2/4) User choice / Model toggles              
  ####################################################################
  rootpath = str_replace(getwd(), "GitHub_vaxAllocation","") #Note: x_results not stored within GitHub repository
  complete_model_runs = 1   # when >1 samples randomly from distribution of parameters (where available)
  #__________________________________________________________________
  
  
  
  #       (3/4) Run model            
  #####################################################################
  ##(A) Initialise setting 
  if (complete_model_runs == 1){run_type="point"
  } else if (complete_model_runs > 1){run_type="rand"}
  
  if (risk_group_toggle == "on"){ num_risk_groups = 2
  } else{ num_risk_groups = 1; vax_risk_strategy_toggle = "off"}
  
  if (exists("ticket") == FALSE){ ticket = 1 }
  if (exists("prev_setting") == FALSE){ prev_setting = "NONE"}
  if (exists("prev_risk_num") == FALSE){ prev_risk_num = "NONE"}
  if (exists("prev_risk_group") == FALSE){ prev_risk_group = "NONE"}
  if (exists("risk_group_name") == FALSE){ risk_group_name = "NO RISK GROUPS"}
  if (exists("prev_run_date") == FALSE){ prev_run_date = as.Date('1900-01-01')}
  
  if (setting != prev_setting | num_risk_groups != prev_risk_num | risk_group_name != prev_risk_group | prev_run_date != Sys.Date()){
    source(paste(getwd(),"/(1)_simulate_setting.R",sep="")) #load setting stats if new setting
  } 
  
  prev_setting = setting
  prev_run_date = Sys.Date()
  prev_risk_num = num_risk_groups 
  prev_risk_group = risk_group_name
  
  
  ##(B) Load functions
  source(paste(getwd(),"/04_functions/(function)_COVID_ODE.R",sep=""))
  source(paste(getwd(),"/04_functions/(function)_VE_time_step.R",sep=""))
  source(paste(getwd(),"/04_functions/(function)_rho_time_step.R",sep=""))
  source(paste(getwd(),"/04_functions/(function)_vax_strategies.R",sep=""))
  source(paste(getwd(),"/04_functions/(function)_vax_strategies_risk.R",sep=""))
  if (exists("VE_estimates_imputed") == FALSE){load(file='01_inputs/VE_estimates_imputed.Rdata')}
  
  
  ##(C) Run the model!
  incidence_log_tracker=data.frame()
  for (run_number in 1:complete_model_runs){
    source(paste(getwd(),"/(3)_disease_characteristics.R",sep=""))
    source(paste(getwd(),"/(2)_inital_state.R",sep=""))
    source(paste(getwd(),"/(4)_time_step.R",sep=""))
    if (fitting == "off"){
      source(paste(getwd(),"/(5)_severe_outcomes_calc.R",sep="")) # COMEBACK - should this just save its results somewhere?
      incidence_log_tracker <-rbind(incidence_log_tracker,incidence_log[,c('daily_cases','date')])
      source(paste(getwd(),"/(6)_severe_outcome_proj.R",sep=""))
    }
  }

return()
}


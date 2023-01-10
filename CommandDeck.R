### The 'CommandDeck' runs all sub-scripts of the COVID-19 transmission model to
### complete one standard 'run' of the disease model.



#       (1/4) Setup                 
####################################################################
#load libraries
library(readr)
library(deSolve)
library(rvest)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(tidyverse)

debug = "off"
debug_type = "partial" #options: "full", "partial"


### set default values of toggles if debug is on
if (exists("fitting") == FALSE){fitting = "off"}
if (exists("fitting_details") == FALSE){fitting_details = "off"}# Reff tracking, VE tracking, rho tracking
if (fitting == "on"){debug = "off"} # can not debug while fitting the model
if ( debug == "on"){
  
  warning('Debugging is on')
  if (debug_type == "full"){rm(list=ls());debug = "on"}  # clear global environment
  plotting = "on"
  
  ## options for run from fit with omicron onwards
  outbreak_timing = "off"
  strain_inital = strain_now = 'omicron'             
  model_weeks = 5          
  #find latest model run in known dates
  list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("fitted_results_",setting,"*",sep=""))
  list_poss_Rdata_details = double()
  for (i in 1:length(list_poss_Rdata)){
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  date_start = as.Date(file.info(paste("1_inputs/fit/",latest_file,sep=''))$mtime) 
  
  ##options for run from start
  # date_start = as.Date('2021-03-31')
  # strain_inital = strain_now = 'WT'            
  # seed_date = c(as.Date('2021-04-25'),as.Date('2021-11-07'),) #first is seed date for delta, second is omicron
  # model_weeks = as.numeric((ceiling(Sys.Date()-date_start)/7))+52
  
  
  setting = "SLE"
  RR_estimate  = 2
  vax_strategy_toggle = "off"
  vax_risk_strategy_toggle = "off"
  risk_group_toggle = "on" 
  risk_group_name = "adults_with_comorbidities" #options: pregnant_women, adults_with_comorbidities
  risk_group_prioritisation_to_date = NA
  default_prioritisation_proportion = 0.5
  risk_group_lower_cov_ratio = NA
  sensitivity_analysis_toggles = list()
  
  vax_strategy_toggles =
    list(vax_strategy_start_date        = date_start+30,
         vax_strategy_num_doses         = as.integer(1642011),
         vax_strategy_roll_out_speed    = 11075 ,                           # doses delivered per day
         vax_delivery_group             = 'universal',
         vax_age_strategy               = "uniform_no_children",            # options: "oldest", "youngest","50_down","uniform"
         vax_dose_strategy              = 1,                                # options: 1,2
         vax_strategy_vaccine_type      = "Johnson & Johnson" ,             # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
         vax_strategy_vaccine_interval  = c(30*3) ,                         # (days) interval between doses, you must specify multiple intervals if multiple doses e.g. c(21,90)
         vax_strategy_max_expected_cov  = 0.88                              # value between 0-1 of age group willing to be vaccinated
    )
  
  apply_risk_strategy_toggles = list(
    vax_risk_strategy = 'Y',             # options: 'Y','N'
    vax_risk_proportion = 0.8,           # value between 0-1 (equivalent to %) of doses prioritised to the at risk group
    vax_doses_general = 1,               # number of doses delivered to general pop
    vax_doses_risk = 2                   # number of doses delivered to risk group
  )
  
  waning_toggle_acqusition = TRUE
  waning_toggle_severe_outcome = FALSE # save some time, no need to accurate gauge severe outcomes when debugging model
  waning_toggle_rho_acqusition = TRUE
}

### load refit, or refit if not recent enough
if (fitting == "on"){
  warning('Fitting is on')
} else if ( ! 'vax_hesistancy_risk_group' %in% names(sensitivity_analysis_toggles)){

    #load latest model run in known dates
    list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("fitted_results_",setting,"*",sep=""))
    list_poss_Rdata_details = double()
    for (i in 1:length(list_poss_Rdata)){
      list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                      file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
    }
    latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
    load(paste(rootpath,'x_results/',latest_file,sep=''))
    #___________________________________
    
    if('additional_doses' %in% names(sensitivity_analysis_toggles)){
      if (sensitivity_analysis_toggles$additional_doses == 'start_2022'){
        load(file = '1_inputs/fitted_results_SA_2022.Rdata')
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
    
    load(file = '1_inputs/SA_vaxHest_fitted_results.Rdata')
       
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
source(paste(getwd(),"/(function)_COVID_ODE.R",sep=""),local=TRUE)
source(paste(getwd(),"/(function)_VE_time_step.R",sep=""),local=TRUE)
source(paste(getwd(),"/(function)_rho_time_step.R",sep=""),local=TRUE)
source(paste(getwd(),"/(function)_vax_strategies.R",sep=""),local=TRUE)
source(paste(getwd(),"/(function)_vax_strategies_risk.R",sep=""),local=TRUE)
if (exists("VE_estimates_imputed") == FALSE){load(file='1_inputs/VE_estimates_imputed.Rdata')}


##(C) Run the model!
incidence_log_tracker=data.frame()
for (run_number in 1:complete_model_runs){
  source(paste(getwd(),"/(3)_disease_characteristics.R",sep=""),local=TRUE)
  source(paste(getwd(),"/(2)_inital_state.R",sep=""),local=TRUE)
  source(paste(getwd(),"/(4)_time_step.R",sep=""),local=TRUE)
  if (fitting == "off"){
    source(paste(getwd(),"/(5)_severe_outcomes_calc.R",sep=""),local=TRUE) # COMEBACK - should this just save its results somewhere?
    incidence_log_tracker <-rbind(incidence_log_tracker,incidence_log[,c('daily_cases','date')])
    source(paste(getwd(),"/(6)_severe_outcome_proj.R",sep=""),local=TRUE)
  }
}

if (complete_model_runs>1){
  summary_over_runs <- 
    incidence_log_tracker %>%
    group_by(date) %>%
    dplyr::summarise(average_daily_cases = mean(daily_cases), 
                     UCI = CI(daily_cases)[1], 
                     LCI = CI(daily_cases)[3]) 
}
rm(incidence_log_tracker)
#__________________________________________________________________


#       (4/4) Basic plots            
#####################################################################
plot_standard = theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

if (exists("plotting") == FALSE){plotting = "off"}  
if (ticket == 1 | plotting == "on"){

incidence_log_plot = incidence_log %>% filter(date >= date_start) %>%
  mutate(           cumulative_incidence = cumsum(daily_cases),
                    cumulative_incidence_percentage = 100*cumsum(daily_cases)/sum(pop))
  
plot1 <- ggplot() + 
  geom_line(data=incidence_log_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("daily cases") +
  ylim(0,150000)+
  plot_standard

plot2 <- ggplot() + 
  geom_line(data=incidence_log_plot,aes(x=date,y=cumulative_incidence),na.rm=TRUE) +
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("cumulative cases") +
  plot_standard

grid.arrange(plot1, plot2, nrow=2)
}

if (debug == "on" | fitting_details == "on"){
  #number as % of whole population
  lay <- rbind(c(1,2),c(3,3))
  plot1 <- 
    ggplot() + 
    geom_line(data=incidence_log_plot,aes(x=date,y=rolling_average_percentage),na.rm=TRUE) +
    xlab("") + #ylim(0,1.0)+ 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    ylab("daily cases % whole pop") +
    plot_standard
  
  plot2 <- ggplot() + 
    geom_line(data=Reff_tracker,aes(x=date,y=Reff),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    #ylim(0,6) +
    ylab("Reff") +
    plot_standard
  
  plot3<- ggplot() + 
    geom_line(data=incidence_log_plot,aes(x=date,y=cumulative_incidence_percentage),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    ylab("cumulative cases % whole pop") +
    plot_standard
  
  grid.arrange(plot1, plot2, plot3, layout_matrix = lay)

  plot4 = ggplot(rho_tracker_dataframe) + geom_line(aes(x=date,y=rho))
  plot5 = ggplot(VE_tracker_dataframe) + geom_line(aes(x=date,y=VE,color=as.factor(dose)))
  lay <- rbind(c(1,2),c(3,3),c(4,5))
  
  grid.arrange(plot1,plot2,plot3,plot4,plot5, layout_matrix = lay)
} 
#__________________________________________________________________ 



### This program runs all of the sub-scripts of the COVID-19 transmission model
### It is intended to complete one standard 'run'/scenario of the disease model



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
#_________________________________________________________________



####DEBUG
if (fitting == "on"){debug = "off"}
if ( debug == "on"){
  warning('Debugging is on')
  
  if (debug_type == "full"){rm(list=ls());debug = "on"}  # clear global environment
  
  setting = "SLE"
  
  
  ## options for run from fit with omicron onwards
  outbreak_timing = "off"
  strain_inital = strain_now = 'omicron'             #options:'WT','delta','omicron'
  load(file = '1_inputs/last_fit_date.Rdata')
  date_start = fitted_max_date ##latest fit date
  model_weeks = 52          # how many weeks should the model run for?
  
  
  ##options for run from start
  # date_start = as.Date('2021-03-31')
  # strain_inital = strain_now = 'WT'             #options:'WT','delta','omicron'
  # seed_date = c(as.Date('2021-04-25'),as.Date('2021-11-07'),) #first is seed date for delta, second is omicron
  # model_weeks = as.numeric((ceiling(Sys.Date()-date_start)/7))+52
  
  
  plotting = "on"
  

  RR_estimate = RR_default = 2
  vax_strategy_toggle = "off"
  vax_risk_strategy_toggle = "off"
  risk_group_toggle = "on" 
  risk_group_name = "pregnant_women" #options: pregnant_women, adults_with_comorbidities
  risk_group_prioritisation_to_date = NA
  default_prioritisation_proportion = 0.5
  VE_at_risk_suppress = 1 #i.e. do not suppress VE at risk
  
  vax_strategy_toggles =
    list(vax_strategy_start_date        = date_start,
         vax_strategy_num_doses         = as.integer(1642011),
         vax_strategy_roll_out_speed    = 11075 ,               # doses delivered per day
         vax_delivery_group             = 'universal',
         vax_age_strategy               = "uniform_no_children",            # options: "oldest", "youngest","50_down","uniform", OTHER?
         vax_dose_strategy              = 1,                    # options: 1,2
         vax_strategy_vaccine_type      = "Johnson & Johnson" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
         vax_strategy_vaccine_interval  = c(30*3) ,                 # (days) interval between doses, you must specify multiple intervals if multiple doses e.g. c(21,90)
         vax_strategy_max_expected_cov  = 0.88                   # value between 0-1 of age group willing to be vaccinated (vaccine hesitancy est in discussion)
    )
  
  apply_risk_strategy_toggles = list(
    vax_risk_strategy = 'Y',             # options: 'Y','N'
    vax_risk_proportion = 0.8,           # value between 0-1 (equivalent to %) of doses prioritised to the at risk group
    vax_doses_general = 1,               # number of doses delivered to general pop
    vax_doses_risk = 2                  # number of doses delivered to risk group
  )
  
  waning_toggle_acqusition = TRUE
  waning_toggle_severe_outcome = FALSE
  waning_toggle_rho_acqusition = TRUE
  rho_severe_disease = "on"
}
if (fitting == "on"){
  warning('Fitting is on')
  if (debug == "on"){debug = "off"}
} else{
  load(file = '1_inputs/last_fit_date.Rdata')
  if (as.numeric(abs(fitted_max_date - Sys.Date()))>30){ 
    warning('refitted')
    source(paste(getwd(),"/(function)_fitting_model.R",sep=""))
  } else{
    load(file = '1_inputs/fitted_results.Rdata')
    if (risk_group_toggle == "off"){
      loaded_fit = fitted_results[[1]]
    } else if (risk_group_name == 'pregnant_women'){
      loaded_fit = fitted_results[[2]]
    } else if (risk_group_name == 'adults_with_comorbidities'){
      loaded_fit = fitted_results[[3]]
    }
    if (risk_group_toggle == "on"){if(!loaded_fit[[5]] == risk_group_name){stop('risk group name != fitted risk group name')}}
    
    parameters = loaded_fit[[1]]
    fitted_next_state = loaded_fit[[2]]
    
    fitted_incidence_log_tidy = loaded_fit[[3]]
    fitted_incidence_log = loaded_fit[[4]]
    fitted_incidence_log_tidy = fitted_incidence_log_tidy %>% filter(date <= date_start) # CHECKED last of fitted log = first of new log
    fitted_incidence_log = fitted_incidence_log %>% filter(date <= date_start)
    
    if ((is.na(risk_group_prioritisation_to_date) == FALSE) | (! default_prioritisation_proportion == 0.5) | (!VE_at_risk_suppress == 1)){
      stop('change fitting to this change in risk group characteristic')
    }
  }
}
if ( debug == "on" | fitting == "on"){
  Reff_tracker = data.frame()
  rho_tracker_dataframe = data.frame()
  VE_tracker_dataframe = data.frame()
}


#       (2/4) User choice / Model toggles              
####################################################################
if (Sys.info()[['user']] == 'u6044061'){ rootpath = 'C:/Users/u6044061/Documents/PhD/Research/2_scarce_COVID_vaccine_supply/4_code/'
}else if (Sys.info()[['user']] == 'gizem'){ rootpath = 'C:/Users/gizem/Documents/PhD/Research/2_scarce_COVID_vaccine_supply/4_code/'}

complete_model_runs = 1   # when >1 samples randomly from distribution of parameters (where available)

NPI_outbreak_toggle = "delta_peaks"   #options: final, delta_peaks
underascertainment_est = 43

discounting_rate = 0 #discounting on YLL
#__________________________________________________________________



#       (3/4) Run model            
# ####################################################################
##(A) Simulate setting 
# time saving tactics! Load setting if not yet loaded
if (complete_model_runs == 1){run_type="point"
} else if (complete_model_runs > 1){run_type="rand"}

if (risk_group_toggle == "on"){
  num_risk_groups = 2
} else{ num_risk_groups = 1; vax_risk_strategy_toggle = "off"}

num_disease_classes = 4                                 # SEIR 

#load setting stats if new setting
if (exists("prev_setting") == FALSE){ prev_setting = "NONE"}
if (exists("prev_risk_num") == FALSE){ prev_risk_num = "NONE"}
if (exists("prev_risk_group") == FALSE){ prev_risk_group = "NONE"}
if (exists("prev_run_date") == FALSE){ prev_run_date = as.Date('1900-01-01')}
if (setting != prev_setting | num_risk_groups != prev_risk_num | risk_group_name != prev_risk_group | prev_run_date != Sys.Date()){
  source(paste(getwd(),"/(1)_simulate_setting.R",sep=""))
} 
prev_setting = setting
prev_run_date = Sys.Date()
prev_risk_num = num_risk_groups 
prev_risk_group = risk_group_name

seed = 0.001*sum(pop)

if (exists("prev_discounting_rate") == FALSE){ prev_discounting_rate = discounting_rate}
if (prev_discounting_rate != discounting_rate){stop('need to re-run "(mech shop) severe outcome setting-specific rates" to apply new discounting rate')}

#making some interim variables to assist with configuring states
num_vax_doses = D = length(unique(vaccination_history_TRUE$dose))  # dose 1, dose 2, COMEBACK no boosters yet in these settings 
vax_type_list = sort(unique(vaccination_history_TRUE$vaccine_type))
num_vax_types = T = length(unique(vaccination_history_TRUE$vaccine_type))
num_vax_classes = num_vax_doses*num_vax_types + 1                 # + 1 for unvaccinated

##(B) Load functions
source(paste(getwd(),"/(function)_COVID_ODE.R",sep=""))
source(paste(getwd(),"/(function)_VE_time_step.R",sep=""))
source(paste(getwd(),"/(function)_rho_time_step.R",sep=""))
source(paste(getwd(),"/(function)_vax_strategies.R",sep=""))
source(paste(getwd(),"/(function)_vax_strategies_risk.R",sep=""))

if (exists("VE_estimates_imputed") == FALSE){source(paste(getwd(),"/(mech shop) VE point estimate.R",sep=""))}


##(C) Run the model!
time.start.CommandDeck=proc.time()[[3]] #let's see how long this runs for

incidence_log_tracker=data.frame()
for (run_number in 1:complete_model_runs){
  source(paste(getwd(),"/(3)_disease_characteristics.R",sep=""))
  source(paste(getwd(),"/(2)_inital_state.R",sep=""))
  source(paste(getwd(),"/(4)_time_step.R",sep=""))
  source(paste(getwd(),"/(once)_severe_outcomes_calc.R",sep="")) # COMEBACK - should this just save its results somewhere?
  incidence_log_tracker <-rbind(incidence_log_tracker,incidence_log[,c('daily_cases','date')])
  source(paste(getwd(),"/(function)_severe_outcome_proj.R",sep=""))
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
# ####################################################################
# NOTE: more advanced plots in scripts title '(plot)_...'
if (exists("ticket") == FALSE){ ticket = 1 }
if (ticket == 1 | plotting == "on"){

  incidence_log_plot = incidence_log %>% filter(date >= date_start) %>%
    mutate(           cumulative_incidence = cumsum(daily_cases),
                      cumulative_incidence_percentage = 100*cumsum(daily_cases)/sum(pop))
  
#raw number - daily and cumulative
plot1 <- ggplot() + 
  geom_line(data=incidence_log_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log_plot$date),],
             aes(x=date,y=rolling_average*underascertainment_est),na.rm=TRUE) + 
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("daily cases") +
  ylim(0,150000)+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

plot2 <- ggplot() + 
  geom_line(data=incidence_log_plot,aes(x=date,y=cumulative_incidence),na.rm=TRUE) +
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("cumulative cases") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

grid.arrange(plot1, plot2, nrow=2)
}

if (debug == "on" | fitting == "on"){
  #number as % of whole population
  lay <- rbind(c(1,2),c(3,3))
  plot1 <- 
    ggplot() + 
    geom_line(data=incidence_log_plot,aes(x=date,y=rolling_average_percentage),na.rm=TRUE) +
    geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log_plot$date),],
               aes(x=date,y=rolling_average*5000*underascertainment_est/sum(pop)),na.rm=TRUE) + 
    xlab("") + #ylim(0,1.0)+ 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    ylab("daily cases % whole pop") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black')) 
  
  plot2 <- ggplot() + 
    geom_line(data=Reff_tracker,aes(x=date,y=Reff),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    #ylim(0,6) +
    ylab("Reff") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  
  plot3<- ggplot() + 
    geom_line(data=incidence_log_plot,aes(x=date,y=cumulative_incidence_percentage),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    ylab("cumulative cases % whole pop") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  
  grid.arrange(plot1, plot2, plot3, layout_matrix = lay)

  plot4 = ggplot(rho_tracker_dataframe) + geom_line(aes(x=date,y=rho))
  plot5 = ggplot(VE_tracker_dataframe) + geom_line(aes(x=date,y=VE,color=as.factor(dose)))
  lay <- rbind(c(1,2),c(3,3),c(4,5))
  grid.arrange(plot1,plot2,plot3,plot4,plot5, layout_matrix = lay)
} 



#either incidence per 100,000 or % of total population
#__________________________________________________________________ 


time.end.CommandDeck=proc.time()[[3]]
time.end.CommandDeck-time.start.CommandDeck
## current runtime (19/05) 8 minutes for 52 weeks with 2 risk groups, 6 mins with 1 risk - COMEBACK - time!

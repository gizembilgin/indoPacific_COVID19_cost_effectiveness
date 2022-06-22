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
#rm(list=ls())  # clear global environment

if (Sys.info()[['user']] == 'u6044061'){ rootpath = 'C:/Users/u6044061/Documents/PhD/Research/2_scarce_COVID_vaccine_supply/4_code/'
}else if (Sys.info()[['user']] == 'gizem'){ rootpath = 'C:/Users/gizem/Documents/PhD/Research/2_scarce_COVID_vaccine_supply/4_code/'}
#_________________________________________________________________



#       (2/4) User choice / Model toggles              
####################################################################
setting = "SLE"

baseline_date_start = as.Date('2022-07-01')

if(outbreak_post_rollout == "on"){
  date_start = max(vaccination_history_FINAL$date)
  seed_date = baseline_date_start 
}else if(outbreak_post_rollout == "off"){ #i.e. rolling out vaccine during outbreak
  date_start = baseline_date_start
  seed_date = baseline_date_start
}

strain_inital = 'omicron'             #options:'WT','delta','omicron'
model_weeks = 52          # how many weeks should the model run for?
complete_model_runs = 1   # when >1 samples randomly from distribution of parameters (where available)

#vax_strategy_toggle = "on"
# vax_strategy_toggles =
#   list(vax_strategy_start_date                  = as.Date('2022-08-20'),
#        vax_strategy_num_doses         = 2090592,
#        vax_strategy_roll_out_speed    = 11075 ,               # doses delivered per day
#        vax_delivery_group             = 'universal',
#        vax_age_strategy               = "uniform_no_children",            # options: "oldest", "youngest","50_down","uniform", OTHER?
#        vax_dose_strategy              = 1,                    # options: 1,2
#        vax_strategy_vaccine_type      = "Johnson & Johnson" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
#        vax_strategy_vaccine_interval  = 7*3 ,                 # (days) interval between first and second dose
#        vax_strategy_max_expected_cov  = 0.88                   # value between 0-1 of age group willing to be vaccinated (vaccine hesitancy est in discussion)
#   )

#RR_estimate = 2
#risk_group_toggle = "off"
#risk_group_name = "adults_with_comorbidities" #options: pregnant_women, adults_with_comorbidities
#vax_risk_strategy_toggle = "off"

#waning_toggle_rho_acqusition = TRUE
#rho_severe_disease = "on"


NPI_outbreak_toggle = "delta_peaks"   #options: final, delta_peaks
underascertainment_est = 43

behaviour_mod = 0  #0.268 if start 01/03/21
uniform_mod=1

discounting_rate = 0 #discounting on YLL
#__________________________________________________________________



#       (3/4) Run model            
# ####################################################################
##(A) Simulate setting 
# time saving tactics! Load setting if not yet loaded
if (complete_model_runs == 1){run_type="point"
} else if (complete_model_runs > 1){run_type="rand"}
if (setting == "PNG"){setting_long = "Papua New Guinea"
} else if (setting == "SLE"){setting_long = "Sierra Leone"}

if (risk_group_toggle == "on"){
  num_risk_groups = 2
} else{ num_risk_groups = 1; vax_risk_strategy_toggle = "off"}

num_disease_classes = 4                                 # SEIR 

if (exists("prev_setting") == FALSE){ prev_setting = "NONE"}
if (exists("prev_risk_num") == FALSE){ prev_risk_num = "NONE"}
if (exists("prev_risk_group") == FALSE){ prev_risk_group = "NONE"}
if (setting != prev_setting | num_risk_groups != prev_risk_num | risk_group_name != prev_risk_group){source(paste(getwd(),"/(1)_simulate_setting.R",sep=""))} #load setting stats if new setting
prev_setting = setting
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

#__________________________________________________________________


#       (4/4) Basic plots            
# ####################################################################
# NOTE: more advanced plots in scripts title '(plot)_...'
if (ticket ==1){

#raw number - daily and cumulative
plot1 <- ggplot() + 
  geom_line(data=incidence_log,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average*underascertainment_est),na.rm=TRUE) + 
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("daily cases") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

plot2 <- ggplot() + 
  geom_line(data=incidence_log,aes(x=date,y=cumulative_incidence),na.rm=TRUE) +
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("cumulative cases") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

#grid.arrange(plot1, plot2, nrow=2)


#number as % of whole population
lay <- rbind(c(1,2),c(3,3))
plot1 <- 
  ggplot() + 
  geom_line(data=incidence_log,aes(x=date,y=rolling_average_percentage),na.rm=TRUE) +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average*100*underascertainment_est/sum(pop)),na.rm=TRUE) + 
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("daily cases % whole pop") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

plot2 <- ggplot() + 
  geom_line(data=incidence_log,aes(x=date,y=Reff),na.rm=TRUE) +
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
  geom_line(data=incidence_log,aes(x=date,y=cumulative_incidence_percentage),na.rm=TRUE) +
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("cumulative cases % whole pop") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))


if (debug == "on"){
  plot4 = ggplot(rho_tracker_dataframe) + geom_line(aes(x=date,y=rho))
  plot5 = ggplot(VE_tracker_dataframe) + geom_line(aes(x=date,y=VE,color=as.factor(dose)))
  lay <- rbind(c(1,2),c(3,3),c(4,5))
  grid.arrange(plot1, plot2, plot3,plot4,plot5, layout_matrix = lay)
} else{
  grid.arrange(plot1, plot2, plot3, layout_matrix = lay)
}

}
#either incidence per 100,000 or % of total population
#__________________________________________________________________ 


time.end.CommandDeck=proc.time()[[3]]
time.end.CommandDeck-time.start.CommandDeck
## current runtime (19/05) 8 minutes for 52 weeks with 2 risk groups, 6 mins with 1 risk

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

#rm(list=ls())  # clear global environment

if (Sys.info()[['user']] == 'u6044061'){ rootpath = 'C:/Users/u6044061/Documents/PhD/Research/2_scarce_COVID_vaccine_supply/4_code/'
}else if (Sys.info()[['user']] == 'gizem'){ rootpath = 'C:/Users/gizem/Documents/PhD/Research/2_scarce_COVID_vaccine_supply/4_code/'}
#_________________________________________________________________



#       (2/4) User choice / Model toggles              
####################################################################
setting = "SLE"

if(outbreak_post_rollout == "on"){
  date_start = max(vaccination_history_FINAL$date)
  seed_date = date_start 
}else if(outbreak_post_rollout == "off"){ #i.e. rolling out vaccine during outbreak
  date_start = as.Date('2022-08-15')
  seed_date = date_start
}

strain_inital = 'omicron'             #options:'WT','delta','omicron'
model_weeks = 52          # how many weeks should the model run for?
complete_model_runs = 1   # when >1 samples randomly from distribution of parameters (where available)

vax_strategy_toggle = "on"

risk_group_toggle = "on"
risk_group_name = "pregnant_women" #options: pregnant_women, adults_with_comorbidities
vax_risk_strategy_toggle = "on"


NPI_outbreak_toggle = "delta_peaks"   #options: final, delta_peaks
underascertainment_est = 43

behaviour_mod = 0  #0.268 if start 01/03/21
uniform_mod=1


#vax_strategy_toggle = "off" #included in (plot)_vax_strategies
#__________________________________________________________________



#       (3/4) Run model            
# ####################################################################
##(A) Load functions
source(paste(getwd(),"/(function)_COVID_ODE.R",sep=""))
source(paste(getwd(),"/(function)_VE_time_step.R",sep=""))
source(paste(getwd(),"/(function)_vax_strategies.R",sep=""))
source(paste(getwd(),"/(function)_vax_strategies_risk.R",sep=""))


##(B) Simulate setting 
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
if (setting != prev_setting | num_risk_groups != prev_risk_num){source(paste(getwd(),"/(1)_simulate_setting.R",sep=""))} #load setting stats if new setting
prev_setting = setting
prev_risk_num = num_risk_groups 
seed = 0.001*sum(pop)

#making some interim variables to assist with configuring states
num_vax_doses = D = length(unique(vaccination_history_TRUE$dose))  # dose 1, dose 2, COMEBACK no boosters yet in these settings 
vax_type_list = sort(unique(vaccination_history_TRUE$vaccine_type))
num_vax_types = T = length(unique(vaccination_history_TRUE$vaccine_type))
num_vax_classes = num_vax_doses*num_vax_types + 1                 # + 1 for unvaccinated
num_total_classes = (num_disease_classes+1)*(num_age_groups*num_vax_classes)*num_risk_groups #+1 for incidence tracker



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

grid.arrange(plot1, plot2, plot3, layout_matrix = lay)
}
#either incidence per 100,000 or % of total population
#__________________________________________________________________ 


time.end.CommandDeck=proc.time()[[3]]
time.end.CommandDeck-time.start.CommandDeck
## current runtime (19/05) 8 minutes for 52 weeks with 2 risk groups, 6 mins with 1 risk

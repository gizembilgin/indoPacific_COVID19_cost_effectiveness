### This program runs all of the sub-scripts of the COVID-19 transmission model



#       (1/4) Setup                 
####################################################################
#load libraries
library(tidyverse)
library(readr)
library(deSolve)
library(rvest)
library(ggplot2)
library(gridExtra)

#rm(list=ls())                                                               # clear global environment
#setwd("~/PhD/Research/2_scarce_COVID_vaccine_supply/4_code/v2021_12_20_X")  # set working directory
#_________________________________________________________________



#       (2/4) User choice / Model toggles              
####################################################################
setting = "SLE"

date_start = as.Date('2022-04-01')  #note, '2020-03-01' used for current WT behaviour_mod fit (02/02/2022); and '2022-04-01 for simulations
model_weeks = 8          # how many weeks should the model run for?, 24 PNG fit
complete_model_runs = 1   # when >1 samples randomly from distribution of parameters (where available)

strain_inital = 'omicron'             #options:'WT','delta'
seed = 50/100000 


NPI_outbreak_toggle = "delta_peaks"   #options: final, delta_peaks

underascertainment_est = 43

behaviour_mod = 0  #0.268 if start 01/03
uniform_mod=1

#vaccine_strategy_toggle = , read in vaccine_strategy.csv
#__________________________________________________________________



#       (3/4) Run model            
# ####################################################################
##(A) Load functions
source(paste(getwd(),"/(function)_COVID_ODE.R",sep=""))
source(paste(getwd(),"/(function)_VE_time_step.R",sep=""))


##(B) Simulate setting 
# time saving tactics! Load setting if not yet loaded
if (setting == "PNG"){setting_long = "Papua New Guinea"
} else if (setting == "SLE"){setting_long = "Sierra Leone"}

if (exists("prev_setting") == FALSE){ prev_setting = "NONE"}
if (setting != prev_setting){source(paste(getwd(),"/(1)_simulate_setting.R",sep=""))} #load setting stats if new setting
prev_setting = setting                             

#making some interim variables to assist with configuring states
seed = sum(pop)*seed
num_disease_classes = 4                                 # SEIR 
num_age_groups = J = length(age_group_labels)           # 0-4,5-11,12-15,16-29,30-59,60+
num_vax_doses = D = length(unique(vaccination_history_FINAL$dose))  # dose 1, dose 2, COMEBACK no boosters yet in these settings 
vax_type_list = sort(unique(vaccination_history_FINAL$vaccine_type))
num_vax_types = T = length(unique(vaccination_history_FINAL$vaccine_type))
num_vax_classes = num_vax_doses*num_vax_types + 1                 # + 1 for unvaccinated
num_total_classes = (num_disease_classes+1)*(num_age_groups*num_vax_classes) #+1 for incidence tracker
source(paste(getwd(),"/(2)_inital_state.R",sep=""))


##(C) Run the model!
if (complete_model_runs == 1){run_type="point"
} else if (complete_model_runs > 1){run_type="rand"}

time.start=proc.time()[[3]] #let's see how long this runs for

incidence_log_tracker=data.frame()
for (run_number in 1:complete_model_runs){
  source(paste(getwd(),"/(3)_disease_characteristics.R",sep=""))
  source(paste(getwd(),"/(4)_time_step.R",sep=""))
  incidence_log_tracker <-rbind(incidence_log_tracker,incidence_log[,c('daily_cases','date')])
}

if (complete_model_runs>1){
  summary_over_runs <- 
    incidence_log_tracker %>%
    group_by(date) %>%
    dplyr::summarise(average_daily_cases = mean(daily_cases), 
                     UCI = CI(daily_cases)[1], 
                     LCI = CI(daily_cases)[3]) 
}
time.end=proc.time()[[3]]
time.end-time.start 
#__________________________________________________________________


#       (4/4) Basic plots            
# ####################################################################
# NOTE: more advanced plots in scripts title '(plot)_...'

#raw number - daily and cumulative
plot1 <- ggplot() + 
  geom_line(data=incidence_log,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average*underascertainment_est),na.rm=TRUE) + 
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  #ylim(0,40) +
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
  #ylim(0,40) +
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
  #ylim(0,40) +
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
  #ylim(0,40) +
  ylab("cumulative cases % whole pop") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

grid.arrange(plot1, plot2, plot3, layout_matrix = lay)

#either incidence per 100,000 or % of total population
#__________________________________________________________________ 


## current runtime (19/01) ~ 117 seconds (although concurrently fitting ACT)
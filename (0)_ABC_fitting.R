### This script uses Approximate Bayesian Computation (ABC) to fit the model from the first reported case until today's date {Sys.Date()}.
### Today's date is then used as the latest fitted date of the model and used as date_start in modelling scenarios.
### (We re derive today's date using the file creation data of fitted_results)
###
### Dependencies: nil
### Creates: fitted_results_<setting>

#clear the field!
rm(list=ls())

this_setting = setting = "FJI"



### Setup __________________________________________________________________________________________________
#general toggles
fitting = "on"
plotting = "off"; ticket = 0
outbreak_timing = "off" #i.e., no new outbreak if =="after" than new VOC after last vaccine delivery date, if == 'during" new VOC introduced one week from now
vax_strategy_toggle = "off" #no additional vax, use real vax data only
vax_risk_strategy_toggle = "off"
sensitivity_analysis_toggles = list()
waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = FALSE #let's save some time, this is not used in the modelling scenarios
waning_toggle_rho_acqusition = TRUE

#initial search range of seed dates
if (this_setting == "FJI"){
  date_start = as.Date('2021-04-30')
  strain_inital = strain_now = 'WT' 
  
  covid19_waves = baseline_covid19_waves = data.frame(date = #c(as.Date('2021-06-06'),as.Date('2021-10-21'),as.Date('2022-01-15')), # initial best guess!
                             c(as.Date('2021-06-09'),as.Date('2021-10-15'),as.Date('2022-02-01')), # previous best guess
                     strain = c('delta','omicron','omicron'))
} else if (this_setting == "PNG"){

  strain_inital = strain_now = 'WT'
  
  baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-01-15'),as.Date('2021-09-01'),as.Date('2021-12-01')),
                             strain = c('WT','delta','omicron'))
  
  date_start = covid19_waves$date[1] - 2
} else if (this_setting == "TLS") {
  strain_inital = strain_now = 'WT'
  baseline_covid19_waves = covid19_waves = data.frame(
    date = c(as.Date('2021-03-01'),as.Date('2021-05-01'),as.Date('2022-01-01')),
    strain = c('WT', 'delta', 'omicron'))
  
  date_start = covid19_waves$date[1] - 2
  
} else if (this_setting == "IDN") {
  strain_inital = strain_now = 'WT'
  
  baseline_covid19_waves = covid19_waves = data.frame(
    date = c( as.Date('2020-12-01'), as.Date('2021-06-01'), as.Date('2022-02-01')),
    strain = c('WT', 'delta', 'omicron'))
  
  date_start = covid19_waves$date[1] - 2
}
model_weeks = as.numeric((Sys.Date()+1-date_start)/7)

#plot standard
plot_standard = theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'))

#risk group toggles
risk_group_prioritisation_to_date = NA
risk_group_lower_cov_ratio = NA
risk_group_toggle = "on"

risk_group_name = 'adults_with_comorbidities'
RR_estimate = 1.95
#______________________________________________________________________________________________________________



### Saving VE for known dates __________________________________________________________________________________
#Load vax history
source(paste(getwd(),"/(function)_VE_time_step.R",sep=""))
source(paste(getwd(),"/(function)_rho_time_step.R",sep=""))
source(paste(getwd(),"/(1)_simulate_setting.R",sep="")) 
source(paste(getwd(),"/(3)_disease_characteristics.R",sep=""))
source(paste(getwd(),"/(2)_inital_state.R",sep=""))

VE_real_range = data.frame()
date_list = seq(max(date_start,min(vaccination_history_TRUE$date)+ min(vaxCovDelay$delay)),
                Sys.Date()+1,
                by="days")

for (i in 1:length(date_list)) {
  #date_now
  date_now = date_list[i]
  
  #(1/3) save VE as all WT-delta estimates
  VE_delta = VE_time_step(
    "delta",
    date_now,
    'any_infection',
    VE_waning_LOCAL = VE_waning_distribution, #loaded from (2)
    vaccination_history_LOCAL = vaccination_history_TRUE, #loaded from (1)
    SA_toggles_local = sensitivity_analysis_toggles) %>%
    mutate(date = date_now,
           strain = "delta")
  
  #(2/3) save VE as all omicron estimates
  VE_omicron = VE_time_step(
    "omicron",
    date_now,
    'any_infection',
    VE_waning_LOCAL = VE_waning_distribution, #loaded from (2)
    vaccination_history_LOCAL = vaccination_history_TRUE, #loaded from (1)
    SA_toggles_local = sensitivity_analysis_toggles) %>%
    mutate(date = date_now,
           strain = "omicron")
  
  #(3/3) within time step of each run calculate as mixed WT-delta/omicron estimates within omicron shift
  
  VE_real_range = rbind(VE_real_range, VE_delta,VE_omicron)
}

rm(VE_time_step)
save(VE_real_range, file = paste('1_inputs/fit/VE_real_range_',this_setting,'_',Sys.Date(),'.Rdata',sep=''))
#______________________________________________________________________________________________________________



### Model run _________________________________________________________________________________________________
#20/12/2022 - have removed Reff/VE/rho tracking using fitting_details parameter; stop VE_time_step for acq (VE_real_range); stop VE_time_step for SO (don't run (5) or (6) when fitting)
#21/12/2022 - current ~20 minute run time for date_start to Sys.Date()

system.time(source(paste(getwd(),"/CommandDeck.R",sep="")))

coeff <- 1/15
ggplot() +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=incidence_log,aes(x=date,y=rolling_average)) + 
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+ 
  plot_standard
#______________________________________________________________________________________________________________



### Fit waves _________________________________________________________________________________________________
#21/12/2022 - abandoned attempt to fit seed dates to wave peaks individually,
#             it appears that the parameter space for fitting three peaks simultaneously is too confusing
#             the Nelder-Mead method kept returning the initial conditions
#             I note here that the timing of the first two peaks is already fairly good, however the second peak appears too large -> impedes a third peak as early as it was reported

#SETUP________________________________________
require('pracma') #contains findpeaks function

#reported_cases remove NA, and within simulation scope
reported_cases = case_history %>% select(date,rolling_average) %>% filter(date > date_start)
reported_cases <- na.omit(reported_cases)

#reported_peaks - peaks in reported daily cases, NB: currently hard coded to FJI
reported_peaks = findpeaks(reported_cases$rolling_average,minpeakheight = 50 ,minpeakdistance = 90, npeaks = 3 )
reported_peaks = reported_cases$date[c(reported_peaks[,2])]
#add third peak
window = reported_cases %>% filter(date>as.Date('2022-05-01'))
third_peak = median(window$date[window$rolling_average == max(window$rolling_average)])
reported_peaks = c(reported_peaks,third_peak)
#reported_peaks #"2021-07-21" "2022-01-21" "2022-07-15"

#fit cutoff dates
if (this_setting == "FJI"){
  fit_cutoff_dates = c(as.Date('2021-10-15'),#earliest likely introduction of Omicron
                       reported_peaks[2] + as.numeric(reported_peaks[3] - reported_peaks[2])/2)  
} else if (this_setting == "PNG"){
  fit_cutoff_dates = c(as.Date('2021-07-05')) #first good introduction of Delta in PNG
}
#_____________________________________________



#FIT FIRST WAVE_______________________________
if (exists("fitting_beta")){rm(fitting_beta)}
if (exists("under_reporting_est")){rm(under_reporting_est)}
if (exists("covid19_waves")){rm(covid19_waves)}

fit_daily_reported_1 <- function(par){
  
  strain_inital = strain_now = 'WT' 
  model_weeks = as.numeric((fit_cutoff_dates[1]-date_start-1)/7)
  covid19_waves = data.frame(date = baseline_covid19_waves$date[1]+round(par[1]),
                             strain = 'delta')
  
  under_reporting_est = par[2]
  fitting_beta= par[3]
  
  source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
  
  workshop = case_history %>% 
    select(date,rolling_average) %>%
    mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
            rolling_average = rolling_average * under_reporting_est) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2)
  
  fit_statistic = sum(workshop$fit_statistic,na.rm=TRUE)
  
  return(fit_statistic)
}

#Attempt One: fit with no bounds
system.time({first_wave_fit = optim(c(3.3917404 , 16.8874017  , 0.9308657),
                                    fit_daily_reported_1,
                       method = "Nelder-Mead")})
# $par
# [1] -3.3917404 16.8874017  0.9308657
# 
# $value
# [1] 239881507
# 
# $counts
# function gradient 
# 156       NA 
# 
# $convergence
# [1] 0
fit_daily_reported_1(first_wave_fit$par)

#Attempt Two: fit bounded
# system.time({first_wave_fit = optim(c(0, 17,1),
#                        fit_daily_reported_1,
#                        method = "L-BFGS-B",
#                        lower = c(-7,10,0.8), upper = c(7,20,1.2))})
# 
# #Attempt Three: fit with different optimisation function
# system.time({first_wave_fit = nlm(fit_daily_reported_1,c(0, 17.9976092,1),
#                      fscale = 278192872, #estimate of the function at the minimum
#                      print.level = 1, #inital and final details are printed
#                      iterlim = 100
#                      )})
#
# Attempt Four: change tol of optim function


to_plot = workshop %>% filter(date>date_start & date<=(date_start+model_weeks*7))
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
  plot_standard
save(first_wave_fit, file = paste('1_inputs/fit/first_wave_fit',this_setting,Sys.Date(),'.Rdata',sep=''))
#______________________________________________________________________________________________________________



#FIT SECOND WAVE_______________________________
if (exists("fitting_beta")){rm(fitting_beta)}
if (exists("under_reporting_est")){rm(under_reporting_est)}
if (exists("covid19_waves")){rm(covid19_waves)}
rm(par)

fit_daily_reported_2 <- function(par){
  
  #Load first wave
  list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("first_wave_fit*",sep=""))
  list_poss_Rdata_details = double()
  for (i in 1:length(list_poss_Rdata)){
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste('1_inputs/fit/',latest_file,sep=''))
  
  strain_inital = strain_now = 'WT' 
  model_weeks =as.numeric((fit_cutoff_dates[2]-date_start)/7)
  covid19_waves = data.frame(date = c(baseline_covid19_waves$date[1]+round(first_wave_fit$par[1]),
                                      baseline_covid19_waves$date[2]+round(par[1])),
                             strain = c('delta','omicron'))
  under_reporting_est = par[2]
  fitting_beta= c(first_wave_fit$par[3],par[3])

  source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
  
  workshop = case_history %>%
    select(date,rolling_average) %>%
    mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
      rolling_average = case_when(
        date > fit_cutoff_dates[1] ~ rolling_average * under_reporting_est,
        date <= fit_cutoff_dates[1] ~ rolling_average * first_wave_fit$par[2])) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2)
  
  fit_statistic = sum(workshop$fit_statistic[workshop$date> fit_cutoff_dates[1]], #fit only after first wave
                      na.rm=TRUE)
  
  return(fit_statistic)
}

system.time({second_wave_fit = optim(c(-11,47,1.04),
                                     fit_daily_reported_2,
                                     method = "Nelder-Mead")})

to_plot = workshop %>% filter(date>date_start & date<=(date_start+model_weeks*7))
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
  plot_standard

save(second_wave_fit, file = paste('1_inputs/fit/second_wave_fit',this_setting,Sys.Date(),'.Rdata',sep=''))
#______________________________________________________________________________________________________________



#FIT THIRD WAVE_______________________________
#### Create start point for wave three fit
fitting = "on"

#Load first wave
list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("first_wave_fit*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste('1_inputs/fit/',latest_file,sep=''))

#Load second wave
list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("second_wave_fit*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste('1_inputs/fit/',latest_file,sep=''))

#configure scenario
strain_inital = strain_now = 'WT' 
baseline_date_start = date_start = as.Date('2021-04-30')
model_weeks = as.numeric(((baseline_covid19_waves$date[3]-28)-date_start)/7)

covid19_waves = data.frame(date = c(baseline_covid19_waves$date[1] + round(first_wave_fit$par[1]),
                                    baseline_covid19_waves$date[2] + round(second_wave_fit$par[1]),
                                    baseline_covid19_waves$date[3]),
                           strain = c('delta','omicron','omicron'))

fitting_beta= c(first_wave_fit$par[3],
                second_wave_fit$par[3],
                1)

source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)

fitted_results = list(
  FR_parameters = parameters,
  FR_next_state = next_state,
  FR_incidence_log_tidy = incidence_log_tidy,
  FR_incidence_log = incidence_log,
  FR_covid19_waves = covid19_waves,
  FR_fitting_beta = fitting_beta
)
save(fitted_results, file = paste("1_inputs/fit/start_point_wave_three_",this_setting,Sys.Date(),".Rdata",sep=''))
#______________________________________________


#### Utilise start point for wave three fit
if (exists("fitting_beta")){rm(fitting_beta)}
if (exists("under_reporting_est")){rm(under_reporting_est)}
if (exists("covid19_waves")){rm(covid19_waves)}
rm(par)

fit_daily_reported_3 <- function(par){
  
  on.exit(.optim <<- list(par = par, obj = print(returnValue())))
  
  #Load first wave
  list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("first_wave_fit*",sep=""))
  list_poss_Rdata_details = double()
  for (i in 1:length(list_poss_Rdata)){
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste('1_inputs/fit/',latest_file,sep=''))
  
  #Load second wave
  list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("second_wave_fit*",sep=""))
  list_poss_Rdata_details = double()
  for (i in 1:length(list_poss_Rdata)){
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste('1_inputs/fit/',latest_file,sep=''))
  
  #configure scenario
  fitting = "wave_three"
  
  date_start = baseline_covid19_waves$date[3]-28-1
  model_weeks = as.numeric((Sys.Date()+1-date_start)/7)
  covid19_waves = data.frame(date = c(baseline_covid19_waves$date[1] + round(first_wave_fit$par[1]),
                                      baseline_covid19_waves$date[2] + round(second_wave_fit$par[1]),
                                      baseline_covid19_waves$date[3] + round(par[1])),
                             strain = c('delta','omicron','omicron'))
  under_reporting_est = par[2]
  fitting_beta= c(first_wave_fit$par[3],
                  second_wave_fit$par[3],
                  par[3])
  
  source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
  
  workshop = case_history %>%
    select(date,rolling_average) %>%
    mutate(
      rolling_average = case_when(
        date > fit_cutoff_dates[2] ~ rolling_average * under_reporting_est,
        date > fit_cutoff_dates[1] ~ rolling_average * second_wave_fit$par[2],
        date <= fit_cutoff_dates[1] ~ rolling_average * first_wave_fit$par[2])) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2)
  
  fit_statistic = sum(workshop$fit_statistic, #fit only after first wave
                      na.rm=TRUE)
  
  return(fit_statistic)
}
.optim <- NULL
#system.time({fit = fit_daily_reported_3(c(0,120,1.2))})
system.time({third_wave_fit = optim(c(0,120,1.2),
                                    fit_daily_reported_3,
                                     method = "Nelder-Mead", 
                                    control = list(trace = TRUE))})

# system.time({third_wave_fit = optim(c(15,120,1.2),
#                                     fit_daily_reported_3,
#                        method = "L-BFGS-B",
#                        lower = c(0,50,1), upper = c(60,250,2))})

to_plot = workshop %>% 
  filter(date>baseline_date_start) %>%
  filter(date>date_start & date<=(date_start+model_weeks*7))
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
  plot_standard

save(third_wave_fit, file = paste('1_inputs/fit/third_wave_fit',this_setting,'.Rdata',sep=''))
#______________________________________________________________________________________________________________




#NEED TO CREATE FITTED_COVID19_WAVES & FR_parameters,FR_next_state,FR_fitted_incidence_log,FR_fitted_incidence_log
### Save ___________________________________________________________________________________________
#fitted_covid19_waves columns: fitted_setting, fitted_date, strain, date
load(file = '1_inputs/fit/fitted_covid19_waves.Rdata')
fitted_covid19_waves = fitted_covid19_waves %>% 
  mutate(fitted_setting = this_setting,
         fitted_date = Sys.Date())
fitted_covid19_waves = rbind(fitted_covid19_waves,this_fit_covid19_waves)
save(fitted_covid19_waves, file = '1_inputs/fit/fitted_covid19_waves.Rdata')
#___________________________________________________________________________


#fitted_results = run with best estimates
load(file = paste('1_inputs/fit/first_wave_fit',this_setting,'.Rdata',sep=''))
load(file = paste('1_inputs/fit/second_wave_fit',this_setting,'.Rdata',sep=''))
load(file = paste('1_inputs/fit/third_wave_fit',this_setting,'.Rdata',sep=''))

strain_inital = strain_now = 'WT' 
model_weeks = as.numeric((Sys.Date()+1-date_start)/7)
covid19_waves = data.frame(date = c(as.Date('2021-06-09')+round(first_wave_fit$par[1]),
                                    as.Date('2021-10-15')+round(par[1]),
                                    as.Date('2022-02-01')),
                           strain = c('delta','omicron','omicron'))
fitting_beta= c(first_wave_fit$par[3],
                par[3],
                1)

source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)

workshop = case_history %>%
  select(date,rolling_average) %>%
  mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
    rolling_average = case_when(
      #COMEBACK - need third wave here
      date > fit_cutoff_dates[1] ~ rolling_average * second_wave_fit$par[2],
      date <= fit_cutoff_dates[1] ~ rolling_average * first_wave_fit$par[2])) %>%
  rename(adjusted_reported = rolling_average) %>%
  left_join(incidence_log, by = "date") %>%
  mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2)

to_plot = workshop %>% filter(date>date_start)
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
  plot_standard
sum(workshop$fit_statistic,na.rm=TRUE)


fitted_results = list(
  FR_parameters = parameters,
  FR_next_state = next_state,
  FR_incidence_log_tidy = incidence_log_tidy,
  FR_incidence_log = incidence_log,
  FR_covid19_waves = covid19_waves,
  FR_fitting_beta = fitting_beta
)
save(fitted_results, file = paste("1_inputs/fitted_results_",this_setting,Sys.Date(),".Rdata"))

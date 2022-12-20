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
plotting = "on"
outbreak_timing = "off" #i.e., no new outbreak if =="after" than new VOC after last vaccine delivery date, if == 'during" new VOC introduced one week from now
vax_strategy_toggle = "off" #no additional vax, use real vax data only
vax_risk_strategy_toggle = "off"
sensitivity_analysis_toggles = list()
waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = FALSE #let's save some time, this is not used in the modelling scenarios
waning_toggle_rho_acqusition = TRUE

#initial search range of seed dates
if (setting == "FJI"){
  date_start = as.Date('2021-04-30')
  strain_inital = strain_now = 'WT' 
  
  covid19_waves = data.frame(date = #c(as.Date('2021-06-06'),as.Date('2021-10-21'),as.Date('2022-01-15')), # initial best guess!
                             c(as.Date('2021-06-06'),as.Date('2021-10-15'),as.Date('2022-02-01')), # previous best guess
                     strain = c('delta','omicron','omicron'))
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
  
  #strain_now
  if (date_now < min(covid19_waves$date[covid19_waves$strain == 'delta'])) {
    strain_now = "WT"
  } else if (date_now >= min(covid19_waves$date[covid19_waves$strain == 'delta']) &
             date_now < min(covid19_waves$date[covid19_waves$strain == 'omicron'])) {
    strain_now = "delta"
  } else if (date_now >= min(covid19_waves$date[covid19_waves$strain == 'omicron'])) {
    strain_now = "omicron"
  }
  
  VE_here = VE_time_step(
    strain_now,
    date_now,
    'any_infection',
    VE_waning_LOCAL = VE_waning_distribution, #loaded from (2)
    vaccination_history_LOCAL = vaccination_history_TRUE, #loaded from (1)
    SA_toggles_local = sensitivity_analysis_toggles) %>%
    mutate(date = date_now)
  
  #gradual shift in VE from omicron to delta
  if (date_now %in% omicron_shift$date){
    VE_here = VE_time_step("delta",date_now,'any_infection',
                                 VE_waning_LOCAL = VE_waning_distribution,
                                 vaccination_history_LOCAL = vaccination_history_TRUE,
                                 SA_toggles_local = sensitivity_analysis_toggles) %>%
      rename(delta_VE = VE) %>%
      left_join(VE_here, by = c("risk_group", "dose", "vaccine_type", "age_group")) %>%
      mutate(VE = VE * omicron_shift$percentage[omicron_shift$date == date_now] +
               delta_VE * (1-omicron_shift$percentage[omicron_shift$date == date_now])) %>%
      select(-delta_VE)
  }
  
  VE_real_range = rbind(VE_real_range, VE_here)
}

rm(VE_time_step)
save(VE_real_range, file = paste('1_inputs/fit/VE_real_range',Sys.Date(),this_setting,'.Rdata',sep=''))
#______________________________________________________________________________________________________________



### Model run _________________________________________________________________________________________________
#20/12/2022 - have removed Reff/VE/rho tracking using fitting_details parameter; stop VE_time_step for acq (VE_real_range); stop VE_time_step for SO (don't run (5) or (6) when fitting)

system.time(source(paste(getwd(),"/CommandDeck.R",sep="")))

coeff <- 1/20
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



### Fit wave peaks ____________________________________________________________________________________________
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

#fit_wave_peaks function fits to the most likely seed dates that correspond to the peaks of observed waves
fit_wave_peaks <- function(par){
  
  strain_inital = strain_now = 'WT' 
  
  covid19_waves = data.frame(date = c(as.Date('2021-06-06')+par[1],as.Date('2021-10-15')+par[2],as.Date('2022-02-01')+par[3]),
                             strain = c('delta','omicron','omicron'))
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  simulated_cases = incidence_log %>% select(date,rolling_average)
  simulated_cases <- na.omit(simulated_cases)
  
  fitted_peaks = findpeaks(simulated_cases$rolling_average,minpeakheight = 500 ,minpeakdistance = 90, npeaks = 3 )
  fitted_peaks = simulated_cases$date[c(fitted_peaks[,2])]
  
  fit_statistic = sum(as.numeric(reported_peaks - fitted_peaks)^2)

  return(fit_statistic)
}
fit_wave_peaks(c(0,0,0))

lets_fit_peaks = optim(par = c(0,0,0),
                       fn = fit_wave_peaks,
                       method = "Nelder-Mead")

#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/optim
# $par
# [1] "2021-06-06" "2021-10-21" "2022-01-01"
# 
# $value
# [1] 16264.25
# 
# $counts
# function gradient 
# 4       NA  -> number of f(x) calls used
# 
# $convergence
# [1] 0 -> indicates successful completion
# 
# $message
# NULL
###not fitting as search space too complicated?
save(lets_fit_peaks, file = paste('1_inputs/fit/lets_fit_peaks',Sys.Date(),this_setting,'.Rdata',sep=''))


### Fit first wave ###########################################################################################
#seed date <-> max date
fit_wave_peak <- function(par){
  
  strain_inital = strain_now = 'WT' 
  
  run_till = reported_peaks[1]+as.numeric(reported_peaks[2] - reported_peaks[1])
  model_weeks = as.numeric((run_till-date_start)/7)
  
  covid19_waves = data.frame(date = as.Date('2021-06-06')+par,
                             strain = 'delta')
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  simulated_cases = incidence_log %>% select(date,rolling_average)
  simulated_cases <- na.omit(simulated_cases)
  
  fitted_peaks = findpeaks(simulated_cases$rolling_average,minpeakheight = 500 ,minpeakdistance = 90, npeaks = 1 )
  fitted_peaks = simulated_cases$date[c(fitted_peaks[,2])]
  
  fit_statistic = sum(as.numeric(reported_peaks[1] - fitted_peaks)^2)
  
  return(fit_statistic)
}

lets_fit_peak = optimize(fit_wave_peak,c(-3,3))
coeff <- 1/16
ggplot() +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=incidence_log,aes(x=date,y=rolling_average)) + 
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+ 
  plot_standard
#decison: on visual inspection close enough to start real fit to fit wave




fit_daily_reported <- function(seed_date_est,
                               under_reporting_est#,
                               #beta_est
                               ){
  
  strain_inital = strain_now = 'WT' 
  
  run_till = as.Date('2021-10-15') #earliest likely introduction of Omicron
  model_weeks = as.numeric((run_till-date_start)/7)
  
  covid19_waves = data.frame(date = as.Date('2021-06-06')+seed_date_est,
                             strain = 'delta')
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  workshop = reported_cases %>%
    filter(date>date_start & date <max(incidence_log$date)) %>%
    mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
            rolling_average = rolling_average * under_reporting_est) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2)
  
  fit_statistic = sum(workshop$fit_statistic,na.rm=TRUE)
  
  return(fit_statistic)
}

lets_fit_daily_reported = optim(c(0,16),
                                fit_daily_reported,
                                method = "Nelder-Mead")

ggplot() +
  geom_line(data=workshop[workshop$date>date_start & workshop$date <max(workshop$date),],
             aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=workshop,aes(x=date,y=adjusted_reported)) + 
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+ 
  plot_standard
#______________________________________________________________________________________________________________



### Under-reporting ____________________________________________________________________________________________
#Three options: uniform, linear decrease, exponential decrease
under_reporting_shape = "exponential"

#Option 1: Uniform
if (under_reporting_shape == "uniform"){
  coeff1 = 35 #20 seems reasonable 15-30 or 10-35 to be sure
  
  reported_cases_here = reported_cases %>%
    filter(date>date_start & date <max(incidence_log$date)) %>%
    mutate(adjusted_average = rolling_average * coeff1)
} else if (under_reporting_shape == "linear"){
  
  coeff1 = 15 #y-int
  coeff2 = 0.05
  
  reported_cases_here = reported_cases %>%
    filter(date>date_start & date <max(incidence_log$date)) %>%
    mutate(under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start),
           adjusted_average = rolling_average * under_reporting_est)
  
  # ggplot() +
  #   geom_point(data=reported_cases_here,aes(x=date,y=under_reporting_est),na.rm=TRUE) + 
  #   plot_standard
  
} else if (under_reporting_shape == "exponential"){
  
  coeff1 = 10 #y-int
  coeff2 = 0.003
  
  reported_cases_here = reported_cases %>%
    filter(date>date_start & date <max(incidence_log$date)) %>%
    mutate(under_reporting_est = coeff1*(1+coeff2)^(as.numeric(date - date_start)),
           adjusted_average = rolling_average * under_reporting_est)
  
  # ggplot() +
  #   geom_point(data=reported_cases_here,aes(x=date,y=under_reporting_est),na.rm=TRUE) +
  #   plot_standard
  
}

ggplot() +
  geom_point(data=reported_cases_here,aes(x=date,y=adjusted_average),na.rm=TRUE) +
  geom_line(data=incidence_log,aes(x=date,y=rolling_average)) + 
  plot_standard
#______________________________________________________________________________________________________________



### Fit complete waves with under reporting ____________________________________________________________________________________________
fit_daily_reported <- function(seed_dates,coeff1,coeff2){
  
  covid19_waves$date = seed_dates
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  workshop = reported_cases %>%
    filter(date>date_start & date <max(incidence_log$date)) %>%
    mutate(under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
           adjusted_reported = rolling_average * under_reporting_est) %>%
    left_join(incidence_log) %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2)
  
  fit_statistic = sum(workshop$fit_statistic)
  
  return(fit_statistic)
}

lets_fit_daily_reported = optim(covid19_waves$date,
                                fit_daily_reported,
                                method = "Nelder-Mead")
#______________________________________________________________________________________________________________#______________________________________________________________________________________________________________




### Save ___________________________________________________________________________________________
#columns: fitted_setting, fitted_date, strain, date
load(file = '1_inputs/fit/fitted_covid19_waves.Rdata')
fitted_covid19_waves = fitted_covid19_waves %>% 
  mutate(fitted_setting = this_setting,
         fitted_date = Sys.Date())
fitted_covid19_waves = rbind(fitted_covid19_waves,this_fit_covid19_waves)
save(fitted_covid19_waves, file = '1_inputs/fit/fitted_covid19_waves.Rdata')
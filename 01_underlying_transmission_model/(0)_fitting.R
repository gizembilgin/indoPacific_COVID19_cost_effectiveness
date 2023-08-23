### This scripts sets up for the fitting scripts stored in 02_fit

rm(list=ls()) #clear the field!
this_setting = setting = "TLS"



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
  baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-06-09'),as.Date('2021-10-15'),as.Date('2022-02-01')), # previous best guess
                                                      strain = c('delta','omicron','omicron'))

} else if (this_setting == "PNG"){
  strain_inital = strain_now = 'WT'
  baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-01-15'),as.Date('2021-09-01'),as.Date('2021-12-01')),
                                                      strain = c('WT','delta','omicron'))
  date_start = covid19_waves$date[1] - 2

} else if (this_setting == "TLS") {
  strain_inital = strain_now = 'WT'
  baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-03-01'),as.Date('2021-05-01'),as.Date('2021-11-01')),
                                                      strain = c('WT', 'delta', 'omicron'))
  date_start = covid19_waves$date[1] - 2
  
} else if (this_setting == "IDN") {
  strain_inital = strain_now = 'WT'
  baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-04-01'),as.Date('2021-10-15')),
                                      strain = c('delta','omicron'))
  date_start = covid19_waves$date[1] - 2
}

model_weeks = as.numeric((Sys.Date()+1-date_start)/7)

#plot standard
plot_standard = theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#risk group toggles
risk_group_prioritisation_to_date = NA
risk_group_lower_cov_ratio = NA
risk_group_toggle = "on"

risk_group_name = 'adults_with_comorbidities'
RR_estimate = 1.95
#______________________________________________________________________________________________________________



### Saving VE for known dates __________________________________________________________________________________
#Load vax history
source(paste(getwd(),"/04_functions/(function)_VE_time_step.R",sep=""))
source(paste(getwd(),"/04_functions/(function)_rho_time_step.R",sep=""))
source(paste(getwd(),"/(1)_simulate_setting.R",sep="")) 
source(paste(getwd(),"/(3)_disease_characteristics.R",sep=""))
source(paste(getwd(),"/(2)_inital_state.R",sep=""))

list_poss_Rdata = list.files(path="01_inputs/fit/",pattern = paste("VE_real_range_",this_setting,"*",sep=''))
if (length(list_poss_Rdata)>0){
  list_poss_Rdata_details = double()
  for (i in 1:length(list_poss_Rdata)){
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste("01_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste("01_inputs/fit/",latest_file,sep=''))
  
  date_list = seq(max(max(VE_real_range$date)+1), # run only dates not yet filled
                  Sys.Date()+1,
                  by="days")
} else{
  VE_real_range = data.frame()
  date_list = seq(max(date_start,min(vaccination_history_TRUE$date)+ min(vaxCovDelay$delay)),
                  Sys.Date()+1,
                  by="days")
}

for (i in 1:length(date_list)) {
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
save(VE_real_range, file = paste('01_inputs/fit/VE_real_range_',this_setting,'_',Sys.Date(),'.Rdata',sep=''))
#______________________________________________________________________________________________________________



### Fit cutoff dates __________________________________________________________________________________________
require('pracma') #contains findpeaks function

#reported_cases remove NA, and within simulation scope
reported_cases = case_history %>% select(date,rolling_average) %>% filter(date > date_start)
reported_cases <- na.omit(reported_cases)

#reported_peaks - peaks in reported daily cases
reported_peaks = findpeaks(reported_cases$rolling_average,minpeakheight = 50 ,minpeakdistance = 90, npeaks = 3 )
reported_peaks = reported_cases$date[c(reported_peaks[,2])]

#fit cutoff dates
if (this_setting == "FJI"){
  #add third peak
  window = reported_cases %>% filter(date>as.Date('2022-05-01'))
  third_peak = median(window$date[window$rolling_average == max(window$rolling_average)])
  reported_peaks = c(reported_peaks,third_peak)
  
  fit_cutoff_dates = c(as.Date('2021-10-15'),#earliest likely introduction of Omicron
                       reported_peaks[2] + as.numeric(reported_peaks[3] - reported_peaks[2])/2)  
} else if (this_setting == "IDN"){
  fit_cutoff_dates =c((sort(reported_peaks)[2]-sort(reported_peaks)[1])/2+sort(reported_peaks)[1],
                      sort(reported_peaks)[2]+90)
}
#_____________________________________________
rm(list=ls())

fit_daily_reported_3 <- function(par){
  
  this_setting = setting = "FJI"
  
  ### Setup ______________________________________________________________________
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
    reported_peaks = c(as.Date("2021-07-21"), as.Date("2022-01-21"), as.Date("2022-07-15"))
    fit_cutoff_dates = c(as.Date('2021-10-15'),#earliest likely introduction of Omicron
                         reported_peaks[2] + as.numeric(reported_peaks[3] - reported_peaks[2])/2)  
    
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
  
  #risk group toggles
  risk_group_prioritisation_to_date = NA
  risk_group_lower_cov_ratio = NA
  risk_group_toggle = "on"
  risk_group_name = 'adults_with_comorbidities'
  RR_estimate = 1.95
  #_______________________________________________________________________________
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
  
  ### Configure scenario _________________________________________________________
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
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))#,local=TRUE)
  
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

# baseline_date_start = as.Date('2021-04-30')
# to_plot = workshop %>%
#   filter(date>baseline_date_start) %>%
#   filter(date>date_start & date<=(date_start+model_weeks*7))
# ggplot() +
#   geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
#   geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
#   plot_standard



library(doParallel); library(parallel); library(foreach); library(optimParallel)
#detectCores() #8, use 3 given 25% CPU usage of each run
CLUSTER <- parallel::makeCluster(3) # create cluster
doParallel::registerDoParallel(CLUSTER) # activate cluster

system.time({third_wave_fit = optimParallel(
  par = c(0,120,1.2),
  fn = fit_daily_reported_3,
  method = "L-BFGS-B",
  lower = c(0,50,1), #upper = c(60,250,2),
  parallel = list(cl = CLUSTER,
                  loginfo = TRUE))})
###NEED CommandDeck as function call instead of source call for parallel to copy out param 

parallel::stopCluster(CLUSTER)
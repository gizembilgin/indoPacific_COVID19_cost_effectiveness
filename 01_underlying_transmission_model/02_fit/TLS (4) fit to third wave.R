### load fit of first two waves
list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("TLS_rough_fit*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste("1_inputs/fit/",latest_file,sep=''))
firstSecondWaveFit = rough_fit
#______________________________



### define fitting function
fit_thirdWave <- function(par){
  
  fitting = "wave_three"
  date_start = baseline_covid19_waves$date[3]-1
  model_weeks = 26
  
  strain_inital = strain_now = 'delta' 
  TOGGLE_delta_truncation_factor = firstSecondWaveFit$optim$bestmem[1]
  fitting_beta = c(firstSecondWaveFit$optim$bestmem[2],
                   firstSecondWaveFit$optim$bestmem[3],
                   par[1])
  
  covid19_waves = baseline_covid19_waves
  covid19_waves$date[1] = covid19_waves$date[1] + round(firstSecondWaveFit$optim$bestmem[4])
  covid19_waves$date[2] = covid19_waves$date[2] + round(firstSecondWaveFit$optim$bestmem[5])
  covid19_waves$date[3] = covid19_waves$date[3] + round(par[2])
  
  source(paste(getwd(), "/CommandDeck.R", sep = ""),local=TRUE) #5 minutes

     
  #quick search under reporting
  increments_list = c(100,50,10,5,1,0.25)
  underreporting_tracker = data.frame()
  
  for (repeat_through in 1:length(increments_list)){
    
    increment = increments_list[repeat_through]
    
    if (repeat_through == 1){
      search_list1  = seq(50,1000,by=increments_list[repeat_through])
    } else{
      best_so_far = underreporting_tracker[underreporting_tracker$fit== min(underreporting_tracker$fit, na.rm=TRUE),]
      best_so_far = unique(best_so_far)
      if (nrow(best_so_far)>1){ #pick best_so_far with min under reporting
        best_so_far = best_so_far[best_so_far$wave3 == min(best_so_far$wave3),]
      }
      search_list1 = seq(best_so_far$wave3 - increments_list[repeat_through-1],
                         best_so_far$wave3 + increments_list[repeat_through-1],
                         by = increments_list[repeat_through])
    }
  
    for(under_reporting_wave3 in search_list1){
        workshop = case_history %>%
          select(date,rolling_average) %>%
          rename(reported_cases = rolling_average) %>%
          right_join(incidence_log, by = "date") %>%
          left_join(delta_shift, by = "date") %>%
          rename(delta = percentage) %>%
          mutate( rolling_average =  rolling_average * 1/under_reporting_wave3) %>%
          filter(date >= as.Date('2021-12-31') &
                   date <= as.Date('2022-04-01')) %>%
         mutate(fit_statistic = abs(rolling_average - reported_cases)^2)
        
        fit_statistic = data.frame(
          fit = sum(workshop$fit_statistic,
                    na.rm=TRUE),
          wave3 = under_reporting_wave3)
        
        underreporting_tracker = rbind(underreporting_tracker,fit_statistic)
    }
  }
  
  
  fit_statistic = min(underreporting_tracker$fit, na.rm=TRUE)
  return(fit_statistic)
}

# plot to check function
best_fit = underreporting_tracker[underreporting_tracker$fit == min(underreporting_tracker$fit),]
under_reporting_wave3 = unique(best_fit$wave3)

ggplot() +
  geom_line(data=workshop,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=workshop,aes(x=date,y=reported_cases)) +
  plot_standard 
ggplot() +
  geom_line(data=incidence_log,aes(x=date,y=rolling_average),na.rm=TRUE)
#__________________________



### fit!
require(DEoptim)
thirdWaveFit <- DEoptim(fn = fit_thirdWave,
                     lower = c(0.5,30),
                     upper = c(2,75),
                     control = list(NP = 20,
                                    itermax = 10,
                                    storepopfrom = 1)) 
save(thirdWaveFit, file = paste('1_inputs/fit/TLS_thirdWave',Sys.Date(),'.Rdata',sep=''))
#__________________________



### Explore fit
summary(thirdWaveFit)
plot(thirdWaveFit, plot.type = "bestvalit")
plot(thirdWaveFit, plot.type ="bestmemit")
plot(thirdWaveFit, plot.type ="storepop")
to_plot = as.data.frame(thirdWaveFit$member$pop)
colnames(to_plot) <- c('beta3','shift3')
ggplot(to_plot) + geom_histogram(aes(x=beta3),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=shift3),bins=10)
ggplot(to_plot) + geom_point(aes(x=beta3,y=shift3))
#__________________________


### Save fitted result
model_weeks = as.numeric((as.Date('2022-12-31') - date_start)/7)
par = thirdWaveFit$optim$bestmem

#<run inside of f(x)>

incidence_log = incidence_log %>% select(date,daily_cases)

fitted_results = list(
  FR_parameters = parameters,
  FR_next_state = next_state,
  FR_incidence_log_tidy = incidence_log_tidy,
  FR_incidence_log = incidence_log,
  FR_covid19_waves = covid19_waves,
  FR_fitting_beta = fitting_beta
)
save(fitted_results, file = paste("1_inputs/fit/fitted_results_",this_setting,Sys.Date(),".Rdata",sep=""))


workshop = case_history %>%
  select(date,rolling_average) %>%
  rename(reported_cases = rolling_average) %>%
  right_join(incidence_log, by = "date") %>%
  left_join(delta_shift, by = "date") %>%
  rename(delta = percentage) %>%
  left_join(omicron_shift, by = "date") %>%
  rename(omicron = percentage) %>%
  mutate(
    rolling_average = case_when(
      date >= min(omicron_shift$date)  & is.na(omicron) == FALSE ~ rolling_average * (1/under_reporting_wave3*omicron + 1/under_reporting_wave2*(1-omicron)),
      date >= min(omicron_shift$date)  ~ rolling_average * 1/under_reporting_wave3,
      date >= min(delta_shift$date)  & is.na(delta) == FALSE ~ rolling_average * (1/under_reporting_wave2*delta + 1/under_reporting_wave1*(1-delta)),
      date >= min(delta_shift$date)  ~ rolling_average * 1/under_reporting_wave2,
      date < min(delta_shift$date) ~ rolling_average * 1/under_reporting_wave1)) 
ggplot() +
  geom_line(data=workshop,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=workshop,aes(x=date,y=reported_cases)) +
  plot_standard 
#_________________________________________________



### Save fitted result for pregnant women
#NB: can't start from third wave save point as that is configured for adults_with_comorbidities

fitting = "on"
strain_inital = strain_now = 'WT' 
risk_group_name = 'pregnant_women'; RR_estimate =  2.4

TOGGLE_delta_truncation_factor = firstSecondWaveFit$optim$bestmem[1]
fitting_beta = c(firstSecondWaveFit$optim$bestmem[2],
                 firstSecondWaveFit$optim$bestmem[3],
                 thirdWaveFit$optim$bestmem[1])

covid19_waves = baseline_covid19_waves
covid19_waves$date[1] = covid19_waves$date[1] + round(firstSecondWaveFit$optim$bestmem[4])
covid19_waves$date[2] = covid19_waves$date[2] + round(firstSecondWaveFit$optim$bestmem[5])
covid19_waves$date[3] = covid19_waves$date[3] + round(thirdWaveFit$optim$bestmem[2])

date_start = covid19_waves$date[1] - 2
model_weeks = as.numeric((as.Date('2022-12-31') - date_start)/7)

source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE) 

#<check>
workshop = case_history %>%
  select(date,rolling_average) %>%
  rename(reported_cases = rolling_average) %>%
  right_join(incidence_log, by = "date") %>%
  left_join(delta_shift, by = "date") %>%
  rename(delta = percentage) %>%
  left_join(omicron_shift, by = "date") %>%
  rename(omicron = percentage) %>%
  mutate(
    rolling_average = case_when(
      date >= min(omicron_shift$date)  & is.na(omicron) == FALSE ~ rolling_average * (1/under_reporting_wave3*omicron + 1/under_reporting_wave2*(1-omicron)),
      date >= min(omicron_shift$date)  ~ rolling_average * 1/under_reporting_wave3,
      date >= min(delta_shift$date)  & is.na(delta) == FALSE ~ rolling_average * (1/under_reporting_wave2*delta + 1/under_reporting_wave1*(1-delta)),
      date >= min(delta_shift$date)  ~ rolling_average * 1/under_reporting_wave2,
      date < min(delta_shift$date) ~ rolling_average * 1/under_reporting_wave1)) 
ggplot() +
  geom_line(data=workshop,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=workshop,aes(x=date,y=reported_cases)) +
  plot_standard 
#CHECKED: fit looks the same!

incidence_log = incidence_log %>% select(date,daily_cases)

fitted_results = list(
  FR_parameters = parameters,
  FR_next_state = next_state,
  FR_incidence_log_tidy = incidence_log_tidy,
  FR_incidence_log = incidence_log,
  FR_covid19_waves = covid19_waves,
  FR_fitting_beta = fitting_beta
)
save(fitted_results, file = paste("1_inputs/fit/fitted_results_pregnant_women_",this_setting,Sys.Date(),".Rdata",sep=""))
#_________________________________________________
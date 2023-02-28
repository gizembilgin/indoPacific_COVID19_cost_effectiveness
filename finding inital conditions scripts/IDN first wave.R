### WAVE ONE - IDN

### Attempt One: fit to WT
strain_inital = strain_now = 'WT' 

fitting_beta= rep(1,length(reported_peaks))
under_reporting_est = 150

baseline_covid19_waves = data.frame(date = c(as.Date('2020-04-01'),as.Date('2021-04-01'),as.Date('2021-12-01')),
                                    strain = c('WT','delta','omicron'))

plot_tracker = data.frame()
fit_tracker = data.frame()
seroprev_tracker = data.frame()
workshop_incidence_log_tracker = data.frame()

for (this_beta_mod in  c(0.9,0.925,0.95,0.975,1)) {
  for (this_shift in c(90,120,150)){#c(135,142,150,157,165)) {
    if(nrow(plot_tracker[plot_tracker$shift == this_shift & plot_tracker$beta_mod == this_beta_mod,])>0){
      #skip
    } else{
      strain_inital = strain_now = 'WT' 
      
      fitting_beta[1]= this_beta_mod
      covid19_waves = baseline_covid19_waves 
      covid19_waves$date[1] = covid19_waves$date[1] + this_shift
      date_start = covid19_waves$date[1] - 7
      model_weeks = as.numeric((fit_cutoff_dates[1]-date_start)/7)
      
      source(paste(getwd(),"/CommandDeck.R",sep=""))
      
      workshop = case_history %>%
        select(date,rolling_average) %>%
        mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
          rolling_average =  rolling_average * under_reporting_est) %>%
        rename(adjusted_reported = rolling_average) %>%
        left_join(incidence_log, by = "date") %>%
        mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2 ,
               beta_mod = this_beta_mod, shift = this_shift)
      
      fit_statistic = data.frame(fit = sum(workshop$fit_statistic, #fit only after first wave
                                           na.rm=TRUE),
                                 beta_mod = this_beta_mod, shift = this_shift)
      
      incidence_log = incidence_log %>%
        mutate(beta_mod = this_beta_mod, shift = this_shift)
      
      this_seroprev = data.frame(beta_mod = this_beta_mod, 
                                 shift = this_shift,
                                 seroprev = sum(incidence_log$daily_cases)/sum(pop))
      
      seroprev_tracker = rbind(seroprev_tracker,this_seroprev)
      plot_tracker = rbind(plot_tracker,workshop)
      fit_tracker = rbind(fit_tracker,fit_statistic)
      
      workshop_incidence_log_tracker = rbind(workshop_incidence_log_tracker,incidence_log)
    }
  }
}
beta_fit_tracker = fit_tracker; beta_fit_tracker
beta_tracker = plot_tracker %>%
  filter(date<= max(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           date>= min(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE])) #%>%
  #filter(!(shift %in% c(120,180,195)) & beta_mod != 0.9)

under_reporting_est = 150

ggplot() +
  geom_line(data=beta_tracker,aes(x=date,y=rolling_average,color=as.factor(beta_mod)),na.rm=TRUE) +
  geom_point(data=case_history[case_history$date<=max(beta_tracker$date),],aes(x=date,y=rolling_average*under_reporting_est)) +
  plot_standard + 
  labs(color = 'beta modifier')+ 
  facet_grid(shift ~ .) 
save(plot_tracker, file = paste('1_inputs/fit/IDN_tracker.Rdata',sep=''))



#All of these seroprev estimates are much higher than expected! See if we can get a delta peak, if NOT then we'll have to use a March estimate of seroprevalence then go forward from there
strain_inital = strain_now = 'WT' 
fitting_beta[1]= 0.95
covid19_waves = baseline_covid19_waves 
covid19_waves$date[1] = covid19_waves$date[1] + 120
date_start = covid19_waves$date[1] - 7
model_weeks = as.numeric((as.Date('2022-01-01')-date_start)/7)

source(paste(getwd(),"/CommandDeck.R",sep=""))
ggplot() +
  geom_line(data=incidence_log,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=case_history,aes(x=date,y=rolling_average*under_reporting_est)) +
  plot_standard
#_______________________________________________________________________________


### Attempt Two: fit to delta
strain_inital = strain_now = 'WT' 

baseline_covid19_waves = data.frame(date = c(as.Date('2021-04-01'),as.Date('2021-12-01')),
                                    strain = c('delta','omicron'))
fitting_beta= rep(1,nrow(baseline_covid19_waves))
under_reporting_est = 70

plot_tracker = data.frame()
fit_tracker = data.frame()
workshop_incidence_log_tracker = data.frame()

for (this_beta_mod in  c(0.9,0.95,1,1.05,1.1,1.15)) {
  for (this_shift in c(30,35,40,45)){#c(135,142,150,157,165)) {
    if(nrow(plot_tracker[plot_tracker$shift == this_shift & plot_tracker$beta_mod == this_beta_mod,])>0){
      #skip
    } else{
      strain_inital = strain_now = 'WT' 
      
      fitting_beta[1]= this_beta_mod
      covid19_waves = baseline_covid19_waves 
      covid19_waves$date[1] = covid19_waves$date[1] + this_shift
      date_start = covid19_waves$date[1] - 7
      model_weeks = as.numeric((covid19_waves$date[2]-date_start)/7)
      
      source(paste(getwd(),"/CommandDeck.R",sep=""))
      
      workshop = case_history %>%
        select(date,rolling_average) %>%
        mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
          rolling_average =  rolling_average * under_reporting_est) %>%
        rename(adjusted_reported = rolling_average) %>%
        left_join(incidence_log, by = "date") %>%
        mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2 ,
               beta_mod = this_beta_mod, shift = this_shift)
      
      fit_statistic = data.frame(fit = sum(workshop$fit_statistic, #fit only after first wave
                                           na.rm=TRUE),
                                 beta_mod = this_beta_mod, shift = this_shift)
      
      incidence_log = incidence_log %>%
        mutate(beta_mod = this_beta_mod, shift = this_shift)
      
      plot_tracker = rbind(plot_tracker,workshop)
      fit_tracker = rbind(fit_tracker,fit_statistic)
      
      workshop_incidence_log_tracker = rbind(workshop_incidence_log_tracker,incidence_log)
    }
  }
}
beta_fit_tracker = fit_tracker; beta_fit_tracker
beta_tracker = plot_tracker %>%
  filter(date<= max(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           date>= min(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) &
           shift %in% c(35,40) &
           beta_mod %in% c(0.95,1,1.05)) 

ggplot() +
  geom_line(data=beta_tracker,aes(x=date,y=rolling_average,color=as.factor(beta_mod)),na.rm=TRUE) +
  geom_point(data=case_history[case_history$date<=max(beta_tracker$date) & case_history$date>min(beta_tracker$date),],aes(x=date,y=rolling_average*under_reporting_est)) +
  plot_standard + 
  labs(color = 'beta modifier')+ 
  facet_grid(shift ~ .) 
save(plot_tracker, file = paste('1_inputs/fit/IDN_tracker_v2.Rdata',sep=''))


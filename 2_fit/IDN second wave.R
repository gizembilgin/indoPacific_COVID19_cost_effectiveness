#### Create start point for wave two fit
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


#configure scenario
strain_inital = strain_now = 'WT' 
model_weeks = as.numeric(((baseline_covid19_waves$date[2]-28)-date_start)/7)

covid19_waves = baseline_covid19_waves
covid19_waves$date[1] = covid19_waves$date[1] + round(first_wave_fit$par[1])

fitting_beta= c(first_wave_fit$par[3],1)

source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)

fitted_results = list(
  FR_parameters = parameters,
  FR_next_state = next_state,
  FR_incidence_log_tidy = incidence_log_tidy,
  FR_incidence_log = incidence_log,
  FR_covid19_waves = covid19_waves,
  FR_fitting_beta = fitting_beta,
  FR_prev_beta = prev_beta,
  FR_this_beta = this_beta
)
save(fitted_results, file = paste("1_inputs/fit/start_point_wave_two_",this_setting,Sys.Date(),".Rdata",sep=''))
#______________________________________________



### Run from start point onwards
fitting = "wave_two"

date_start = baseline_covid19_waves$date[2]-28-1
model_weeks = as.numeric((fit_cutoff_dates[2]-date_start)/7)

covid19_waves = baseline_covid19_waves
covid19_waves$date[1] = covid19_waves$date[1] + round(first_wave_fit$par[1])

under_reporting_est = 70
fitting_beta= c(first_wave_fit$par[3],1)

system.time({source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)})

workshop = case_history %>%
  select(date,rolling_average) %>%
  mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
    rolling_average = case_when(
      date > fit_cutoff_dates[1] ~ rolling_average * under_reporting_est,
      date <= fit_cutoff_dates[1] ~ rolling_average * first_wave_fit$par[2])) %>%
  rename(adjusted_reported = rolling_average) %>%
  left_join(incidence_log, by = "date") %>%
  mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2)

to_plot = workshop %>% filter(date>date_start & date<=(date_start+model_weeks*7))
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
  plot_standard
#______________________________________________



### Search 
### Attempt Two: fit to delta
baseline_covid19_waves  = data.frame(date = c(as.Date('2021-04-01'),as.Date('2021-10-15')),
                                                    strain = c('delta','omicron'))
date_start = baseline_covid19_waves$date[2]-28-1
model_weeks = as.numeric((fit_cutoff_dates[2]-date_start)/7)

fitting_beta= c(first_wave_fit$par[3],1)
under_reporting_est = 70

plot_tracker = data.frame()
fit_tracker = data.frame()
workshop_incidence_log_tracker = data.frame()

for (TOGGLE_omicron_truncation_factor in c(1,0.9,0.8,0.7,0.6)){
  for (this_shift in c(0, 7, -7, 14, -14, 21, -21)){ #7
    for (this_beta_mod in  c(1,0.95,1.05,0.9,1.1,1.15)) { #6
      if(nrow(plot_tracker[plot_tracker$shift == this_shift & 
                           plot_tracker$beta_mod == this_beta_mod &
                           plot_tracker$omicron_truncation == TOGGLE_omicron_truncation_factor,])>0){
        #skip
      } else{
        
        strain_inital = strain_now = 'delta' 
        fitting_beta[2]= this_beta_mod
        
        covid19_waves = baseline_covid19_waves
        covid19_waves$date[1] = covid19_waves$date[1] + round(first_wave_fit$par[1])
        covid19_waves$date[2] = covid19_waves$date[2] + this_shift
        
        source(paste(getwd(),"/CommandDeck.R",sep=""))
        
        workshop = case_history %>%
          select(date,rolling_average) %>%
          mutate(
            rolling_average = case_when(
              date > fit_cutoff_dates[1] ~ rolling_average * under_reporting_est,
              date <= fit_cutoff_dates[1] ~ rolling_average * first_wave_fit$par[2])) %>%
          rename(adjusted_reported = rolling_average) %>%
          left_join(incidence_log, by = "date") %>%
          mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2 ,
                 beta_mod = this_beta_mod, 
                 shift = this_shift,
                 omicron_truncation = TOGGLE_omicron_truncation_factor)
        
        fit_statistic = data.frame(fit = sum(workshop$fit_statistic, #fit only after first wave
                                             na.rm=TRUE),
                                   beta_mod = this_beta_mod, 
                                   shift = this_shift,
                                   omicron_truncation = TOGGLE_omicron_truncation_factor)
        
        incidence_log = incidence_log %>%
          mutate(beta_mod = this_beta_mod, shift = this_shift,
                 omicron_truncation = TOGGLE_omicron_truncation_factor)
        
        plot_tracker = rbind(plot_tracker,workshop)
        fit_tracker = rbind(fit_tracker,fit_statistic)
        workshop_incidence_log_tracker = rbind(workshop_incidence_log_tracker,incidence_log)
      }
    }
  }
}
save(plot_tracker, file = paste('1_inputs/fit/IDN_tracker_wave2.Rdata',sep=''))

beta_fit_tracker = fit_tracker; beta_fit_tracker[beta_fit_tracker$fit == min(beta_fit_tracker$fit),]
beta_tracker = plot_tracker %>%
  filter(date<= max(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           date>= min(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           date > date_start &
           shift %in% c(0, 7, -7) &
           beta_mod %in% c(0.9, 0.95, 1, 1.05, 1.1, 1.15) #&
           #omicron_truncation != 1
         ) 

under_reporting_est = 90

ggplot() +
  geom_line(data=beta_tracker,aes(x=date,y=rolling_average,color=as.factor(beta_mod),linetype = as.factor(omicron_truncation)),na.rm=TRUE) +
  geom_point(data=case_history[case_history$date<=max(beta_tracker$date) & case_history$date>min(beta_tracker$date),],aes(x=date,y=rolling_average*under_reporting_est)) +
  plot_standard + 
  labs(color = 'beta modifier')+ 
  facet_grid(shift ~ .) 

#load(file = paste('1_inputs/fit/IDN_tracker_v2.Rdata',sep=''))


#find range under reporting
find_underreporting = data.frame()
best_this_combo = data.frame()
for (TOGGLE_omicron_truncation_factor in c(1,0.9,0.8,0.7,0.6)){
  for (this_shift in c(0, 7, -7, 14, -14, 21, -21)){ #7
    for (this_beta_mod in  c(0.9, 0.95, 1, 1.05, 1.1, 1.15)) {
      
      this_incidence_log = workshop_incidence_log_tracker %>%
        filter(beta_mod == this_beta_mod & shift == this_shift & omicron_truncation == TOGGLE_omicron_truncation_factor)
      
      if (nrow(this_incidence_log)>0){
        for (this_underreporting in c(40:110)){
          
          workshop = case_history %>%
            select(date,rolling_average) %>%
            mutate(
              rolling_average = case_when(
                date > fit_cutoff_dates[1] ~ rolling_average * this_underreporting,
                date <= fit_cutoff_dates[1] ~ rolling_average * first_wave_fit$par[2])) %>%
            rename(adjusted_reported = rolling_average) %>%
            left_join(this_incidence_log, by = "date") %>%
            mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2 )
          
          fit_statistic = data.frame(fit = sum(workshop$fit_statistic, #fit only after first wave
                                               na.rm=TRUE),
                                     beta_mod = this_beta_mod, 
                                     shift = this_shift,
                                     under_reporting_est = this_underreporting,
                                     omicron_truncation = TOGGLE_omicron_truncation_factor)
          
          find_underreporting = rbind(find_underreporting,fit_statistic)
        }
        
        workshop = find_underreporting %>%
          filter(beta_mod == this_beta_mod & shift == this_shift & omicron_truncation == TOGGLE_omicron_truncation_factor)
        workshop = workshop[workshop$fit == min(workshop$fit,na.rm=TRUE),]
        
        best_this_combo = rbind(best_this_combo,workshop)
      }
    }
  }
}



beta_tracker = plot_tracker %>%
  filter(date<= max(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           date>= min(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           shift %in% c(7) &
           beta_mod %in% c(1.05)
  ) 

under_reporting_est = 79

ggplot() +
  geom_line(data=beta_tracker,aes(x=date,y=rolling_average,color=as.factor(beta_mod)),na.rm=TRUE) +
  geom_point(data=case_history[case_history$date<=max(beta_tracker$date) & case_history$date>min(beta_tracker$date),],aes(x=date,y=rolling_average*under_reporting_est)) +
  plot_standard + 
  labs(color = 'beta modifier')+ 
  facet_grid(shift ~ .) 

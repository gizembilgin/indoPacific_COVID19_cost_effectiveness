#### Create start point for wave three fit
fitting = "on"

#Load first wave
list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("first_wave_fit",this_setting,"*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste('1_inputs/fit/',latest_file,sep=''))


#configure scenario
strain_inital = strain_now = 'WT' 

TOGGLE_delta_truncation_factor = first_wave_fit$optim$bestmem[1]

covid19_waves = baseline_covid19_waves
covid19_waves$date[1] = covid19_waves$date[1] + round(first_wave_fit$optim$bestmem[2])
covid19_waves$date[2] = covid19_waves$date[2] + round(first_wave_fit$optim$bestmem[3])
date_start = covid19_waves$date[1] - 2

fitting_beta= c(first_wave_fit$optim$bestmem[4],
                first_wave_fit$optim$bestmem[5],
                 1)

model_weeks = as.numeric(((baseline_covid19_waves$date[3]-28)-date_start)/7)

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
save(fitted_results, file = paste("1_inputs/fit/start_point_wave_three_",this_setting,Sys.Date(),".Rdata",sep=''))
#______________________________________________





### Run from start point onwards
fitting = "wave_three"

date_start = baseline_covid19_waves$date[3]-28-1
model_weeks = as.numeric((as.Date('2022-07-01')-date_start)/7)

system.time({source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)})

under_reporting_est = 50

workshop = case_history %>%
  select(date,rolling_average) %>%
  mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
    rolling_average = case_when(
      date>= date_start ~ rolling_average * under_reporting_est,
      date > as.Date('2021-08-01') & date < date_start ~ rolling_average * first_wave_fit$optim$bestmem[7],
      date <= as.Date('2021-08-01') ~ rolling_average * first_wave_fit$optim$bestmem[6])) %>%
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
plot_tracker = data.frame()
fit_tracker = data.frame()
workshop_incidence_log_tracker = data.frame()

for (TOGGLE_omicron_truncation_factor in c(1,0.8,0.9)){ #3
  for (this_shift in c(0, 7, -7, 14, -14)){ #5
    for (this_beta_mod in  c(1,0.95,1.05,0.9,1.1,1.2)) { #6
      if(nrow(plot_tracker[plot_tracker$shift == this_shift & 
                           plot_tracker$beta_mod == this_beta_mod &
                           plot_tracker$omicron_truncation == TOGGLE_omicron_truncation_factor,])>0){
        #skip
      } else{
        
        strain_inital = strain_now = 'delta' 
        fitting_beta[3]= this_beta_mod
        
        covid19_waves = baseline_covid19_waves
        covid19_waves$date[1] = covid19_waves$date[1] + round(first_wave_fit$optim$bestmem[2])
        covid19_waves$date[2] = covid19_waves$date[2] + round(first_wave_fit$optim$bestmem[3])
        covid19_waves$date[3] = covid19_waves$date[3] + this_shift
        
        source(paste(getwd(),"/CommandDeck.R",sep=""))
        
        workshop = case_history %>%
          select(date,rolling_average) %>%
          mutate(
            rolling_average = case_when(
              date>= date_start ~ rolling_average * under_reporting_est,
              date > as.Date('2021-08-01') & date < date_start ~ rolling_average * first_wave_fit$optim$bestmem[7],
              date <= as.Date('2021-08-01') ~ rolling_average * first_wave_fit$optim$bestmem[6])) %>%
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
save(plot_tracker, file = paste('1_inputs/fit/TLS_tracker_wave3.Rdata',sep=''))

beta_fit_tracker = fit_tracker; beta_fit_tracker[beta_fit_tracker$fit == min(beta_fit_tracker$fit),]
beta_tracker = plot_tracker %>%
  filter(date<= max(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           date>= min(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           date > date_start &
           shift %in% c(0, 7, -7, -14, -21) &
           beta_mod %in% c(0.95, 1, 1.05, 1.1) &
         omicron_truncation == 1
  ) 

under_reporting_est = 70

ggplot() +
  geom_line(data=beta_tracker,aes(x=date,y=rolling_average,color=as.factor(beta_mod),linetype = as.factor(omicron_truncation)),na.rm=TRUE) +
  geom_point(data=case_history[case_history$date<=max(beta_tracker$date) & case_history$date>min(beta_tracker$date),],aes(x=date,y=rolling_average*under_reporting_est)) +
  plot_standard + 
  labs(color = 'beta modifier')+ 
  facet_grid(shift ~ .) 
#______________________________________________





### Trial fit of all three TLS waves in one!
fit_all_waves <- function(par){
  
  fitting = "on"
  strain_inital = strain_now = 'WT' 
  
  TOGGLE_delta_truncation_factor = par[1]
  TOGGLE_omicron_truncation_factor = 1
  
  fitting_beta = c(par[2],
                   par[3],
                   par[4])
  
  covid19_waves = baseline_covid19_waves
  covid19_waves$date[1] = covid19_waves$date[1] + round(par[5])
  covid19_waves$date[2] = covid19_waves$date[2] + round(par[6])
  covid19_waves$date[3] = covid19_waves$date[3] + round(par[7])
  
  date_start = covid19_waves$date[1] - 2
  model_weeks = as.numeric((as.Date('2022-07-01') - date_start)/7)

  source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
    
  workshop = case_history %>%
    select(date,rolling_average) %>%
    rename(reported_cases = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    left_join(delta_shift, by = "date") %>%
    rename(delta = percentage) %>%
    left_join(omicron_shift, by = "date") %>%
    rename(omicron = percentage) %>%
    mutate(
      rolling_average = case_when(
        date>= min(omicron_shift$date) & is.na(omicron) == FALSE ~ rolling_average * (1/par[10]*omicron + 1/par[9]*(1-omicron)),
        date>= min(omicron_shift$date) ~ rolling_average * 1/par[10],
        date >= min(delta_shift$date) & date < min(omicron_shift$date) & is.na(delta) == FALSE ~ rolling_average * (1/par[9]*delta + 1/par[8]*(1-delta)),
        date >= min(delta_shift$date) & date < min(omicron_shift$date)  ~ rolling_average * 1/par[9],
        date < min(delta_shift$date) ~ rolling_average * 1/par[8])) %>%
    mutate(fit_statistic = abs(rolling_average - reported_cases)^2)

  fit_statistic = sum(workshop$fit_statistic, #fit only after first wave
                      na.rm=TRUE)
  
  return(fit_statistic)
}

require(DEoptim)
#need by next Tuesday (5 days away) (40*10*15)/60/24 ~ 4.2 days
full_fit <- DEoptim(fn = fit_all_waves,
                    lower = c(0.2,
                              0.9, 0.95, 0.95,
                              -75 ,45, -21,
                              40, 20, 40
                              ),
                    upper = c(0.4,
                              1.1, 1.1, 1.1,
                              -45, 75, 7,
                              80, 40, 80
                              ),
                    control = list(NP = 15,
                                   itermax = 10,
                                   storepopfrom = 1)) 
save(full_fit, file = paste('1_inputs/fit/full_fit',this_setting,Sys.Date(),'.Rdata',sep=''))

### Explore fit
summary(full_fit)
plot(full_fit, plot.type = "bestvalit")
#plot(full_fit, plot.type ="bestmemit")
plot(full_fit, plot.type ="storepop")
to_plot = as.data.frame(full_fit$member$pop)
colnames(to_plot) <- c('seed_date','under_reporting','beta_modifier')
ggplot(to_plot) + geom_histogram(aes(x=seed_date),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=under_reporting),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=beta_modifier),bins=10)
ggplot(to_plot) + geom_point(aes(x=beta_modifier,y=under_reporting))
#_________________________________________________


### Compare sequential fit to overall fit
#Option 1: full waves fit
par = full_fit$optim$bestmem

#Option 2: fit piece-by-piece
#Load first and second wave
list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("first_wave_fit",this_setting,"*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste('1_inputs/fit/',latest_file,sep=''))
#Load third wave
list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("third_wave_fit",this_setting,"*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste('1_inputs/fit/',latest_file,sep=''))

par = c(first_wave_fit$optim$bestmem[1],
        first_wave_fit$optim$bestmem[4],first_wave_fit$optim$bestmem[5],third_wave_fit$optim$bestmem[3],
        first_wave_fit$optim$bestmem[2],first_wave_fit$optim$bestmem[3],third_wave_fit$optim$bestmem[1],
        first_wave_fit$optim$bestmem[6],first_wave_fit$optim$bestmem[7],third_wave_fit$optim$bestmem[2])
#___________________________________________________


### Check transmission until the end of 2023
#model_weeks = as.numeric((as.Date('2024-01-01') - date_start)/7)
to_plot = workshop %>% 
  filter(date>date_start & date<=(date_start+model_weeks*7))

ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=reported_cases)) +
  plot_standard +
  xlab("")
require(beepr)
beep()


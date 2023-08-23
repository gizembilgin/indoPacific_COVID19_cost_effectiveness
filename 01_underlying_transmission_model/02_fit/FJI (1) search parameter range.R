### View previous fit ##########################################################
fitting = "on"
strain_inital = strain_now = 'WT' 
date_start = as.Date('2021-04-30')
model_weeks = as.numeric(ceiling((as.Date('2022-12-31') - date_start)/7)) 

prev_fitted_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-06-06'),as.Date('2021-10-04'),as.Date('2022-02-01')),
                           strain = c('delta','omicron','omicron'))

# previous fit where NPI parameter was ill configured as NPI instead of (1-NPI)
fitting_beta= c(0.93,1.04,1.22)
under_reporting_est = c(16.9,60,166.5)

NPI_adj = NPI_estimates$NPI[NPI_estimates$date %in% covid19_waves$date]/100
NPI_adj = NPI_adj/(1- NPI_adj)

fitting_beta = fitting_beta * NPI_adj
baseline_fitting_beta = fitting_beta

source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)

workshop = case_history %>%
  select(date,rolling_average) %>%
  rename(reported_cases = rolling_average) %>%
  right_join(incidence_log, by = "date") %>%
  left_join(omicron_shift, by = "date") %>%
  rename(omicron = percentage) %>%
  mutate(
    rolling_average = case_when(
      date>= min(omicron_shift$date[omicron_shift$wave == 2]) & is.na(omicron) == FALSE ~ 
        rolling_average * (1/under_reporting_est[3]*omicron + 1/under_reporting_est[2]*(1-omicron)),
      date>= min(omicron_shift$date[omicron_shift$wave == 2]) ~ 
        rolling_average * 1/under_reporting_est[3],
  
      date >= min(omicron_shift$date[omicron_shift$wave == 1]) & date < min(omicron_shift$date[omicron_shift$wave == 2]) & is.na(omicron) == FALSE ~ 
        rolling_average * (1/under_reporting_est[2]*omicron + 1/under_reporting_est[1]*(1-omicron)),
      date >= min(omicron_shift$date[omicron_shift$wave == 1]) & date < min(omicron_shift$date[omicron_shift$wave == 2])  ~ 
        rolling_average * 1/under_reporting_est[2],
      
      date < min(omicron_shift$date[omicron_shift$wave == 1]) ~ rolling_average * 1/under_reporting_est[1])) %>%
  mutate(fit_statistic = abs(rolling_average - reported_cases)^2)

prev_fit = to_plot = workshop %>% filter(date>date_start)
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=reported_cases)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("") 
#_______________________________________________________________________________



### Check possible range of each fitting_beta ##################################
fitting_beta_range_plot = data.frame()
for (search_this_wave in 1:length(fitting_beta)){
  for (search_this_adj in c(0.75,1.25)){
    fitting_beta = baseline_fitting_beta
    fitting_beta[search_this_wave] = fitting_beta[search_this_wave] * search_this_adj
    
    strain_inital = strain_now = 'WT' 
    source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
    
    workshop = case_history %>%
      select(date,rolling_average) %>%
      rename(reported_cases = rolling_average) %>%
      right_join(incidence_log, by = "date") %>%
      left_join(omicron_shift, by = "date") %>%
      rename(omicron = percentage) %>%
      mutate(
        rolling_average = case_when(
          date>= min(omicron_shift$date[omicron_shift$wave == 2]) & is.na(omicron) == FALSE ~ 
            rolling_average * (1/under_reporting_est[3]*omicron + 1/under_reporting_est[2]*(1-omicron)),
          date>= min(omicron_shift$date[omicron_shift$wave == 2]) ~ 
            rolling_average * 1/under_reporting_est[3],
          
          date >= min(omicron_shift$date[omicron_shift$wave == 1]) & date < min(omicron_shift$date[omicron_shift$wave == 2]) & is.na(omicron) == FALSE ~ 
            rolling_average * (1/under_reporting_est[2]*omicron + 1/under_reporting_est[1]*(1-omicron)),
          date >= min(omicron_shift$date[omicron_shift$wave == 1]) & date < min(omicron_shift$date[omicron_shift$wave == 2])  ~ 
            rolling_average * 1/under_reporting_est[2],
          
          date < min(omicron_shift$date[omicron_shift$wave == 1]) ~ rolling_average * 1/under_reporting_est[1])) %>%
      mutate(fit_statistic = abs(rolling_average - reported_cases)^2) %>%
      mutate(wave = search_this_wave,
             adj = search_this_adj)
    
    fitting_beta_range_plot = rbind(fitting_beta_range_plot,workshop)
  }
}

to_plot = fitting_beta_range_plot %>% filter(date>date_start)
for (this_wave in unique(to_plot$wave)){
  workshop = prev_fit %>%
    mutate(wave = this_wave,
           adj = 1)
  to_plot = rbind(workshop,to_plot)
}
  
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(adj)),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=reported_cases)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("") +
  facet_grid(wave ~. )
#_______________________________________________________________________________



### Check possible range of beta and seed date of first wave ###################
model_weeks = as.numeric((fit_cutoff_dates[1]-date_start-1)/7)

first_wave_range = data.frame()
#for (this_shift in seq(-60,30,by=30)){
#for (this_shift in seq(-30,15,by=15)){
for (this_shift in seq(-15,15,by=5)){
  for (this_beta_mod in seq(2,5,by=0.5)){
    
    if (nrow(first_wave_range[first_wave_range$shift == this_shift &
                              first_wave_range$beta_mod == this_beta_mod,])>0){
      
    } else{
      fitting_beta = baseline_fitting_beta
      fitting_beta[1] = this_beta_mod
      
      covid19_waves = prev_fitted_covid19_waves
      covid19_waves$date[1] = covid19_waves$date[1] + this_shift
      
      strain_inital = strain_now = 'WT' 
      
      source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
      
      workshop = case_history %>%
        select(date,rolling_average) %>%
        rename(reported_cases = rolling_average) %>%
        right_join(incidence_log, by = "date") %>%
        mutate(rolling_average = rolling_average * 1/under_reporting_est[1]) %>%
        mutate(fit_statistic = abs(rolling_average - reported_cases)^2) %>%
        mutate(shift = this_shift,
               beta_mod = this_beta_mod)
      
      first_wave_range = rbind(first_wave_range,workshop)
    }
  }
}
beep()

to_plot = first_wave_range %>% 
  filter(date>date_start) %>%
  filter(shift>-30 & shift<5 &
           beta_mod<4.5)

ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(beta_mod)),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=reported_cases)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("") +
  facet_grid(shift ~. )

#range to search:
# beta: 2-4
# shift: -15:0
# three scenarios for search of wave 2: (1) 2.5, -15; (2) 3, -10; (3) 3.5, -5
#_______________________________________________________________________________



### Save best three scenarios for wave one to use for wave two search ##########
model_weeks = as.numeric((prev_fitted_covid19_waves$date[2]-28-date_start)/7)

first_wave_scenarios = data.frame( beta1 = c(2.5,3,3.5),
                                   shift1 = c(-15,-10,-5))
to_plot = data.frame()

for (ticket in 1:nrow(first_wave_scenarios)){
  
  fitting_beta = baseline_fitting_beta
  fitting_beta[1] = first_wave_scenarios$beta1[ticket]
  
  covid19_waves = prev_fitted_covid19_waves
  covid19_waves$date[1] = covid19_waves$date[1] + first_wave_scenarios$shift1[ticket]
  
  strain_inital = strain_now = 'WT' 
  
  source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
  
  workshop = case_history %>%
    select(date,rolling_average) %>%
    rename(reported_cases = rolling_average) %>%
    right_join(incidence_log, by = "date") %>%
    mutate(rolling_average = rolling_average * 1/under_reporting_est[1]) %>%
    mutate(fit_statistic = abs(rolling_average - reported_cases)^2) %>%
    mutate(shift = first_wave_scenarios$shift1[ticket],
           beta_mod = first_wave_scenarios$beta1[ticket],
           scenario_number = ticket)
  
  to_plot = rbind(to_plot,workshop)
  
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
  save(fitted_results, file = paste("01_inputs/fit/start_point_wave_two_",this_setting,"_v_",ticket,"_",Sys.Date(),".Rdata",sep=''))
  
}
beep()

ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(scenario_number)),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=reported_cases)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("") 
#_______________________________________________________________________________



### Wave two search with three first wave scenarios ############################
#Does the first wave really impact the second wave?
#How long is this run? ~ 8.5 minutes

fitting = "wave_two"

date_start = prev_fitted_covid19_waves$date[2]-28-1
model_weeks = as.numeric((fit_cutoff_dates[2]-date_start-1)/7)

second_wave_range = data.frame()

# for (this_shift in seq(-21, 7, by = 7)) {
#   for (this_beta_mod in seq(1, 6, by = 0.5)) {
for (this_shift in seq(-21, 21, by = 7)) {
  for (this_beta_mod in seq(4, 6, by = 0.5)) {
    for (scenario_MASTER in c(2)) { #c(1:3)
      
      if (nrow(second_wave_range[second_wave_range$first_wave_scenario == scenario_MASTER &
                                 second_wave_range$shift == this_shift &
                                 second_wave_range$beta_mod == this_beta_mod,])>0){
        
      } else{
        
        fitting_beta = baseline_fitting_beta
        fitting_beta[1] = first_wave_scenarios$beta1[scenario_MASTER]
        fitting_beta[2] = this_beta_mod
        
        covid19_waves = prev_fitted_covid19_waves
        covid19_waves$date[1] = covid19_waves$date[1] + first_wave_scenarios$shift1[scenario_MASTER]
        covid19_waves$date[2] = covid19_waves$date[2] + this_shift
        covid19_waves = covid19_waves[c(1,2),]
        
        strain_inital = strain_now = 'delta' 
        
        source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
        
        workshop = case_history %>%
          select(date,rolling_average) %>%
          rename(reported_cases = rolling_average) %>%
          right_join(incidence_log, by = "date")  %>%
          left_join(omicron_shift[omicron_shift$wave == 1,], by = "date") %>%
          rename(omicron = percentage)%>%
          mutate(rolling_average = case_when(
            date >= min(omicron_shift$date[omicron_shift$wave == 1])  & is.na(omicron) == FALSE ~ rolling_average * (1/under_reporting_est[2]*omicron + 1/under_reporting_est[1]*(1-omicron)),
            date >= min(omicron_shift$date[omicron_shift$wave == 1])  ~ rolling_average * 1/under_reporting_est[2],
            date < min(omicron_shift$date[omicron_shift$wave == 1]) ~ rolling_average * 1/under_reporting_est[1])) %>%
          mutate(fit_statistic = abs(rolling_average - reported_cases)^2) %>%
          mutate(shift = this_shift,
                 beta_mod = this_beta_mod,
                 first_wave_scenario = scenario_MASTER)
        
        second_wave_range = rbind(second_wave_range,workshop)
      }
    }
  }
}
#31/03/2023 8.4 minute run time
beep()

#range for fit
# to_plot = second_wave_range %>% 
#   filter(first_wave_scenario == 2 &
#            beta_mod>=4 &
#            shift>=0 & 
#            shift<=15
#          ) %>%
#   filter(date>date_start)

to_plot = second_wave_range %>% 
  filter(first_wave_scenario == 2 &
           beta_mod %in% c(5.5) &
           shift>=0 & 
           shift<=15
  ) %>%
  filter(date>date_start)

#CHECKED: wave one scenarios loaded
# ggplot() +
#   geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(first_wave_scenario)),na.rm=TRUE) +
#   geom_point(data=to_plot,aes(x=date,y=reported_cases)) +
#   theme_bw() + 
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   xlab("") +
#   facet_grid(beta_mod ~. )

#CHECKED: beta has an impact!
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average*(0.6),color=as.factor(beta_mod),linetype = as.factor(first_wave_scenario)),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=reported_cases)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("") +
  facet_grid(shift ~. )

ggplot(NPI_estimates[NPI_estimates$date %in% to_plot$date,]) + geom_line(aes(x=date,y=NPI))

second_wave_scenarios = data.frame(beta2 = c(5.5),
                                   shift2 = c(7))
rm(scenario_MASTER)
#_______________________________________________________________________________
#can we go with the middle first wave scenario? Yes




### Save best scenario for wave two to use for wave three search ##########
fitting = "on"
strain_inital = strain_now = 'WT' 
date_start = as.Date('2021-04-30')
model_weeks = as.numeric((fit_cutoff_dates[2]-date_start-30)/7)

fitting_beta    = baseline_fitting_beta
fitting_beta[1] = first_wave_scenarios$beta1[2]
fitting_beta[2] = second_wave_scenarios$beta2

covid19_waves        = prev_fitted_covid19_waves
covid19_waves$date[1] = covid19_waves$date[1] + first_wave_scenarios$shift1[2]
covid19_waves$date[2] = covid19_waves$date[2] + second_wave_scenarios$shift2
covid19_waves = covid19_waves[c(1,2),]

strain_inital = strain_now = 'WT' 

source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)

workshop = case_history %>%
  select(date,rolling_average) %>%
  rename(reported_cases = rolling_average) %>%
  right_join(incidence_log, by = "date")  %>%
  left_join(omicron_shift[omicron_shift$wave == 1,], by = "date") %>%
  rename(omicron = percentage)%>%
  mutate(rolling_average = case_when(
    date >= min(omicron_shift$date[omicron_shift$wave == 1])  & is.na(omicron) == FALSE ~ rolling_average * (1/under_reporting_est[2]*omicron + 1/under_reporting_est[1]*(1-omicron)),
    date >= min(omicron_shift$date[omicron_shift$wave == 1])  ~ rolling_average * 1/under_reporting_est[2],
    date < min(omicron_shift$date[omicron_shift$wave == 1]) ~ rolling_average * 1/under_reporting_est[1])) %>%
  mutate(fit_statistic = abs(rolling_average - reported_cases)^2)

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
save(fitted_results, file = paste("01_inputs/fit/start_point_wave_three_",this_setting,"_",Sys.Date(),".Rdata",sep=''))

beep()

ggplot() +
  geom_line(data=workshop,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=workshop,aes(x=date,y=reported_cases)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("") 
#_______________________________________________________________________________





### Wave three search with three second wave scenarios ############################
#How long is this run?
#How far forward do we need to push wave three?
#Check loads in CommandDeck

fitting = "wave_three"

date_start = round(fit_cutoff_dates[2])-31
date_start = as.Date(date_start)
model_weeks = as.numeric((as.Date('2022-12-31') - date_start)/7) 

third_wave_range = data.frame()

for (this_beta_mod in seq(1, 5, by = 0.5)) {
  for (this_shift in seq(75,105, by = 15)) {
      if (nrow(third_wave_range[
                                third_wave_range$shift == this_shift &
                                third_wave_range$beta_mod == this_beta_mod,])>0){
        
      } else{
        
        fitting_beta    = baseline_fitting_beta
        fitting_beta[1] = first_wave_scenarios$beta1[2]
        fitting_beta[2] = second_wave_scenarios$beta2
        fitting_beta[3] = this_beta_mod
        
        covid19_waves        = prev_fitted_covid19_waves
        covid19_waves$date[1] = covid19_waves$date[1] + first_wave_scenarios$shift1[2]
        covid19_waves$date[2] = covid19_waves$date[2] + second_wave_scenarios$shift2
        covid19_waves$date[3] = covid19_waves$date[3] + this_shift

        strain_inital = strain_now = 'omicron' 
        
        source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
        
        workshop = case_history %>%
          select(date,rolling_average) %>%
          rename(reported_cases = rolling_average) %>%
          right_join(incidence_log, by = "date")  %>%
          left_join(omicron_shift[omicron_shift$wave == 1,], by = "date") %>%
          rename(omicron = percentage)%>%
          mutate(rolling_average = case_when(
            date >= min(omicron_shift$date[omicron_shift$wave == 2])  & is.na(omicron) == FALSE ~ rolling_average * (1/under_reporting_est[3]*omicron + 1/under_reporting_est[2]*(1-omicron)),
            date >= min(omicron_shift$date[omicron_shift$wave == 2])  ~ rolling_average * 1/under_reporting_est[3],
            
            date >= min(omicron_shift$date[omicron_shift$wave == 1])  & is.na(omicron) == FALSE ~ rolling_average * (1/under_reporting_est[2]*omicron + 1/under_reporting_est[1]*(1-omicron)),
            date >= min(omicron_shift$date[omicron_shift$wave == 1])  ~ rolling_average * 1/under_reporting_est[2],
            
            date < min(omicron_shift$date[omicron_shift$wave == 1]) ~ rolling_average * 1/under_reporting_est[1])) %>%
          mutate(fit_statistic = abs(rolling_average - reported_cases)^2) %>%
          mutate(shift = this_shift,
                 beta_mod = this_beta_mod)
        
        third_wave_range = rbind(third_wave_range,workshop)
    }
  }
}
#31/03/2023 8.4 minute run time
beep()

to_plot = third_wave_range %>% 
  filter(beta_mod %in% c(1.5,2,2.5,3,3.5)) %>%
  filter(date>date_start) 

#check scenario loaded!
#check case_when under reporting adjustments worked

ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average*0.5,color=as.factor(beta_mod)),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=reported_cases)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("") +
  facet_grid(shift ~. )
#_______________________________________________________________________________





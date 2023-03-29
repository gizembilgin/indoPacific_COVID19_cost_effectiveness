### View previous fit ##########################################################
fitting = "on"
strain_inital = strain_now = 'WT' 
date_start = as.Date('2021-04-30')
model_weeks = as.numeric(ceiling((as.Date('2022-12-31') - date_start)/7)) 

covid19_waves = data.frame(date = c(as.Date('2021-06-06'),as.Date('2021-10-04'),as.Date('2022-02-01')),
                           strain = c('delta','omicron','omicron'))

fitting_beta= c(0.93,1.04,1.22)
under_reporting_est = c(16.9,60,166.5)

NPI_adj = NPI_estimates$NPI[NPI_estimates$date %in% covid19_waves$date]/100
NPI_adj = NPI_adj/(1- NPI_adj)

fitting_beta = fitting_beta * NPI_adj
baseline_fitting_beta = fitting_beta

source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
beep()

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
beep()

to_plot = fitting_beta_range_plot %>% filter(date>date_start)

for (this_wave in unique())
add_rows = crossing(prev_fit,wave = c(1,2,3))

ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(adj)),na.rm=TRUE) +
  geom_line(data=prev_fit,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=reported_cases)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("") +
  facet_grid(wave ~. )

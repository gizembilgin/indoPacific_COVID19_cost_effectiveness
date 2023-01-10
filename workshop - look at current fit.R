#look at current fit

load(file = paste('1_inputs/fit/first_wave_fit',this_setting,'.Rdata',sep=''))

date_start = as.Date('2021-04-30')
strain_inital = strain_now = 'WT' 

covid19_waves = data.frame(date = 
                             c(as.Date('2021-06-09')+round(first_wave_fit$par[1]),
                               as.Date('2021-11-01'),
                               as.Date('2022-04-01')), 
                           strain = c('delta','omicron','omicron'))

model_weeks = as.numeric((Sys.Date()+1-date_start)/7)

under_reporting_est = first_wave_fit$par[2]

fitting_beta= c(first_wave_fit$par[3],
                1,
                1)

source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)

workshop = case_history %>%
  select(date,rolling_average) %>%
  mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
    rolling_average = rolling_average * under_reporting_est) %>%
  rename(adjusted_reported = rolling_average) %>%
  left_join(incidence_log, by = "date") %>%
  mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2)

fit_statistic = sum(workshop$fit_statistic,na.rm=TRUE)

to_plot = workshop %>% filter(date>date_start & date<=(date_start+model_weeks*7))
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
  plot_standard

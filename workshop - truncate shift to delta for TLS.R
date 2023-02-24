### TLS - need to fit first two waves together!
setting = this_setting = "TLS"

baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-03-01'),as.Date('2021-05-01'),as.Date('2022-01-01')),
                                                    strain = c('WT','delta','omicron'))
under_reporting_est = 45

plot_tracker = data.frame()
fit_tracker = data.frame()
workshop_incidence_log_tracker = data.frame()


for (TOGGLE_delta_truncation_factor in c(0.3,0.35,0.4)){
  for (this_beta1 in c(0.95,0.975)) {
    for (this_beta2 in c(1.1,1.2)) {
      for (this_shift1 in c(-60)) {
        for (this_shift2 in c(45,60,75)) {
          if (nrow(plot_tracker[plot_tracker$delta_truncation == TOGGLE_delta_truncation_factor &
                                plot_tracker$beta1 == this_beta1 &
                                plot_tracker$beta2 == this_beta2 &
                                plot_tracker$shift1 == this_shift1 &
                                plot_tracker$shift2 == this_shift2, ]) > 0) {
            #skip
          } else{
            
            strain_inital = strain_now = 'WT' 
            fitting_beta = c(this_beta1,
                             this_beta2,
                             1)
            
            covid19_waves = baseline_covid19_waves
            covid19_waves$date[1] = covid19_waves$date[1] + this_shift1
            covid19_waves$date[2] = covid19_waves$date[2] + this_shift2
            date_start = covid19_waves$date[1] - 2
            model_weeks = as.numeric((covid19_waves$date[3] - date_start)/7)
            
            source(paste(getwd(), "/CommandDeck.R", sep = ""))
            
            workshop = case_history %>%
              select(date, rolling_average) %>%
              mutate(rolling_average =  rolling_average * under_reporting_est) %>%
              rename(adjusted_reported = rolling_average) %>%
              left_join(incidence_log, by = "date") %>%
              mutate(
                fit_statistic = abs(rolling_average - adjusted_reported) ^ 2 ,
                delta_truncation = TOGGLE_delta_truncation_factor,
                beta1 = this_beta1,
                beta2 = this_beta2,
                shift1 = this_shift1,
                shift2 = this_shift2
              )
            
            fit_statistic = data.frame(
              fit = sum(workshop$fit_statistic,na.rm = TRUE),
              delta_truncation = TOGGLE_delta_truncation_factor,
              beta1 = this_beta1,
              beta2 = this_beta2,
              shift1 = this_shift1,
              shift2 = this_shift2
            )
            
            incidence_log = incidence_log %>%
              mutate(
                delta_truncation = TOGGLE_delta_truncation_factor,
                beta1 = this_beta1,
                beta2 = this_beta2,
                shift1 = this_shift1,
                shift2 = this_shift2
              )
            
            plot_tracker = rbind(plot_tracker, workshop)
            fit_tracker = rbind(fit_tracker, fit_statistic)
            
            workshop_incidence_log_tracker = rbind(workshop_incidence_log_tracker, incidence_log)
          }
        }
      }
    }
  }
}

under_reporting_est_w1 = 65
under_reporting_est_w2 = 28
reported_cases_plot = case_history[case_history$date<=max(to_plot$date) & case_history$date>=min(to_plot$date),] %>%
  mutate(rolling_average = case_when(
    date<as.Date('2021-08-01') ~ rolling_average * under_reporting_est_w1,
    TRUE ~ rolling_average * under_reporting_est_w2
  ))
#TLS_fit_tracker = fit_tracker; TLS_fit_tracker;TLS_fit_tracker[TLS_fit_tracker$fit == min(TLS_fit_tracker$fit),]

TLS_tracker = plot_tracker %>%
  filter(date<= max(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           date>= min(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE])) %>%
  filter(delta_truncation %in% c(0.3) &
           shift1 == -60 &
           shift2 %in% c(60) &
           beta2 %in% c(1.2) &
           beta1 %in% c(0.975)
           )
to_plot = TLS_tracker  #%>% filter(beta2 == 2  & !(beta1 %in% c(0.7,0.8,2,1.5,0.9,1.2))& shift2 == 30)
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(delta_truncation),linetype=as.factor(beta1)),na.rm=TRUE) +
  geom_point(data=reported_cases_plot,aes(x=date,y=rolling_average)) +
  plot_standard +
  labs(color = 'delta_truncation',linetype='beta1') + 
  facet_grid(beta2 ~. )



under_reporting_est_w1 = 50
under_reporting_est_w2 = 28
reported_cases_plot = case_history[case_history$date<=max(to_plot$date) & case_history$date>=min(to_plot$date),] %>%
  mutate(rolling_average = case_when(
    date<as.Date('2021-08-01') ~ rolling_average * under_reporting_est_w1,
    TRUE ~ rolling_average * under_reporting_est_w2
  ))

TLS_tracker = plot_tracker %>%
  filter(date<= max(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           date>= min(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE])) %>%
  filter(delta_truncation %in% c(0.25) &
           shift1 == -60 &
           shift2 %in% c(60) &
           beta2 %in% c(1) &
           beta1 %in% c(0.925)
  )
to_plot = TLS_tracker  #%>% filter(beta2 == 2  & !(beta1 %in% c(0.7,0.8,2,1.5,0.9,1.2))& shift2 == 30)
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(delta_truncation),linetype=as.factor(beta1)),na.rm=TRUE) +
  geom_point(data=reported_cases_plot,aes(x=date,y=rolling_average)) +
  plot_standard +
  labs(color = 'delta_truncation',linetype='beta1') + 
  facet_grid(beta2 ~. )

beep()
save(plot_tracker, file = paste('1_inputs/fit/TLS_tracker.Rdata',sep=''))




###proj forward to best choices to omicron
scenario = list(c(0.3,-60,60,0.975,1.2,65,28),
                c(0.25,-60,60,0.925,1,50,28))

fit_tracker_FINAL = data.frame()
plot_tracker_FINAL = data.frame()

for (this_scenario in 1:length(scenario)){
  strain_inital = strain_now = 'WT' 
  fitting_beta = c(scenario[[this_scenario]][4],
                   scenario[[this_scenario]][5],
                   1)
  
  covid19_waves = baseline_covid19_waves
  covid19_waves$date[1] = covid19_waves$date[1] + scenario[[this_scenario]][2]
  covid19_waves$date[2] = covid19_waves$date[2] + scenario[[this_scenario]][3]
  date_start = covid19_waves$date[1] - 2
  model_weeks = as.numeric((as.Date('2022-06-01') - date_start)/7)
  
  TOGGLE_delta_truncation_factor = scenario[[this_scenario]][1]
  
  source(paste(getwd(), "/CommandDeck.R", sep = ""))
  
  workshop = case_history %>%
    select(date, rolling_average) %>%
    mutate(rolling_average  = case_when(
      date<as.Date('2021-08-01') ~ rolling_average * scenario[[this_scenario]][6],
      TRUE ~ rolling_average * scenario[[this_scenario]][7]
    )) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(
      fit_statistic = abs(rolling_average - adjusted_reported) ^ 2 ,
      scenario = this_scenario
    )
  
  fit_statistic = data.frame(
    fit = sum(workshop$fit_statistic,na.rm = TRUE),
    scenario = this_scenario
  )
  
  incidence_log = incidence_log %>%
    mutate(
      scenario = this_scenario
    )
  
  plot_tracker_FINAL = rbind(plot_tracker_FINAL, workshop)
  fit_tracker_FINAL = rbind(fit_tracker_FINAL, fit_statistic)
  
  
}

to_plot = plot_tracker_FINAL %>% filter(date>date_start & date<=(date_start+model_weeks*7))
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
  plot_standard +
  facet_grid(scenario ~. )







setting = this_setting = "TLS"

baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-03-01'),as.Date('2021-05-01'),as.Date('2021-11-01')),
                                                    strain = c('WT','delta','omicron'))


### Let's visualise NPI and case history together
plot_1 = ggplot() + 
  geom_line(data = NPI_estimates[NPI_estimates$date>as.Date('2021-01-01') & NPI_estimates$date<as.Date('2022-07-01'),], aes(x=date,y=(100-NPI))) 

plot_2 = ggplot() +
  geom_point(data=case_history[case_history$date>as.Date('2021-01-01') & case_history$date<as.Date('2022-07-01'),],aes(x=date,y=rolling_average)) +
  geom_vline(xintercept = as.Date('2021-03-01'),linetype = "dashed") +
  geom_vline(xintercept = as.Date('2021-07-01'),linetype = "dashed")  +
  geom_vline(xintercept = as.Date('2021-07-01')-30,linetype = "dashed")  +
  geom_vline(xintercept = as.Date('2021-07-01')+15,linetype = "dashed") #+
  #geom_vline(xintercept = covid19_waves$date[3])

grid.arrange(plot_1,plot_2)

#beta_fitted_values
#Reff in January is 0.97 with beta1 = 1.5
#_______________________________________________________________________________



### Let's search for the start of the first wave ###############################
first_wave_search = data.frame()
for (this_shift1 in c(0,-7,-15)) {
  for (this_beta1 in c(1.5,1.75,2,2.25,2.5)) {
    if (nrow(first_wave_search[first_wave_search$beta1 == this_beta1 &
                               first_wave_search$shift1 == this_shift1 , ]) > 0) {
      #skip
    } else{
      
      strain_inital = strain_now = 'WT'
      fitting_beta[1] = this_beta1
      
      covid19_waves = baseline_covid19_waves
      covid19_waves$date[1] = covid19_waves$date[1] + this_shift1
      date_start = covid19_waves$date[1] - 2
      model_weeks = as.numeric((as.Date('2021-07-01')+15 - date_start)/7)
      
      #covid19_waves = covid19_waves[1,]
      
      source(paste(getwd(), "/CommandDeck.R", sep = ""))
      
      workshop = case_history %>%
        select(date, rolling_average) %>%
        mutate(rolling_average =  rolling_average * under_reporting_est) %>%
        rename(adjusted_reported = rolling_average) %>%
        left_join(incidence_log, by = "date") %>%
        mutate(
          fit_statistic = abs(rolling_average - adjusted_reported) ^ 2 ,
          beta1 = this_beta1,
          shift1 = this_shift1
        )

      first_wave_search = rbind(first_wave_search, workshop)
    }
  }
}

under_reporting_est_w1 = 75
reported_cases_plot = case_history[case_history$date<=as.Date('2021-07-01') & case_history$date>=covid19_waves$date[1]-60,] %>%
  mutate(rolling_average = rolling_average * under_reporting_est_w1)

to_plot = first_wave_search %>%
  filter(date<= max(first_wave_search$date[is.na(first_wave_search$rolling_average) == FALSE]) & 
           date>= min(first_wave_search$date[is.na(first_wave_search$rolling_average) == FALSE])) %>%
  filter(shift1>-16 &
           beta1>1.75)
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(beta1),na.rm=TRUE)) +
  geom_point(data=reported_cases_plot,aes(x=date,y=rolling_average)) +
  plot_standard +
  labs(color = 'beta1') + 
  facet_grid(shift1 ~. )

save(first_wave_search, file = paste('1_inputs/fit/TLS_first_wave_search.Rdata',sep=''))
#_______________________________________________________________________________



### Let's look at the first two waves together! ################################
plot_tracker = data.frame()

for (this_shift1 in c(-15,-7,0)) {
  for (this_beta1 in c(2,2.25,2.5)) {
    for (TOGGLE_delta_truncation_factor in c(0.15,0.25,0.33)){ #(0.15,0.25,0.33,0.5
      for (this_shift2 in c(60,75,90)) {
        for (this_beta2 in c(3.5,3,2.5)) { #3.5,3,2.5,2,1.5,1
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

under_reporting_est_w1 = 75
under_reporting_est_w2 = 40
reported_cases_plot = case_history[case_history$date<=max(to_plot$date) & case_history$date>=min(to_plot$date),] %>%
  mutate(rolling_average = case_when(
    date<as.Date('2021-08-01') ~ rolling_average * under_reporting_est_w1,
    TRUE ~ rolling_average * under_reporting_est_w2
  ))
#TLS_fit_tracker = fit_tracker; TLS_fit_tracker;TLS_fit_tracker[TLS_fit_tracker$fit == min(TLS_fit_tracker$fit),]

TLS_tracker = plot_tracker %>%
  filter(date<= max(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           date>= min(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE])) 
to_plot = TLS_tracker  %>% 
  filter(shift1 == -7 &
           beta1 == 2.25 &
           #shift2 == 60
           delta_truncation == 0.15
         )

ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(beta2),na.rm=TRUE)) +
  geom_point(data=reported_cases_plot,aes(x=date,y=rolling_average)) +
  plot_standard +
  labs(color = 'beta2') + 
  facet_grid(shift2 ~. )

beep()
save(plot_tracker, file = paste('1_inputs/fit/TLS_second_wave_search.Rdata',sep=''))
#_______________________________________________________________________________



### Let's include the third wave! ##############################################
###proj forward to best choices to omicron
scenario = list(c(0.15,-7,80,2.25,3,75,60),
                c(0.15,-0,80,2.5,3,75,60))

plot_tracker_FINAL = data.frame()

for (this_scenario in 1:length(scenario)){
  strain_inital = strain_now = 'WT' 
  fitting_beta = c(scenario[[this_scenario]][4],
                   scenario[[this_scenario]][5],
                   2.5)
  
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
  
  incidence_log = incidence_log %>%
    mutate(
      scenario = this_scenario
    )
  
  plot_tracker_FINAL = rbind(plot_tracker_FINAL, workshop)
}

to_plot = plot_tracker_FINAL %>% filter(date>date_start & date<=(date_start+model_weeks*7))
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
  plot_standard +
  facet_grid(scenario ~. )
#save(plot_tracker_FINAL, file = paste('1_inputs/fit/TLS_third_wave_search.Rdata',sep=''))
#load(file = paste('1_inputs/fit/TLS_third_wave_search.Rdata',sep=''))

#NOTE: third wave takes off without seeding due to drop in NPI (scenario = 3 in plot_tracker_FINAL)
#DECISION (March 2023): let's 
#(1) roughly fit the first two waves, to then 
#(2) search for parameter ranges of the third wave, before
#(3) setting all waves to fit

#REVISED DECISION (April 2023): fitting all waves together pushes the third wave down to zero, let's
#(1) fit the first two waves, to then
#(2) search for paramteer ranges of the third wave, before
#(3) fitting the third wave independently
#_______________________________________________________________________________



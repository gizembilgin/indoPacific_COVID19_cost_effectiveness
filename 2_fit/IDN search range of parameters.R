### Let's visualise NPI and case history together
plot_1 = ggplot() + 
  geom_line(data = NPI_estimates[NPI_estimates$date>as.Date('2021-04-16') & NPI_estimates$date<as.Date('2022-07-01'),], aes(x=date,y=(100-NPI))) 

plot_2 = ggplot() +
  geom_point(data=case_history[case_history$date>as.Date('2021-04-16') & case_history$date<as.Date('2022-07-01'),],aes(x=date,y=rolling_average)) +
  geom_vline(xintercept = covid19_waves$date[1],linetype = "dashed") +
  geom_vline(xintercept = covid19_waves$date[2],linetype = "dashed")  +
  geom_vline(xintercept = covid19_waves$date[2]+90,linetype = "dashed")

grid.arrange(plot_1,plot_2)

#beta_fitted_values
#Reff in April 2021 is 0.813 for delta with fitting_beta = 3, but will get higher as rho diminishes
#_______________________________________________________________________________



### Let's search for the start of the first wave ###############################
first_wave_search = data.frame()
for (this_shift1 in c(30)) { #0,15,21,30
  for (this_beta1 in c(2.5)) { #1.75,2,2.25,2.5
    if (nrow(first_wave_search[first_wave_search$beta1 == this_beta1 &
                               first_wave_search$shift1 == this_shift1 , ]) > 0) {
      #skip
    } else{
      
      strain_inital = strain_now = 'WT'
      fitting_beta[1] = this_beta1
      
      covid19_waves = baseline_covid19_waves
      covid19_waves$date[1] = covid19_waves$date[1] + this_shift1
      date_start = covid19_waves$date[1] - 2
      model_weeks = as.numeric((as.Date('2021-10-15') - date_start)/7)
      
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

under_reporting_est_w1 = 50
under_reporting_est_w2 = 100
reported_cases_plot = case_history[case_history$date<=max(first_wave_search$date[is.na(first_wave_search$rolling_average)==FALSE]) & case_history$date>=covid19_waves$date[1]-14,] %>%
  mutate(rolling_average = case_when(
    date < covid19_waves$date[2] ~ rolling_average * under_reporting_est_w1,
    date >= covid19_waves$date[2] ~ rolling_average * under_reporting_est_w2
  ))

to_plot = first_wave_search %>%
  filter(date<= max(first_wave_search$date[is.na(first_wave_search$rolling_average) == FALSE]) & 
           date>= min(first_wave_search$date[is.na(first_wave_search$rolling_average) == FALSE]) &
           beta1<3 & beta1>1.5) 
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(beta1),na.rm=TRUE)) +
  geom_point(data=reported_cases_plot,aes(x=date,y=rolling_average)) +
  plot_standard +
  labs(color = 'beta1') + 
  facet_grid(shift1 ~. )

save(first_wave_search, file = paste('1_inputs/fit/IDN_first_wave_search.Rdata',sep=''))
#_______________________________________________________________________________


### Let's include the second wave!

strain_inital = strain_now = 'WT' 

baseline_covid19_waves = data.frame(date = c(as.Date('2021-04-01'),as.Date('2021-10-15')),
                                    strain = c('delta','omicron'))
fitting_beta= rep(1,nrow(baseline_covid19_waves))
under_reporting_est = 70

plot_tracker = data.frame()
fit_tracker = data.frame()
workshop_incidence_log_tracker = data.frame()

for (this_beta1 in  c(2,2.25,2.5)){
  for (this_shift1 in c(14,21,30)){
    for (this_shift2 in c(30,37,45,53,60,67,75)) {
      for (this_beta2 in c(3.5,3,2.5,2)) {
        if(nrow(plot_tracker[
                             plot_tracker$beta1 == this_beta1 &
                             plot_tracker$beta2 == this_beta2 &
                             plot_tracker$shift1 == this_shift1 &
                             plot_tracker$shift2 == this_shift2, ]) > 0) {
          #skip
        } else{
          strain_inital = strain_now = 'WT' 
          
          fitting_beta[1]= this_beta1
          fitting_beta[2] = this_beta2
          
          covid19_waves = baseline_covid19_waves 
          covid19_waves$date[1] = covid19_waves$date[1] + this_shift1
          covid19_waves$date[2] = covid19_waves$date[2] + this_shift2
          
          date_start = covid19_waves$date[1] - 7
          model_weeks = as.numeric((as.Date('2022-06-01')-date_start)/7)
          
          source(paste(getwd(),"/CommandDeck.R",sep=""))
          
          workshop = case_history %>%
            select(date,rolling_average) %>%
            mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
              rolling_average =  rolling_average * under_reporting_est) %>%
            rename(adjusted_reported = rolling_average) %>%
            left_join(incidence_log, by = "date") %>%
            mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2 ,
                   beta1 = this_beta1,
                   beta2 = this_beta2,
                   shift1 = this_shift1,
                   shift2 = this_shift2)
          
          fit_statistic = data.frame(fit = sum(workshop$fit_statistic, #fit only after first wave
                                               na.rm=TRUE),
                                     beta1 = this_beta1,
                                     beta2 = this_beta2,
                                     shift1 = this_shift1,
                                     shift2 = this_shift2)
          
          incidence_log = incidence_log %>%
            mutate(                beta1 = this_beta1,
                                   beta2 = this_beta2,
                                   shift1 = this_shift1,
                                   shift2 = this_shift2)
          
          plot_tracker = rbind(plot_tracker,workshop)
          fit_tracker = rbind(fit_tracker,fit_statistic)
          
          workshop_incidence_log_tracker = rbind(workshop_incidence_log_tracker,incidence_log)
        }
      }
    }
  }
}
beta_fit_tracker = fit_tracker; beta_fit_tracker
beta_tracker = plot_tracker %>%
  filter(date<= max(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           date>= min(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) &
           #shift1 %in% c(14,21,30) &
           #beta1 %in% c(2,2.25,2.5) &
           beta1 == 2.25 &
           shift1 == 21 #&
           #beta2 %in% c(3) &
           #shift2 %in% c(60) 
           ) 


under_reporting_est_w1 = 50
under_reporting_est_w2 = 100
reported_cases_plot = case_history[case_history$date<=max(beta_tracker$date[is.na(beta_tracker$rolling_average)==FALSE]) & case_history$date>=covid19_waves$date[1]-14,] %>%
  mutate(rolling_average = case_when(
    date < covid19_waves$date[2] ~ rolling_average * under_reporting_est_w1,
    date >= covid19_waves$date[2] ~ rolling_average * under_reporting_est_w2
  ))

ggplot() +
  geom_line(data=beta_tracker,aes(x=date,y=rolling_average,color=as.factor(beta2)),na.rm=TRUE) +
  geom_point(data=reported_cases_plot,aes(x=date,y=rolling_average)) +
  plot_standard + 
  labs(color = 'beta modifier')+ 
  facet_grid(shift2 ~ .) 
save(plot_tracker, file = paste('1_inputs/fit/IDN_second_wave_search.Rdata',sep=''))
#load(file = paste('1_inputs/fit/IDN_second_wave_search.Rdata',sep=''))

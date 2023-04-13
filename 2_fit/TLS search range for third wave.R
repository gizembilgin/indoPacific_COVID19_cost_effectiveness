
### Load rough wave one and two
list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("TLS_rough_fit","*",sep=''))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste("1_inputs/fit/",latest_file,sep=''))
#_______________________________________________________________________________



### Let's include the third wave! ##############################################
third_wave_tracker = data.frame()

for (this_shift3 in seq(30,60,by=15)) {
  # for (this_beta3 in c(0.5,seq(0.7,1,by=0.1),seq(2,6,by=1))) {
  for (this_beta3 in seq(0.5,2,by=0.5)) {
    
    if (nrow(third_wave_tracker[third_wave_tracker$shift3 == this_shift3 &
                                third_wave_tracker$beta3 == this_beta3, ]) > 0) {
      #skip
    } else{
      fitting = "wave_three"
      
      date_start = baseline_covid19_waves$date[3]-1
      #model_weeks = as.numeric((as.Date('2023-01-01') - date_start)/7)
      model_weeks = as.numeric((as.Date('2024-01-01') - date_start)/7)
      
      
      strain_inital = strain_now = 'delta' 
      
      TOGGLE_delta_truncation_factor = rough_fit$optim$bestmem[1]
      fitting_beta = c(rough_fit$optim$bestmem[2],
                       rough_fit$optim$bestmem[3],
                       this_beta3)
      
      covid19_waves = baseline_covid19_waves
      covid19_waves$date[1] = covid19_waves$date[1] + round(rough_fit$optim$bestmem[4])
      covid19_waves$date[2] = covid19_waves$date[2] + round(rough_fit$optim$bestmem[5])
      covid19_waves$date[3] = covid19_waves$date[3] + this_shift3
      
      
      source(paste(getwd(), "/CommandDeck.R", sep = "")) #10 minutes
      
      workshop = case_history %>%
        select(date, rolling_average) %>%
        mutate(rolling_average  = case_when(
          date<as.Date('2021-08-01') ~ rolling_average * under_reporting_wave1,
          TRUE ~ rolling_average * under_reporting_wave2
        )) %>%
        rename(adjusted_reported = rolling_average) %>%
        right_join(incidence_log, by = "date") %>%
        mutate(
          fit_statistic = abs(rolling_average - adjusted_reported) ^ 2 ,
          beta3 = this_beta3,
          shift3 = this_shift3
        )
      
      third_wave_tracker = rbind(third_wave_tracker, workshop)
    }
  }
}

to_plot = third_wave_tracker %>%
  filter(
    shift3>15 & shift3==60 &
      beta3<=2 &
    date> baseline_covid19_waves$date[3] #&
           #date<max(third_wave_tracker$date[is.na(third_wave_tracker$rolling_average)==FALSE],na.rm=TRUE)
    )
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(beta3)),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
  plot_standard +
  facet_grid(shift3 ~. )  +
  geom_vline(xintercept = baseline_covid19_waves$date[3] + ceiling(365/2))
beep()
#save(third_wave_tracker, file = paste('1_inputs/fit/TLS_third_wave_search.Rdata',sep=''))
#load(file = paste('1_inputs/fit/TLS_third_wave_search.Rdata',sep=''))
#_______________________________________________________________________________




### Let's have another look at beta2! ##############################################
middle_search = data.frame()

for (this_beta3 in seq(1,6)) {
  for (this_beta2 in seq(2,6)) {
      
      if (nrow(middle_search[middle_search$beta2 == this_beta2 &
                             middle_search$beta3 == this_beta3, ]) > 0) {
        #skip
      } else{
        
        fitting = "on"
  
        strain_inital = strain_now = 'WT' 
        
        TOGGLE_delta_truncation_factor = rough_fit$optim$bestmem[1]
        
        fitting_beta = c(rough_fit$optim$bestmem[2],
                         this_beta2,
                         this_beta3)
        
        covid19_waves = baseline_covid19_waves
        covid19_waves$date[1] = covid19_waves$date[1] + round(rough_fit$optim$bestmem[4])
        covid19_waves$date[2] = covid19_waves$date[2] + round(rough_fit$optim$bestmem[5])
        covid19_waves$date[3] = covid19_waves$date[3] + 60
        
        date_start = covid19_waves$date[1] - 2
        model_weeks = as.numeric((as.Date('2022-04-01') - date_start)/7) 
      
  
        
        source(paste(getwd(), "/CommandDeck.R", sep = "")) #10 minutes
        
        workshop = case_history %>%
          select(date, rolling_average) %>%
          mutate(rolling_average  = case_when(
            date<as.Date('2021-08-01') ~ rolling_average * under_reporting_wave1,
            TRUE ~ rolling_average * under_reporting_wave2
          )) %>%
          rename(adjusted_reported = rolling_average) %>%
          left_join(incidence_log, by = "date") %>%
          mutate(
            fit_statistic = abs(rolling_average - adjusted_reported) ^ 2 ,
            beta2 = this_beta2,
            beta3 = this_beta3
          )
        
        middle_search = rbind(middle_search, workshop)
    }
  }
}

to_plot = middle_search %>%
  filter(
           date> baseline_covid19_waves$date[1] &
             date<max(middle_search$date[is.na(middle_search$rolling_average)==FALSE],na.rm=TRUE))
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(beta2)),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
  plot_standard +
  facet_grid(beta3 ~.)
#save(third_wave_tracker, file = paste('1_inputs/fit/TLS_third_wave_search.Rdata',sep=''))
#load(file = paste('1_inputs/fit/TLS_third_wave_search.Rdata',sep=''))
#_______________________________________________________________________________






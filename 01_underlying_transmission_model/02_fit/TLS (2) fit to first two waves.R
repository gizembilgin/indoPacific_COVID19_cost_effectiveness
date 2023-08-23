fit_firstTwo_waves <- function(par){
  
  fitting = "on"
  strain_inital = strain_now = 'WT' 
  
  TOGGLE_delta_truncation_factor = par[1]
  
  fitting_beta = c(par[2],
                   par[3])
  
  covid19_waves = baseline_covid19_waves
  covid19_waves$date[1] = covid19_waves$date[1] + round(par[4])
  covid19_waves$date[2] = covid19_waves$date[2] + round(par[5])
  
  date_start = covid19_waves$date[1] - 2
  model_weeks = as.numeric((covid19_waves$date[3]  - date_start)/7)
  
  source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE) #5 minutes!
  
  #quick search under reporting
  increments_list = c(100,50,10,5,1,0.25)
  underreporting_tracker = data.frame()
  
  for (repeat_through in 1:length(increments_list)){
    
    increment = increments_list[repeat_through]
    
    if (repeat_through == 1){
      search_list1 = search_list2 = seq(50,1000,by=increments_list[repeat_through])
    } else{
      best_so_far = underreporting_tracker[underreporting_tracker$fit== min(underreporting_tracker$fit, na.rm=TRUE),]
      best_so_far = unique(best_so_far)
      if (nrow(best_so_far)>1){ #pick best_so_far with min under reporting
        best_so_far = best_so_far %>% mutate(under_reporting_mean = (wave1+wave2)/3)
        best_so_far = best_so_far[best_so_far$under_reporting_mean == min(best_so_far$under_reporting_mean),]
      }
      
      search_list1 = seq(best_so_far$wave1 - increments_list[repeat_through-1],
                         best_so_far$wave1 + increments_list[repeat_through-1],
                         by = increments_list[repeat_through])
      search_list2 = seq(best_so_far$wave2 - increments_list[repeat_through-1],
                         best_so_far$wave2 + increments_list[repeat_through-1],
                         by = increments_list[repeat_through])
    }
    
    for(under_reporting_wave1 in search_list1){
      for (under_reporting_wave2 in search_list2){
          
        workshop = case_history %>%
          select(date,rolling_average) %>%
          rename(reported_cases = rolling_average) %>%
          right_join(incidence_log, by = "date") %>%
          left_join(delta_shift, by = "date") %>%
          rename(delta = percentage) %>%
          mutate(
            rolling_average = case_when(
              date >= min(delta_shift$date)  & is.na(delta) == FALSE ~ rolling_average * (1/under_reporting_wave2*delta + 1/under_reporting_wave1*(1-delta)),
              date >= min(delta_shift$date)  ~ rolling_average * 1/under_reporting_wave2,
              date < min(delta_shift$date) ~ rolling_average * 1/under_reporting_wave1)) %>%
          mutate(fit_statistic = abs(rolling_average - reported_cases)^2)
          
          fit_statistic = data.frame(
            fit = sum(workshop$fit_statistic,
                      na.rm=TRUE),
            wave1 = under_reporting_wave1,
            wave2 = under_reporting_wave2)
          
          underreporting_tracker = rbind(underreporting_tracker,fit_statistic)
      }
    }
  }
  
  
  fit_statistic = min(underreporting_tracker$fit, na.rm=TRUE)
  
  return(fit_statistic)
}

#plot to check function
best_fit = underreporting_tracker[underreporting_tracker$fit == min(underreporting_tracker$fit),]
under_reporting_wave1 = best_fit$wave1
under_reporting_wave2 = best_fit$wave2

ggplot() +
  geom_line(data=workshop,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=workshop,aes(x=date,y=reported_cases)) +
  plot_standard
#____________________


### Fit!
require(DEoptim)
rough_fit <- DEoptim(fn = fit_firstTwo_waves,
                    lower = c(0.1,
                              2,2,
                              -14,60
                    ),
                    upper = c(0.5,
                              4,6,
                              7,90
                    ),
                    control = list(NP = 50,
                                   itermax = 10,
                                   storepopfrom = 1)) 
save(rough_fit, file = paste('01_inputs/fit/TLS_rough_fit',Sys.Date(),'.Rdata',sep=''))
#____________________



### Explore fit
summary(rough_fit)
plot(rough_fit, plot.type = "bestvalit")
plot(rough_fit, plot.type ="bestmemit")
plot(rough_fit, plot.type ="storepop")
to_plot = as.data.frame(rough_fit$member$pop)
colnames(to_plot) <- c('delta_trunc','beta1','beta2','shift1','shift2')
ggplot(to_plot) + geom_histogram(aes(x=delta_trunc),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=beta1),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=beta2),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=shift1),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=shift2),bins=10)
ggplot(to_plot) + geom_point(aes(x=beta1,y=shift1))
ggplot(to_plot) + geom_point(aes(x=beta2,y=shift2))
ggplot(to_plot) + geom_point(aes(x=beta2,y=delta_trunc))
#_________________________________________________


### Save fit for search of third wave
par = rough_fit$optim$bestmem
#model_weeks = as.numeric((covid19_waves$date[3]  - date_start)/7)
incidence_log = incidence_log %>% select(date,daily_cases)

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
save(fitted_results, file = paste("01_inputs/fit/start_point_wave_three_",this_setting,Sys.Date(),".Rdata",sep=""))
#____________________
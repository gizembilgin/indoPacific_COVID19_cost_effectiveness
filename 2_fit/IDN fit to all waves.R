fit_all_waves <- function(par){
  
  fitting = "on"
  strain_inital = strain_now = 'WT' 
  
  fitting_beta = c(par[1],
                   par[2])
  
  covid19_waves = baseline_covid19_waves
  covid19_waves$date[1] = covid19_waves$date[1] + round(par[3])
  covid19_waves$date[2] = covid19_waves$date[2] + round(par[4])
  
  date_start = covid19_waves$date[1] - 2
  model_weeks = as.numeric((fit_cutoff_dates[2] - date_start)/7)
  
  source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE) #20 minutes
  
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
          left_join(omicron_shift, by = "date") %>%
          rename(omicron = percentage) %>%
          mutate(rolling_average = case_when(
            date >= min(omicron_shift$date[omicron_shift$wave == 1])  & is.na(omicron) == FALSE ~ rolling_average * (1/under_reporting_wave2*omicron + 1/under_reporting_wave1*(1-omicron)),
            date >= min(omicron_shift$date[omicron_shift$wave == 1])  ~ rolling_average * 1/under_reporting_wave2,
            
            date < min(omicron_shift$date[omicron_shift$wave == 1]) ~ rolling_average * 1/under_reporting_wave1)) %>%
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

  fit_statistic = min(underreporting_tracker$fit,na.rm=TRUE)
  
  return(fit_statistic)
}

these_waves = underreporting_tracker[underreporting_tracker$fit == min(underreporting_tracker$fit),]
under_reporting_wave2 = these_waves$wave2
under_reporting_wave1 = these_waves$wave1
ggplot() +
  geom_line(data=workshop,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=workshop,aes(x=date,y=reported_cases)) +
  plot_standard
ggplot() + 
  geom_line(data=incidence_log,aes(x=date,y=rolling_average))


require(DEoptim)
#first round by Monday (4 days away) so roughly 100 runs
full_fit <- DEoptim(fn = fit_all_waves,
                    lower = c(2,2,
                              0,45
                    ),
                    upper = c(4,6,
                              45,75
                    ),
                    control = list(NP = 20,
                                   itermax = 10,
                                   storepopfrom = 1)) 
save(full_fit, file = paste('1_inputs/fit/full_fit',this_setting,Sys.Date(),'.Rdata',sep=''))

### Explore fit
summary(full_fit)
plot(full_fit, plot.type = "bestvalit")
#plot(full_fit, plot.type ="bestmemit")
plot(full_fit, plot.type ="storepop")
to_plot = as.data.frame(full_fit$member$pop)
colnames(to_plot) <- c('beta1','beta2','seed1','seed2')
ggplot(to_plot) + geom_histogram(aes(x=beta1),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=beta2),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=seed1),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=seed2),bins=10)
ggplot(to_plot) + geom_point(aes(x=beta1,y=seed1))
#_________________________________________________
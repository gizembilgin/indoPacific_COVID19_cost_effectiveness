fit_all_waves <- function(par){
  
  fitting = "on"
  strain_inital = strain_now = 'WT' 
  
  TOGGLE_delta_truncation_factor = par[1]
  
  fitting_beta = c(par[2],
                   par[3]
  )
  
  covid19_waves = baseline_covid19_waves
  covid19_waves$date[1] = covid19_waves$date[1] + round(par[4])
  covid19_waves$date[2] = covid19_waves$date[2] + round(par[5])
  
  date_start = covid19_waves$date[1] - 2
  model_weeks = as.numeric((covid19_waves$date[3]  - date_start)/7)
  
  source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
  
  
  #quick search under reporting
  search_list = seq(20,120,by=10)
  
  underreporting_tracker = data.frame()
  for(under_reporting_wave1 in search_list){
    for (under_reporting_wave2 in search_list){
      
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
        fit = sum(workshop$fit_statistic, na.rm=TRUE),
        wave1 = under_reporting_wave1,
        wave2 = under_reporting_wave2)
      
      underreporting_tracker = rbind(underreporting_tracker,fit_statistic)
      #}
    }
  }
  
  #detailed search under reporting
  best_so_far = underreporting_tracker[underreporting_tracker$fit== min(underreporting_tracker$fit, na.rm=TRUE),]
  for(under_reporting_wave1 in seq(best_so_far$wave1 -10,best_so_far$wave1 + 10)){
    for(under_reporting_wave2 in seq(best_so_far$wave2 -10,best_so_far$wave2 + 10)){
      #for(under_reporting_wave3 in seq(best_so_far$wave3 -10,best_so_far$wave3 + 10)){
      
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
        fit = sum(workshop$fit_statistic, na.rm=TRUE),
        wave1 = under_reporting_wave1,
        wave2 = under_reporting_wave2)
      
      underreporting_tracker = rbind(underreporting_tracker,fit_statistic)
      
      
      #}
    }
  }
  
  fit_statistic = min(underreporting_tracker$fit, na.rm=TRUE)
  
  return(fit_statistic)
}

best_fit = underreporting_tracker[underreporting_tracker$fit == min(underreporting_tracker$fit),]
under_reporting_wave1 = best_fit$wave1
under_reporting_wave2 = best_fit$wave2

ggplot() +
  geom_line(data=workshop,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=workshop,aes(x=date,y=reported_cases)) +
  plot_standard



require(DEoptim)
#Run until next Monday ~ 3 days ~ 150 runs
rough_fit <- DEoptim(fn = fit_all_waves,
                    lower = c(0.1,
                              2,2,
                              -14,60
                    ),
                    upper = c(0.5,
                              3,4,
                              7,90
                    ),
                    control = list(NP = 15,
                                   itermax = 10,
                                   storepopfrom = 1)) 
save(rough_fit, file = paste('1_inputs/fit/TLS_rough_fit',Sys.Date(),'.Rdata',sep=''))

### Explore fit
summary(rough_fit)
plot(rough_fit, plot.type = "bestvalit")
#plot(rough_fit, plot.type ="bestmemit")
plot(rough_fit, plot.type ="storepop")
to_plot = as.data.frame(rough_fit$member$pop)
colnames(to_plot) <- c('seed_date','under_reporting','beta_modifier')
ggplot(to_plot) + geom_histogram(aes(x=seed_date),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=under_reporting),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=beta_modifier),bins=10)
ggplot(to_plot) + geom_point(aes(x=beta_modifier,y=under_reporting))
#_________________________________________________
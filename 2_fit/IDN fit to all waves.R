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
  
  source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
  
  
  #quick search under reporting
  search_interaval = 10
  search_list = seq(20,120,by=search_interaval)
  
  underreporting_tracker = data.frame()
  for(under_reporting_wave1 in search_list){
    for (under_reporting_wave2 in search_list){
        
        workshop = case_history %>%
          select(date,rolling_average) %>%
          rename(reported_cases = rolling_average) %>%
          right_join(incidence_log, by = "date") %>%
          left_join(omicron_shift, by = "date") %>%
          rename(omicron = percentage) %>%
          # mutate(
          #   rolling_average = case_when(
          #     date>= min(omicron_shift$date) & is.na(omicron) == FALSE ~ rolling_average * (1/under_reporting_wave2*omicron + 1/under_reporting_wave1*(1-omicron)),
          #     date>= min(omicron_shift$date) ~ rolling_average * 1/under_reporting_wave2,
          #     date < fit_cutoff_dates[1] ~ rolling_average * 1/under_reporting_wave1)) %>%
          mutate(
            rolling_average = case_when(
              date>= min(omicron_shift$date) & is.na(omicron) == FALSE ~ rolling_average * (1/under_reporting_wave2*omicron + 1/under_reporting_wave1*(1-omicron)),
              date>= min(omicron_shift$date) ~ rolling_average * 1/under_reporting_wave2,
              date < min(omicron_shift$date) ~ rolling_average * 1/under_reporting_wave1)) %>%
          mutate(fit_statistic = abs(rolling_average - reported_cases)^2)
        
        fit_statistic = data.frame(
          fit = sum(workshop$fit_statistic,na.rm=TRUE),
          wave1 = under_reporting_wave1,
          wave2 = under_reporting_wave2)
        
        underreporting_tracker = rbind(underreporting_tracker,fit_statistic)
    }
  }
  
  #detailed search under reporting
  best_so_far = underreporting_tracker[underreporting_tracker$fit == min(underreporting_tracker$fit, na.rm=TRUE),]
  for(under_reporting_wave1 in seq(best_so_far$wave1 -search_interaval,best_so_far$wave1 + search_interaval)){
    for(under_reporting_wave2 in seq(best_so_far$wave2 -search_interaval,best_so_far$wave2 + search_interaval)){
            
            workshop = case_history %>%
              select(date,rolling_average) %>%
              rename(reported_cases = rolling_average) %>%
              right_join(incidence_log, by = "date") %>%
              left_join(omicron_shift, by = "date") %>%
              rename(omicron = percentage) %>%
              # mutate(
              #   rolling_average = case_when(
              #     date>= min(omicron_shift$date) & is.na(omicron) == FALSE ~ rolling_average * (1/under_reporting_wave2*omicron + 1/under_reporting_wave1*(1-omicron)),
              #     date>= min(omicron_shift$date) ~ rolling_average * 1/under_reporting_wave2,
              #     date < fit_cutoff_dates[1] ~ rolling_average * 1/under_reporting_wave1)) %>%
              mutate(
                rolling_average = case_when(
                  date>= min(omicron_shift$date) & is.na(omicron) == FALSE ~ rolling_average * (1/under_reporting_wave2*omicron + 1/under_reporting_wave1*(1-omicron)),
                  date>= min(omicron_shift$date) ~ rolling_average * 1/under_reporting_wave2,
                  date < min(omicron_shift$date) ~ rolling_average * 1/under_reporting_wave1)) %>%
              mutate(fit_statistic = abs(rolling_average - reported_cases)^2)
            
            fit_statistic = data.frame(
              fit = sum(workshop$fit_statistic,na.rm=TRUE),
              wave1 = under_reporting_wave1,
              wave2 = under_reporting_wave2)
            
            underreporting_tracker = rbind(underreporting_tracker,fit_statistic)
    }
  }
  
  fit_statistic = min(underreporting_tracker$fit, 
                      na.rm=TRUE)
  
  these_waves = underreporting_tracker[underreporting_tracker$fit == min(underreporting_tracker$fit),]
  under_reporting_wave2 = these_waves$wave2
  under_reporting_wave1 = these_waves$wave1
  
  
  return(fit_statistic)
}

ggplot() +
  geom_line(data=workshop,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=workshop,aes(x=date,y=reported_cases)) +
  plot_standard


require(DEoptim)
#first round by Monday (4 days away) so roughly 100 runs
full_fit <- DEoptim(fn = fit_all_waves,
                    lower = c(2,2,
                              0,30
                    ),
                    upper = c(4,4,
                              45,75
                    ),
                    control = list(NP = 15,
                                   itermax = 8,
                                   storepopfrom = 1)) 
save(full_fit, file = paste('1_inputs/fit/full_fit',this_setting,Sys.Date(),'.Rdata',sep=''))

### Explore fit
summary(full_fit)
plot(full_fit, plot.type = "bestvalit")
#plot(full_fit, plot.type ="bestmemit")
plot(full_fit, plot.type ="storepop")
to_plot = as.data.frame(full_fit$member$pop)
colnames(to_plot) <- c('seed_date','under_reporting','beta_modifier')
ggplot(to_plot) + geom_histogram(aes(x=seed_date),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=under_reporting),bins=10)
ggplot(to_plot) + geom_histogram(aes(x=beta_modifier),bins=10)
ggplot(to_plot) + geom_point(aes(x=beta_modifier,y=under_reporting))
#_________________________________________________
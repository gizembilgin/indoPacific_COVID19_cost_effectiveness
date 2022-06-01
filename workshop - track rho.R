### To have a dynamic rho need:
# (1) shape of rho waning
# (3) tracking of days since infection of people in Recovered class

#Assumptions log:
# assume that no additional immunity from repeat infections

#Limitations:
# can not track individuals

#Approach
# use omega (180 day) cut-off, and see density of days since in this period
# at each time-step sum incidence 0-179 days


### (1) Shape of waning ###############################################################################
#See rho.xlsx in parameter estimation
rho_dn = crossing(strain = 'omicron',
                                   outcome = c('symptomatic_disease','severe_outcome'),
                                   days = seq(0,365*2)
                                   )
rho_dn = rho_dn %>% mutate(
  protection = case_when(
    outcome == 'symptomatic_disease' ~ 0.9932*exp(-0.004*days),
    outcome == 'severe_outcome' ~ 0.8859*exp(-0.0002*days)
  )
)

rho_est_plots = list()
rho_est_plots[[1]] = ggplot() + geom_line(data = rho_dn[rho_dn$outcome == 'symptomatic_disease',],
                     aes(days,protection)) + ylim(0,1) +labs(title = 'symptomatic_disease')
rho_est_plots[[2]] = ggplot() + geom_line(data = rho_dn[rho_dn$outcome == 'severe_outcome',],
                     aes(days,protection)) + ylim(0,1) +labs(title = 'severe_outcome')

grid.arrange(rho_est_plots[[1]], rho_est_plots[[2]])
#######################################################################################################



### (2) Tracking of days since infection (proxy) for people in Recovered class ########################
hist_cases = case_history %>% mutate(daily_cases = new * underascertainment_est) %>%
  select(date,daily_cases)

rho_time_step <- function(this_outcome,date_now){
  
  this_rho = rho_dn %>% filter(outcome == this_outcome)
  
  workshop = incidence_log %>% select(date,daily_cases)
  workshop = rbind(workshop,hist_cases)
  #LIMITATION - gap in dates between today and date start of model
  
  workshop = workshop[workshop$date <= date_now & workshop$date > date_now - 1/omega,] %>%
    mutate(days = as.numeric(date_now - date)) 
  
  workshop = workshop %>% mutate(prop_window = daily_cases/sum(workshop$daily_cases))
  
  ggplot(workshop) + geom_line(aes(date,prop_window))  
  
  if (sum(workshop$prop_window) != 1){stop('error in rho_time_step')}
  

  workshop = workshop %>% left_join(this_rho) %>%
    mutate(interim = protection * prop_window)
  
  return(sum(workshop$interim))
}

#rho_time_step(strain_now,date_now,incidence_log)
#######################################################################################################



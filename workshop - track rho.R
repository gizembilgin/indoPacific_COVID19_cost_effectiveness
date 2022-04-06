### To have a dynamic rho need:
# (1) estimate of max rho
# (2) estimate of shape of waning
# (3) tracking of days since infection of people in Recovered class

#Assumptions log:
# assume that no additional immunity from repeat infections

#Limitations:
# can not track individuals

#Approach
# use omega (180 day) cut-off, and see density of days since in this period
# at each time-step sum incidence 0-179 days


### (1) Estimate of max rho ###########################################################################
#COMEBACK - lit review
max_rho = 0.95
#######################################################################################################



### (2) Shape of waning ###############################################################################
#COMEBACK - lit review
#quick look
ggplot(immunity_from_infection[immunity_from_infection$outcome == 'acquisition',]) + geom_line(aes(days,ve_predict_mean,color=strain))

workshop <- immunity_from_infection[immunity_from_infection$outcome == 'acquisition',] %>%
  group_by(strain) %>%
  mutate(waning = ve_predict_mean / max(ve_predict_mean))

ggplot(workshop) + geom_line(aes(days,waning,color=strain))
#diff shape!

rho_dn = workshop[workshop$strain == strain_now,] %>%
  mutate(rho_est = max_rho * waning) %>% 
  ungroup() %>%
  select(days,rho_est)
ggplot(rho_dn) + geom_line(aes(days,rho_est)) + ylim(0,1)
#######################################################################################################



### (3) Tracking of days since infection (proxy) for people in Recovered class ########################
hist_cases = case_history %>% mutate(daily_cases = new * underascertainment_est) %>%
  select(date,daily_cases)

rho_time_step <- function(strain_now,date_now){
  
  workshop = incidence_log %>% select(date,daily_cases)
  workshop = rbind(workshop,hist_cases)
  #LIMITATION - gap in dates between today and date start of model
  
  workshop = workshop[workshop$date <= date_now & workshop$date > date_now - 1/omega,] %>%
    mutate(days = as.numeric(date_now - date)) 
  
  workshop = workshop %>% mutate(prop_window = daily_cases/sum(workshop$daily_cases))
  
  ggplot(workshop) + geom_line(aes(date,prop_window))  
  

  workshop = workshop %>% left_join(rho_dn) %>%
    mutate(interim = rho_est * prop_window)
  
  return(sum(workshop$interim))
}

#rho_time_step(strain_now,date_now,incidence_log)
#######################################################################################################



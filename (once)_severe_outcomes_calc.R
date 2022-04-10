

load(file = '1_inputs/severe_outcome_FINAL.Rdata') #adjusted values from Qatar
time.start=proc.time()[[3]]


#####(6/7) Multiplying severe outcomes by VE
#(A/B) calculate VE against severe outcomes by day
if (ticket == 1 | vax_strategy_plot == "on"){ #only have to run when vax strategy changing
  VE_tracker = data.frame()
  for (outcome in c('death','hospitalisation')){
    for (day in 1:(model_weeks*7) ){
      workshop = VE_time_step(strain_inital,date_start+day,outcome)
      workshop = workshop %>% mutate(date=day,
                                     outcome_VE=outcome)
      VE_tracker = rbind(VE_tracker,workshop)
    }
  }
  VE_tracker$date = date_start + VE_tracker$date 
  
  workshop = crossing(dose = 0,
                      vaccine_type = "unvaccinated",
                      age_group = age_group_labels,
                      VE = 0,
                      date = unique(VE_tracker$date),
                      outcome_VE=unique(VE_tracker$outcome))
  VE_tracker = rbind(VE_tracker,workshop)
}

#(B/B) calculate severe outcome incidence by vax_status
severe_outcome_this_run = severe_outcome_FINAL %>% left_join(VE_tracker) %>%
  mutate(percentage = percentage*(1-VE)) %>%
  select(date,outcome,outcome_long,age_group,vaccine_type,dose,percentage)
#_______________________________________________________________________________


time.end=proc.time()[[3]]
time.end-time.start 

#TIME BURNER

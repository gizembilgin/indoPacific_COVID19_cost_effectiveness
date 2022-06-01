


time.start=proc.time()[[3]]


#####(6/7) Multiplying severe outcomes by VE
#(A/C) calculate VE against severe outcomes by day
#REMOVED AS NO WANING OPTION FOR VE AGAINST SEVERE OUTCOMES
# if (ticket == 1 | vax_strategy_toggle == "on"){ #only have to run when vax strategy changing
#   VE_tracker = data.frame()
#   for (outcome in c('death','severe_disease')){
#     for (day in 1:(model_weeks*7) ){
#       workshop = VE_time_step(strain_inital,date_start+day,outcome)
#       workshop = workshop %>% mutate(date=day,
#                                      outcome_VE=outcome)
#       VE_tracker = rbind(VE_tracker,workshop)
#     }
#   }
#   VE_tracker$date = date_start + VE_tracker$date 
#   

# }
VE_tracker= crossing(risk_group = risk_group_labels,
                     dose = seq(1,D),
                     vaccine_type = vax_type_list,
                     age_group = age_group_labels,
                     date = unique(incidence_log$date),
                     outcome_VE = c('death','severe_disease'))
workshop = VE_estimates_imputed %>% filter(strain == strain_now) %>% 
  select(vaccine_type,dose,outcome,VE) %>% mutate(VE=VE/100) %>%
  rename(outcome_VE = outcome)
VE_tracker = VE_tracker %>% left_join(workshop)

workshop = crossing(risk_group = risk_group_labels,
                    dose = 0,
                    vaccine_type = "unvaccinated",
                    age_group = age_group_labels,
                    date = unique(incidence_log$date),
                    outcome_VE = c('death','severe_disease'),
                    VE = 0)
VE_tracker = rbind(VE_tracker,workshop)



#(B/C) 
load(file = '1_inputs/severe_outcome_FINAL.Rdata') #adjusted values from Qatar
if (risk_group_toggle == "on"){
  severe_outcomes_list = unique(severe_outcome_FINAL$outcome)
  
  severe_outcome_FINAL_wRisk = data.frame()
  for (o in 1:length(severe_outcomes_list)){
    this_outcome = severe_outcomes_list[o]
    for (i in 1:num_age_groups){
      this_age = age_group_labels[i]
      row = severe_outcome_FINAL %>% filter(outcome == this_outcome & age_group == this_age)
      
      IR = row$percentage
      P = pop_setting$pop[pop_setting$age_group == this_age]
      P_general = pop_risk_group_dn$pop[pop_risk_group_dn$risk_group == 'general_public' & pop_risk_group_dn$age_group == this_age]
      P_risk = pop_risk_group_dn$pop[pop_risk_group_dn$risk_group == risk_group_name & pop_risk_group_dn$age_group == this_age]
      if (P != (P_general+P_risk)){stop('Line 15 P_general + P_risk != P_overall')}
      
      IR_gen = ((IR*P)/((RR_estimate*P_risk)/P_general +1))/P_general
      IR_risk = ((IR*P)/(P_general/(RR_estimate*P_risk) +1))/P_risk
      
      row_gen = row %>% mutate(percentage = IR_gen,risk_group = 'general_public')
      row_risk = row %>% mutate(percentage = IR_risk,risk_group = risk_group_name)
      severe_outcome_FINAL_wRisk = rbind(severe_outcome_FINAL_wRisk,row_gen,row_risk)
    }
  }
  severe_outcome_FINAL_wRisk$percentage[is.na(severe_outcome_FINAL_wRisk$percentage)]=0
  severe_outcome_FINAL = severe_outcome_FINAL_wRisk
}

#(C/C) calculate severe outcome incidence by vax_status
severe_outcome_this_run = severe_outcome_FINAL %>% left_join(VE_tracker) %>%
  mutate(percentage = percentage*(1-VE)) %>%
  select(date,outcome,outcome_long,age_group,risk_group,vaccine_type,dose,percentage)
#_______________________________________________________________________________


time.end=proc.time()[[3]]
time.end-time.start 

#TIME BURNER

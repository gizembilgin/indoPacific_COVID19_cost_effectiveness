#split severe_outcome_FINAL into risk and general population IRs

RR_estimate = 1 
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
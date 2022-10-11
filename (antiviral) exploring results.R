workshop = RECORD_antiviral_model_simulations 
workshop$outcome = workshop$outcome = factor(workshop$outcome,levels=c('severe_disease','hosp','death','YLL','neonatal_deaths'))
workshop = workshop%>% 
  filter(VE_sensitivity_analysis == "off" &
           antiviral_type == "paxlovid" &
           result %in% c('average','percentage', "antiviral_rollout_capacity_utilised","antiviral_eligible_pop_coverage" )) %>%
  arrange(outcome) %>%
  mutate(value = round(as.numeric(value),digits=1)) %>%
  select(-VE_sensitivity_analysis, -antiviral_type, -vax_scenario_risk_group) %>% 
  pivot_wider(
    id_cols = c(antiviral_target,vax_scenario,result),
    names_from = outcome,
    values_from = value) %>%
  arrange(result)

write.csv(workshop,file=paste(rootpath,'x_results/antiviral_inital_results.csv',sep=''))

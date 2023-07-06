check =  antiviral_model_worker(
  manager_scenario_dataframe,
  RECORD_antiviral_setup,
  setting = setting,
  
  local_LIST_antiviral_target_group = LIST_antiviral_target_group,
  local_LIST_antiviral_start_date = LIST_antiviral_start_date,
  local_LIST_antiviral_type = LIST_antiviral_type,
  local_stochastic_SO = toggle_stochastic_SO,
  local_compare_to_vaccine_effect = toggle_compare_to_vaccine_effect,
  local_sensitivity_analysis = toggle_sensitivity_analysis,
  local_pathway_to_care = pathway_to_care,
  local_antiviral_delivery_capacity = toggle_antiviral_delivery_capacity,
  local_fixed_antiviral_coverage = toggle_fixed_antiviral_coverage,
  local_antiviral_effectiveness = antiviral_effectiveness,
  local_booster_start_date = booster_start_date,
  
  worker_stochastic_VE_sampling = manager_stochastic_VE_sampling,
  
  stochastic_severe_outcomes_sampling = copy_sampling_fx_into_cluster,
  stochastic_severe_outcomes_application = copy_application_fx_into_cluster
)

subset_df = check %>% filter(evaluation_group == "net")

subset_ageSpecific = subset_df %>% filter(is.na(age_group)==FALSE) %>% group_by(outcome,antiviral_type,antiviral_target_group,intervention,vax_scenario,vax_scenario_risk_group) %>% summarise(n_ageSpecific=sum(n)) 
subset_overall = subset_df %>% filter(is.na(age_group)) %>% group_by(outcome,antiviral_type,antiviral_target_group,intervention,vax_scenario,vax_scenario_risk_group) %>% summarise(n_overall=sum(n))

compare = subset_overall %>% 
  left_join(subset_ageSpecific)%>% 
  filter(round(n_overall) != round(n_ageSpecific))

require(beepr); beep()
nrow(compare)
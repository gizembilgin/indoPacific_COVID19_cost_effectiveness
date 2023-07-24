
number_of_runs = CommandDeck_result_long %>%
  group_by(evaluation_level,setting,booster_vax_scenario,antiviral_type,antiviral_target_group,outcome,perspective,discounting_rate,antiviral_cost) %>%
  summarise(n=n(), .groups = "keep") 
number_of_runs = unique(number_of_runs$n)



CEAC_dataframe = CommandDeck_result_long %>%
  filter(evaluation_level == "incremental" &
           cost_per_outcome_averted != -Inf &
           cost_per_outcome_averted != Inf) %>%
  group_by(outcome,setting,perspective,discounting_rate,antiviral_cost,booster_vax_scenario,antiviral_type,antiviral_target_group) %>%
  arrange(cost_per_outcome_averted) %>%
  mutate(row_number = row_number(),
         probability = row_number/number_of_runs) %>%
  rename(WTP = cost_per_outcome_averted) %>%
  select(outcome,setting,perspective,discounting_rate,antiviral_cost,booster_vax_scenario,antiviral_type,antiviral_target_group,probability,WTP)
  

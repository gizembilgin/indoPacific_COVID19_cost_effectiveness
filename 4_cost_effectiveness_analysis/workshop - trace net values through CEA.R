### outcomesAverted_estimator
## CHECKED: same number of rows/outcomes for 'net' and 'incremental' evaluation levels
check = TRANSLATED_antiviral_simulations %>% 
  pivot_wider(names_from = "evaluation_level", values_from = "count_outcomes") %>%
  
  #find the rows where missing either net or incremental value
  filter((is.na(net) & is.na(incremental)) | 
           (is.na(net) & is.na(incremental) == FALSE) | 
           (is.na(net) == FALSE & is.na(incremental))) %>%
  
  #we expect no incremental effect of antivirals in ages 0-17 who are ineligible for antivirals
  filter(!(is.na(incremental) & age_group %in% c("0 to 4", "5 to 9", "10 to 17") & intervention %in% c( "molunipiravir 2023-01-01","nirmatrelvir_ritonavir 2023-01-01"))) %>%
  
  #we expect no incremental effect of antivirals on mild disease or number of cases
  filter(!(is.na(incremental) & outcome %in% c("mild","total_cases") & intervention %in% c( "molunipiravir 2023-01-01","nirmatrelvir_ritonavir 2023-01-01"))) %>%
  
  # we haven't collected net hosp_after_antivirals as this will be the same as incremental hosp_after_antivirals
  filter(!(is.na(net) & outcome == "hosp_after_antivirals")) %>%
  
  #don't need "no intervention" for incremental
  filter(!(is.na(incremental & intervention == "no intervention")))
nrow(check) == 0
#_______________________________________________________________________________


## CHECKED: incremental aligns with differences between net scenarios

outcomes_averted %>% group_by(evaluation_level) %>% summarise(n= n())
# evaluation_level     n
# incremental        102
# net                105
# n = 3 extra rows for no booster, no antiviral scenario

workshop = outcomes_averted %>%
  filter(evaluation_level == "net")
null_row = workshop %>% 
  filter(booster_vax_scenario == "no booster dose" & antiviral_scenario == "no antiviral") %>%
  ungroup() %>%
  select(-booster_vax_scenario,-antiviral_scenario,-antiviral_target_group) %>%
  rename(baseline = count_outcomes)
workshop = workshop %>%
  left_join(null_row, by = join_by(evaluation_level,setting,outcome)) %>%
  mutate(incremental = baseline - count_outcomes) %>%
  ungroup() %>%
  select(-count_outcomes,-baseline,-evaluation_level) %>%
  #join back incremental
  left_join(outcomes_averted[outcomes_averted$evaluation_level == "incremental",], by = join_by(setting,outcome,booster_vax_scenario,antiviral_scenario,antiviral_target_group))

workshop %>% filter(round(incremental,digits=2) != round(count_outcomes,digits=2))
#completely don't agree when DECISION_sampling_strategy = "empirical_distribution" as separate samples from the empirical distribution of 'net' and 'incremental'
#agree (with below exception) when DECISION_sampling_strategy = "single_run"

# setting outcome booster_vax_scenario antiviral_scenario                antiviral_target_group    incremental evaluation_level count_outcomes
#  PNG     QALYs   no booster dose      molunipiravir 2023-01-01          adults_with_comorbidities      91783. incremental               6643.
#  PNG     QALYs   no booster dose      molunipiravir 2023-01-01          all_adults                     95035. incremental               9894.
#  PNG     QALYs   no booster dose      molunipiravir 2023-01-01          unvaccinated_adults            94787. incremental               9647.
#  PNG     QALYs   no booster dose      nirmatrelvir_ritonavir 2023-01-01 adults_with_comorbidities     105771. incremental              20631.
#  PNG     QALYs   no booster dose      nirmatrelvir_ritonavir 2023-01-01 all_adults                    115678. incremental              30537.
#  PNG     QALYs   no booster dose      nirmatrelvir_ritonavir 2023-01-01 unvaccinated_adults           114917. incremental              29777.

#_______________________________________________________________________________
################################################################################

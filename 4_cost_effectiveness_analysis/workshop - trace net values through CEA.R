### outcomesAverted_estimator

#check = TRANSLATED_antiviral_simulations %>% group_by(evaluation_group,setting, outcome, booster_vax_scenario, intervention, intervention_target_group, age_group) %>% summarise(n= n()) #all 1 row
check = TRANSLATED_antiviral_simulations %>% group_by(setting, outcome, booster_vax_scenario, intervention, intervention_target_group, age_group) %>% summarise(n= n())
check_2 = check %>% filter(n == 1)
check_3 = check %>% filter(n >1)
unique(check_2$outcome); unique(check_3$outcome)
unique(check_2$booster_vax_scenario);unique(check_3$booster_vax_scenario)
unique(check_2$intervention);unique(check_3$intervention)
unique(check_2$intervention_target_group);unique(check_3$intervention_target_group)
unique(check_2$age_group); unique(check_3$age_group)

check_4 = TRANSLATED_antiviral_simulations %>% filter(outcome %in% c("mild","total_cases")) %>% select(evaluation_group,outcome)
unique(check_4)
check_4 = TRANSLATED_antiviral_simulations %>% filter(age_group %in% c("0 to 4","5 to 9","10 to 17")) %>% select(evaluation_group,age_group)
unique(check_4)
check_4 = TRANSLATED_antiviral_simulations %>% filter(intervention %in% c( "booster dose 2023-03-01" )) %>% select(evaluation_group,intervention)
unique(check_4)

#finding exact missing rows
check_5 = TRANSLATED_antiviral_simulations %>% 
  pivot_wider(names_from = "evaluation_group", values_from = "count_outcomes_averted") %>%
  filter((is.na(net) & is.na(pop_level)) | 
           (is.na(net) & is.na(pop_level) == FALSE) | 
           (is.na(net) == FALSE & is.na(pop_level))) %>%
  
  #we expect no 'pop_level' which is actually intervention level for ages 0-17
  filter(!(is.na(pop_level) & intervention %in% c( "molunipiravir 2023-01-01",          "nirmatrelvir_ritonavir 2023-01-01"))) %>%
  
  # we don't need hosp_after_antivirals for 'net' as will be the same as pop_level
  filter(!(is.na(net) & outcome == "hosp_after_antivirals")) %>%
  
  #don't need "no intervention" for pop_level
  filter(!(is.na(pop_level & intervention == "no intervention")))


### check how net vs incremental stored
workshop = TRANSLATED_antiviral_simulations %>% 
  select(evaluation_group,booster_vax_scenario,intervention,intervention_target_group)
workshop = unique(workshop)
View(workshop)


### check results
#outcomes_averted
nothing_net = outcomes_averted$count_outcomes_averted
net = outcomes_averted %>%
  filter(evaluation_group == "net")
  
#QALY breakdown

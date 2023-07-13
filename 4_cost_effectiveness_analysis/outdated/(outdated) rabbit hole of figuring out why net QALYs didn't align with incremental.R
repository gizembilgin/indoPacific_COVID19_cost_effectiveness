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

workshop  = workshop %>% filter(round(incremental,digits=2) != round(count_outcomes,digits=2))
#completely don't agree when DECISION_sampling_strategy = "empirical_distribution" as separate samples from the empirical distribution of 'net' and 'incremental'
#agree (with below exception) when DECISION_sampling_strategy = "single_run"

# setting outcome booster_vax_scenario antiviral_scenario                antiviral_target_group    incremental evaluation_level count_outcomes
#  PNG     QALYs   no booster dose      molunipiravir 2023-01-01          adults_with_comorbidities      91783. incremental               6643.
#  PNG     QALYs   no booster dose      molunipiravir 2023-01-01          all_adults                     95035. incremental               9894.
#  PNG     QALYs   no booster dose      molunipiravir 2023-01-01          unvaccinated_adults            94787. incremental               9647.
#  PNG     QALYs   no booster dose      nirmatrelvir_ritonavir 2023-01-01 adults_with_comorbidities     105771. incremental              20631.
#  PNG     QALYs   no booster dose      nirmatrelvir_ritonavir 2023-01-01 all_adults                    115678. incremental              30537.
#  PNG     QALYs   no booster dose      nirmatrelvir_ritonavir 2023-01-01 unvaccinated_adults           114917. incremental              29777.




workshop = TRANSLATED_antiviral_simulations %>% group_by(evaluation_level,
                                                         setting,
                                                         outcome,
                                                         booster_vax_scenario,
                                                         intervention,
                                                         intervention_target_group) %>%
  summarise(count_outcomes = sum(count_outcomes),.groups = "keep") 

workshop_net = workshop %>%
  filter(evaluation_level == "net")
null_row = workshop_net %>% 
  filter(booster_vax_scenario == "no booster dose" & intervention == "no intervention" ) %>%
  ungroup() %>%
  select(-booster_vax_scenario,-intervention,-intervention_target_group) %>%
  rename(baseline = count_outcomes)
workshop_net = workshop_net %>%
  left_join(null_row, by = join_by(evaluation_level,setting,outcome)) %>%
  mutate(incremental = baseline - count_outcomes) %>%
  ungroup() %>%
  select(-count_outcomes,-baseline,-evaluation_level) 

workshop_incremental = workshop[workshop$evaluation_level == "incremental",]
add_booster = workshop_incremental %>% 
  filter(intervention == "booster dose 2023-03-01") %>%
  ungroup() %>%
  select(setting,outcome,booster_vax_scenario,count_outcomes) %>%
  rename(booster_outcomes = count_outcomes)
workshop_incremental = workshop_incremental%>%
  left_join(add_booster, by = join_by(setting,outcome,booster_vax_scenario)) %>%
  mutate(count_outcomes_2 = case_when(
    intervention != "booster dose 2023-03-01" & booster_vax_scenario == "no booster dose" ~ count_outcomes,
    intervention != "booster dose 2023-03-01" ~ count_outcomes + booster_outcomes,
    TRUE ~ count_outcomes
  )) %>%
  filter(outcome != "hosp_after_antivirals")

workshop  = workshop_incremental %>% 
  left_join(workshop_net) %>%
  filter(round(count_outcomes_2,digits=2) != round(incremental,digits=2) | 
           (is.na(count_outcomes_2) & is.na(incremental) == FALSE) |
           (is.na(count_outcomes_2)==FALSE & is.na(incremental)))
workshop  = workshop_incremental %>% 
  right_join(workshop_net) %>%
  filter(round(count_outcomes_2,digits=2) != round(incremental,digits=2) | 
           (is.na(count_outcomes_2) & is.na(incremental) == FALSE) |
           (is.na(count_outcomes_2)==FALSE & is.na(incremental))) 
#TRANSLATED_antiviral_simulations aligns

#check combined <-> workshop_incremental
workshop_combined = Combined %>%
  group_by(evaluation_level,
           setting,
           outcome,
           booster_vax_scenario,
           antiviral_scenario,
           antiviral_target_group) %>%
  summarise(count_outcomes_combined = sum(count_outcomes),.groups = "keep") 
workshop_incremental_2 = workshop_incremental %>%
  rename(antiviral_scenario = intervention,
         antiviral_target_group = intervention_target_group) %>%
  mutate(
    antiviral_target_group = case_when(antiviral_scenario == "booster dose 2023-03-01" ~ NA,
                                       TRUE ~ antiviral_target_group),
    antiviral_scenario = case_when(
      antiviral_scenario == "booster dose 2023-03-01" ~ "no antiviral",
      TRUE ~ antiviral_scenario
    ))
workshop = workshop_combined %>%
  left_join(workshop_incremental_2) %>%
  filter(outcome != "hosp_after_antivirals") %>%
  filter(round(count_outcomes_2,digits=2) != round(count_outcomes_combined,digits=2) | 
           (is.na(count_outcomes_2) & is.na(count_outcomes_combined) == FALSE) |
           (is.na(count_outcomes_2)==FALSE & is.na(count_outcomes_combined))) 
workshop = workshop_combined %>%
  right_join(workshop_incremental_2) %>%
  filter(outcome != "hosp_after_antivirals") %>%
  filter(round(count_outcomes_2,digits=2) != round(count_outcomes_combined,digits=2) | 
           (is.na(count_outcomes_2) & is.na(count_outcomes_combined) == FALSE) |
           (is.na(count_outcomes_2)==FALSE & is.na(count_outcomes_combined))) 
#combined <-> aligns with incremental

#check altered TRANSLATED_antiviral_simulations aligns with workshop_net
workshop = TRANSLATED_antiviral_simulations %>%
  filter(evaluation_level != "incremental") %>%
  rename(antiviral_scenario = intervention,
         antiviral_target_group = intervention_target_group) %>%
  mutate(
    antiviral_target_group = case_when(
      antiviral_scenario %in% c("booster dose 2023-03-01","no intervention") ~ NA,
      TRUE ~ antiviral_target_group
    ),
    antiviral_scenario = case_when(
      antiviral_scenario %in% c("booster dose 2023-03-01","no intervention") ~ "no antiviral",
      TRUE ~ antiviral_scenario
    ))%>%
  group_by(evaluation_level,
           setting,
           outcome,
           booster_vax_scenario,
           antiviral_scenario,
           antiviral_target_group) %>%
  summarise(count_outcomes_modified = sum(count_outcomes),.groups = "keep") 
null_row = workshop %>% 
  filter(booster_vax_scenario == "no booster dose" & antiviral_scenario == "no antiviral" ) %>%
  ungroup() %>%
  select(-booster_vax_scenario,-antiviral_scenario,-antiviral_target_group) %>%
  rename(baseline = count_outcomes_modified)
workshop = workshop %>%
  left_join(null_row, by = join_by(evaluation_level,setting,outcome)) %>%
  mutate(incremental = baseline - count_outcomes_modified) %>%
  ungroup() %>%
  select(-count_outcomes_modified,-baseline,-evaluation_level) 
workshop_net_2 = workshop_net %>%
  rename(antiviral_scenario = intervention,
         antiviral_target_group = intervention_target_group) %>%
  mutate(
    antiviral_target_group = case_when(antiviral_scenario == "booster dose 2023-03-01" ~ NA,
                                       TRUE ~ antiviral_target_group),
    antiviral_scenario = case_when(
      antiviral_scenario %in% c("booster dose 2023-03-01","no intervention") ~ "no antiviral",
      TRUE ~ antiviral_scenario
    ))
check = workshop %>%
  rename(incremental_2 = incremental) %>%
  left_join(workshop_net_2) %>%
  filter(outcome != "hosp_after_antivirals") %>%
  filter(round(incremental,digits=2) != round(incremental_2,digits=2) | 
           (is.na(incremental) & is.na(incremental_2) == FALSE) |
           (is.na(incremental)==FALSE & is.na(incremental_2))) 
check = workshop %>%
  rename(incremental_2 = incremental) %>%
  right_join(workshop_net_2) %>%
  filter(outcome != "hosp_after_antivirals") %>%
  filter(round(incremental,digits=2) != round(incremental_2,digits=2) | 
           (is.na(incremental) & is.na(incremental_2) == FALSE) |
           (is.na(incremental)==FALSE & is.na(incremental_2))) 
#combined <-> aligns with incremental


#CHECK - rejoined TRANSLATED_antiviral_simulations align
workshop = TRANSLATED_antiviral_simulations %>% group_by(evaluation_level,
                                                         setting,
                                                         outcome,
                                                         booster_vax_scenario,
                                                         antiviral_scenario,
                                                         antiviral_target_group) %>%
  summarise(count_outcomes = sum(count_outcomes),.groups = "keep") 

workshop_net = workshop %>%
  filter(evaluation_level == "net")
null_row = workshop_net %>% 
  filter(booster_vax_scenario == "no booster dose" & antiviral_scenario == "no antiviral" ) %>%
  ungroup() %>%
  select(-booster_vax_scenario,-antiviral_scenario,-antiviral_target_group) %>%
  rename(baseline = count_outcomes)
workshop_net = workshop_net %>%
  left_join(null_row, by = join_by(evaluation_level,setting,outcome)) %>%
  mutate(incremental = baseline - count_outcomes) %>%
  ungroup() %>%
  select(-count_outcomes,-baseline,-evaluation_level) 

workshop_incremental = workshop[workshop$evaluation_level == "incremental",] %>%
  filter(outcome != "hosp_after_antivirals")
# add_booster = workshop_incremental %>% 
#   filter(antiviral_scenario == "no antiviral") %>%
#   ungroup() %>%
#   select(setting,outcome,booster_vax_scenario,count_outcomes) %>%
#   rename(booster_outcomes = count_outcomes)
# workshop_incremental = workshop_incremental%>%
#   left_join(add_booster, by = join_by(setting,outcome,booster_vax_scenario)) %>%
#   mutate(count_outcomes_2 = case_when(
#     antiviral_scenario != "booster dose 2023-03-01" & booster_vax_scenario == "no booster dose" ~ count_outcomes,
#     antiviral_scenario != "booster dose 2023-03-01" ~ count_outcomes + booster_outcomes,
#     TRUE ~ count_outcomes
#   )) %>%
#   filter(outcome != "hosp_after_antivirals")

workshop  = workshop_incremental %>% 
  left_join(workshop_net) %>%
  filter(round(count_outcomes,digits=2) != round(incremental,digits=2) | 
           (is.na(count_outcomes) & is.na(incremental) == FALSE) |
           (is.na(count_outcomes)==FALSE & is.na(incremental)))
workshop  = workshop_incremental %>% 
  right_join(workshop_net) %>%
  filter(round(count_outcomes,digits=2) != round(incremental,digits=2) | 
           (is.na(count_outcomes) & is.na(incremental) == FALSE) |
           (is.na(count_outcomes)==FALSE & is.na(incremental))) 
#TRANSLATED_antiviral_simulations aligns


#step one of calculation of outcomes_averted
unique(QALY_estimates$outcome)
workshop = TRANSLATED_antiviral_simulations %>%
  left_join(QALY_estimates, by = c("setting","outcome","age_group")) %>%
  filter(is.na(QALYs)) %>%
  filter(!(outcome %in% c("total_cases","hosp","hosp_after_antivirals")))
#no one is being left behind


#complete outcomes_averted
workshop_net = outcomes_averted %>%
  filter(evaluation_level == "net")
null_row = workshop_net %>% 
  filter(booster_vax_scenario == "no booster dose" & antiviral_scenario == "no antiviral" ) %>%
  ungroup() %>%
  select(-booster_vax_scenario,-antiviral_scenario,-antiviral_target_group) %>%
  rename(baseline = count_outcomes)
workshop_net = workshop_net %>%
  left_join(null_row, by = join_by(evaluation_level,setting,outcome)) %>%
  mutate(incremental = baseline - count_outcomes) %>%
  ungroup() %>%
  rename(count_outcomes_net = count_outcomes) %>%
  select(-evaluation_level) 

workshop_incremental = outcomes_averted %>%
  filter(evaluation_level == "incremental") %>%
  filter(outcome != "hosp_after_antivirals")
# add_booster = workshop_incremental %>% 
#   filter(antiviral_scenario == "no antiviral") %>%
#   ungroup() %>%
#   select(setting,outcome,booster_vax_scenario,count_outcomes) %>%
#   rename(booster_outcomes = count_outcomes)
# workshop_incremental = workshop_incremental%>%
#   left_join(add_booster, by = join_by(setting,outcome,booster_vax_scenario)) %>%
#   mutate(count_outcomes_2 = case_when(
#     antiviral_scenario != "booster dose 2023-03-01" & booster_vax_scenario == "no booster dose" ~ count_outcomes,
#     antiviral_scenario != "booster dose 2023-03-01" ~ count_outcomes + booster_outcomes,
#     TRUE ~ count_outcomes
#   )) %>%
#   filter(outcome != "hosp_after_antivirals")

workshop  = workshop_incremental %>% 
  left_join(workshop_net) %>%
  filter(round(count_outcomes,digits=2) != round(incremental,digits=2) | 
           (is.na(count_outcomes) & is.na(incremental) == FALSE) |
           (is.na(count_outcomes)==FALSE & is.na(incremental)))
workshop  = workshop_incremental %>% 
  right_join(workshop_net) %>%
  filter(round(count_outcomes,digits=2) != round(incremental,digits=2) | 
           (is.na(count_outcomes) & is.na(incremental) == FALSE) |
           (is.na(count_outcomes)==FALSE & is.na(incremental))) 


workshop = TRANSLATED_antiviral_simulations %>% group_by(evaluation_level,
                                                         setting,
                                                         outcome,
                                                         booster_vax_scenario,
                                                         antiviral_scenario,
                                                         antiviral_target_group) %>%
  summarise(count_outcomes = sum(count_outcomes),.groups = "keep") %>%
  filter(evaluation_level == "net") %>%
  filter(outcome %in% QALY_estimates$outcome) %>%
  mutate(antiviral_scenario_long = paste(antiviral_scenario,antiviral_target_group)) %>%
  filter(outcome == "mild")

plot_list = list()
for (this_outcome in unique(workshop$outcome)){
  plot_list[[length(plot_list) + 1]] <- ggplot(workshop[workshop$outcome == this_outcome,]) + 
    geom_col(aes(x=antiviral_scenario_long,y=count_outcomes,fill=as.factor(booster_vax_scenario)),
             position = "dodge") +
    labs(title = this_outcome)
}
plot_list
#no mild!

add_mild_to_net = TRANSLATED_antiviral_simulations %>%
  filter(outcome == "mild" & evaluation_level == "net" & booster_vax_scenario == "no booster dose")
structure = TRANSLATED_antiviral_simulations %>%
  select(antiviral_scenario,antiviral_target_group) %>%
  filter(!(antiviral_scenario %in% add_mild_to_net$antiviral_scenario))
structure = unique(structure)
add_mild_to_net = add_mild_to_net %>%
  select(-antiviral_scenario,-antiviral_target_group)
add_mild_to_net = crossing(add_mild_to_net,structure)
TRANSLATED_antiviral_simulations = rbind(TRANSLATED_antiviral_simulations,add_mild_to_net)
#_______________________________________________________________________________
################################################################################

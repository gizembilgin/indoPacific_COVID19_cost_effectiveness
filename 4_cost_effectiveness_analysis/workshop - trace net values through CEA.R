### ALL ALIGN FOR DECISION_sampling_strategy == "single_run"
### DON'T ALIGN FOR DECISION_sampling_strategy == "empirical_distribution" since sampling from both age-specific dn and net dn


### MASTER_antiviral_simulations ###############################################
#(1/3) Make complete scenarios out of incremental ##############################
workshop_incremental_0 = MASTER_antiviral_simulations %>% 
  filter(evaluation_level == "incremental")

workshop_incremental = data.frame()
##vax no antiviral
this_row = workshop_incremental_0 %>% 
  filter(intervention == "booster dose 2023-03-01") %>%
  mutate(antiviral_type = "no antiviral",
         antiviral_target_group = NA)  %>%
  select(setting,evaluation_level,booster_vax_scenario,antiviral_type,antiviral_target_group,
         age_group,outcome,value)
workshop_incremental = rbind(workshop_incremental,this_row)

##vax with antiviral
for (this_antiviral in unique(workshop_incremental_0$intervention[workshop_incremental_0$intervention != "booster dose 2023-03-01"])){     # cycle through the types of antivirals
  for (this_antiviral_target in unique(workshop_incremental_0$intervention_target_group)){                                     # cycle through antiviral target groups
    
    this_row = workshop_incremental_0 %>% 
      filter(intervention %in% c("booster dose 2023-03-01",this_antiviral)) %>%
      filter(intervention_target_group == this_antiviral_target | intervention == "booster dose 2023-03-01") %>%
      group_by(evaluation_level,setting,booster_vax_scenario,outcome,age_group) %>%
      summarise(value = sum(value),
                .groups = "keep") %>%
      mutate(antiviral_type = this_antiviral,
             antiviral_target_group = this_antiviral_target) %>%
      select(setting,evaluation_level,booster_vax_scenario,antiviral_type,antiviral_target_group,
             age_group,outcome,value)
    workshop_incremental = rbind(workshop_incremental,this_row)
  }
}


#(2/3) Make incremental from net scenarios #####################################
workshop_net = MASTER_antiviral_simulations %>%
  filter(evaluation_level != "incremental") %>%
  rename(antiviral_type = intervention,
         antiviral_target_group = intervention_target_group) %>%
  mutate(
    antiviral_target_group = case_when(
      antiviral_type %in% c("booster dose 2023-03-01","no intervention") ~ NA,
      TRUE ~ antiviral_target_group
    ),
    antiviral_type = case_when(
      antiviral_type %in% c("booster dose 2023-03-01","no intervention") ~ "no antiviral",
      TRUE ~ antiviral_type
    )) %>%
  select(-run_ID,-vax_scenario_risk_group,-intervention_doses_delivered )

null_row = workshop_net %>% 
  filter(booster_vax_scenario == "no booster dose" & antiviral_type == "no antiviral") %>%
  ungroup() %>%
  select(-booster_vax_scenario,-antiviral_type,-antiviral_target_group) %>%
  rename(baseline = value)

workshop_net = workshop_net %>%
  rename(net = value) %>%
  left_join(null_row) %>%
  mutate(incremental_derived = baseline - net) %>%
  ungroup() 


#(3/3) Join and check ##########################################################
Combined = workshop_incremental %>%
  rename(incremental = value) %>%
  select(-evaluation_level) %>%
  left_join(workshop_net) %>%
  mutate(comparison = incremental - incremental_derived)

Combined = Combined %>% 
  filter(is.na(comparison) | round(comparison,digits=2)>0) %>%
  
  # we haven't collected net hosp_after_antivirals or booster doses delivered as this will be the same as incremental hosp_after_antivirals
  filter(!(is.na(net) & outcome %in% c("hosp_after_antivirals","booster_doses_delivered","YLL"))) 

#NB: haven't included "mild" in net with antivirals -> need to addded back in sample_compartmentalModel 
#NB: haven't included "total_cases" in pop-level net -> this variable is only used in long COVID estimate which is age-specific
#_______________________________________________________________________________





### outcomesAvertedEstimation$outcomes_averted ###############################################
#(1/3) Make complete scenarios out of incremental ##############################
workshop_incremental_0 = outcomesAvertedEstimation$outcomes_averted %>%
  ungroup() %>%
  filter(evaluation_level == "incremental")

workshop_incremental = data.frame()
##vax no antiviral
this_row = workshop_incremental_0 %>% 
  filter(intervention == "booster dose 2023-03-01") %>%
  mutate(antiviral_type = "no antiviral",
         antiviral_target_group = NA)  %>%
  select(setting,evaluation_level,booster_vax_scenario,antiviral_type,antiviral_target_group,
         outcome,count_outcomes)
workshop_incremental = rbind(workshop_incremental,this_row)

##vax with antiviral
for (this_antiviral in unique(workshop_incremental_0$intervention[workshop_incremental_0$intervention != "booster dose 2023-03-01"])){     # cycle through the types of antivirals
  for (this_antiviral_target in unique(workshop_incremental_0$intervention_target_group)){                                     # cycle through antiviral target groups
    
    this_row = workshop_incremental_0 %>% 
      filter(intervention %in% c("booster dose 2023-03-01",this_antiviral)) %>%
      filter(intervention_target_group == this_antiviral_target | intervention == "booster dose 2023-03-01") %>%
      group_by(evaluation_level,setting,booster_vax_scenario,outcome) %>%
      summarise(count_outcomes = sum(count_outcomes),
                .groups = "keep") %>%
      mutate(antiviral_type = this_antiviral,
             antiviral_target_group = this_antiviral_target) %>%
      select(setting,evaluation_level,booster_vax_scenario,antiviral_type,antiviral_target_group,
             outcome,count_outcomes)
    workshop_incremental = rbind(workshop_incremental,this_row)
  }
}


#(2/3) Make incremental from net scenarios #####################################
workshop_net = outcomesAvertedEstimation$outcomes_averted  %>%
  filter(evaluation_level != "incremental") %>%
  rename(antiviral_type = intervention,
         antiviral_target_group = intervention_target_group) %>%
  mutate(
    antiviral_target_group = case_when(
      antiviral_type %in% c("booster dose 2023-03-01","no intervention") ~ NA,
      TRUE ~ antiviral_target_group
    ),
    antiviral_type = case_when(
      antiviral_type %in% c("booster dose 2023-03-01","no intervention") ~ "no antiviral",
      TRUE ~ antiviral_type
    )) 

null_row = workshop_net %>% 
  filter(booster_vax_scenario == "no booster dose" & antiviral_type == "no antiviral") %>%
  ungroup() %>%
  select(-booster_vax_scenario,-antiviral_type,-antiviral_target_group) %>%
  rename(baseline = count_outcomes)

workshop_net = workshop_net %>%
  rename(net = count_outcomes) %>%
  left_join(null_row) %>%
  mutate(incremental_derived = baseline - net) %>%
  ungroup() 


#(3/3) Join and check ##########################################################
Combined = workshop_incremental %>%
  rename(incremental = count_outcomes) %>%
  select(-evaluation_level) %>%
  left_join(workshop_net) %>%
  mutate(comparison = incremental - incremental_derived)

Combined = Combined %>% 
  filter(is.na(comparison) | round(comparison,digits=2)>0) 
#nrow == 0!
#_______________________________________________________________________________





### healthcareCostEstimation$healthcareCosts_averted ###############################################
#(1/3) Make complete scenarios out of incremental ##############################
workshop_incremental_0 = healthcareCostEstimation$healthcareCosts_averted %>%
  ungroup() %>%
  filter(evaluation_level == "incremental")

workshop_incremental = data.frame()
##vax no antiviral
this_row = workshop_incremental_0 %>% 
  filter(intervention == "booster dose 2023-03-01") %>%
  mutate(antiviral_type = "no antiviral",
         antiviral_target_group = NA)  %>%
  select(setting,evaluation_level,booster_vax_scenario,antiviral_type,antiviral_target_group,cost)
workshop_incremental = rbind(workshop_incremental,this_row)

##vax with antiviral
for (this_antiviral in unique(workshop_incremental_0$intervention[workshop_incremental_0$intervention != "booster dose 2023-03-01"])){     # cycle through the types of antivirals
  for (this_antiviral_target in unique(workshop_incremental_0$intervention_target_group)){                                     # cycle through antiviral target groups
    
    this_row = workshop_incremental_0 %>% 
      filter(intervention %in% c("booster dose 2023-03-01",this_antiviral)) %>%
      filter(intervention_target_group == this_antiviral_target | intervention == "booster dose 2023-03-01") %>%
      group_by(evaluation_level,setting,booster_vax_scenario) %>%
      summarise(cost = sum(cost),
                .groups = "keep") %>%
      mutate(antiviral_type = this_antiviral,
             antiviral_target_group = this_antiviral_target) %>%
      select(setting,evaluation_level,booster_vax_scenario,antiviral_type,antiviral_target_group,cost)
    workshop_incremental = rbind(workshop_incremental,this_row)
  }
}


#(2/3) Make incremental from net scenarios #####################################
workshop_net = healthcareCostEstimation$healthcareCosts_averted  %>%
  filter(evaluation_level != "incremental") %>%
  rename(antiviral_type = intervention,
         antiviral_target_group = intervention_target_group) %>%
  mutate(
    antiviral_target_group = case_when(
      antiviral_type %in% c("booster dose 2023-03-01","no intervention") ~ NA,
      TRUE ~ antiviral_target_group
    ),
    antiviral_type = case_when(
      antiviral_type %in% c("booster dose 2023-03-01","no intervention") ~ "no antiviral",
      TRUE ~ antiviral_type
    )) 

null_row = workshop_net %>% 
  filter(booster_vax_scenario == "no booster dose" & antiviral_type == "no antiviral") %>%
  ungroup() %>%
  select(-booster_vax_scenario,-antiviral_type,-antiviral_target_group) %>%
  rename(baseline = cost)

workshop_net = workshop_net %>%
  rename(net = cost) %>%
  left_join(null_row) %>%
  mutate(incremental_derived = baseline - net) %>%
  ungroup() 


#(3/3) Join and check ##########################################################
Combined = workshop_incremental %>%
  rename(incremental = cost) %>%
  select(-evaluation_level) %>%
  left_join(workshop_net) %>%
  select(-evaluation_level) %>%
  mutate(comparison = incremental - incremental_derived,
         relative = comparison/incremental_derived)

Combined = Combined %>% 
  filter(is.na(comparison) | round(comparison,digits=2)>0) 
#nrow == 0! WHEN toggle_uncertainty = "fixed"
#_______________________________________________________________________________





### productivityCosts ##########################################################
#(1/3) Make complete scenarios out of incremental ##############################
workshop_incremental_0 = productivityCosts %>%
  ungroup() %>%
  filter(evaluation_level == "incremental")

workshop_incremental = data.frame()
##vax no antiviral
this_row = workshop_incremental_0 %>% 
  filter(intervention == "booster dose 2023-03-01") %>%
  mutate(antiviral_type = "no antiviral",
         antiviral_target_group = NA)  %>%
  select(setting,evaluation_level,booster_vax_scenario,antiviral_type,antiviral_target_group,cost)
workshop_incremental = rbind(workshop_incremental,this_row)

##vax with antiviral
for (this_antiviral in unique(workshop_incremental_0$intervention[workshop_incremental_0$intervention != "booster dose 2023-03-01"])){     # cycle through the types of antivirals
  for (this_antiviral_target in unique(workshop_incremental_0$intervention_target_group)){                                     # cycle through antiviral target groups
    
    this_row = workshop_incremental_0 %>% 
      filter(intervention %in% c("booster dose 2023-03-01",this_antiviral)) %>%
      filter(intervention_target_group == this_antiviral_target | intervention == "booster dose 2023-03-01") %>%
      group_by(evaluation_level,setting,booster_vax_scenario) %>%
      summarise(cost = sum(cost),
                .groups = "keep") %>%
      mutate(antiviral_type = this_antiviral,
             antiviral_target_group = this_antiviral_target) %>%
      select(setting,evaluation_level,booster_vax_scenario,antiviral_type,antiviral_target_group,cost)
    workshop_incremental = rbind(workshop_incremental,this_row)
  }
}


#(2/3) Make incremental from net scenarios #####################################
workshop_net = productivityCosts  %>%
  filter(evaluation_level != "incremental") %>%
  rename(antiviral_type = intervention,
         antiviral_target_group = intervention_target_group) %>%
  mutate(
    antiviral_target_group = case_when(
      antiviral_type %in% c("booster dose 2023-03-01","no intervention") ~ NA,
      TRUE ~ antiviral_target_group
    ),
    antiviral_type = case_when(
      antiviral_type %in% c("booster dose 2023-03-01","no intervention") ~ "no antiviral",
      TRUE ~ antiviral_type
    )) 

null_row = workshop_net %>% 
  filter(booster_vax_scenario == "no booster dose" & antiviral_type == "no antiviral") %>%
  ungroup() %>%
  select(-booster_vax_scenario,-antiviral_type,-antiviral_target_group) %>%
  rename(baseline = cost)

workshop_net = workshop_net %>%
  rename(net = cost) %>%
  left_join(null_row) %>%
  mutate(incremental_derived = baseline - net) %>%
  ungroup() 


#(3/3) Join and check ##########################################################
Combined = workshop_incremental %>%
  rename(incremental = cost) %>%
  select(-evaluation_level) %>%
  left_join(workshop_net) %>%
  select(-evaluation_level) %>%
  mutate(comparison = incremental - incremental_derived,
         relative = comparison/incremental_derived)

Combined = Combined %>% 
  filter(is.na(comparison) | round(comparison,digits=2)>0) 
#nrow == 0! WHEN toggle_uncertainty = "fixed"
#_______________________________________________________________________________





### simulationSummary ##########################################################
workshop = this_result %>%
  filter(run_ID == unique(this_result$run_ID)[1] &
          # !(antiviral_target_group %in% c("all_adults", "unvaccinated_adults")) &
          # !(antiviral_type ==  "molunipiravir 2023-01-01" ) &
          # outcome == "QALYs" &
           setting == "PNG") %>% #let's check one run
  pivot_longer(cols = c("interventionCost"  ,"healthcareCostAverted",    "productivityLoss" , "QALYs","death","hosp"),
               names_to = "parameter",
               values_to = "value")

workshop_net = workshop %>% filter(evaluation_level == "net") %>% select(-evaluation_level)
workshop_incremental =  workshop %>% filter(evaluation_level != "net") %>% select(-evaluation_level)

null_row = workshop_net %>% 
  filter(booster_vax_scenario == "no booster dose" & antiviral_type == "no antiviral") %>%
  ungroup() %>%
  select(-booster_vax_scenario,-antiviral_type,-antiviral_target_group) %>%
  rename(baseline = value)

workshop_net = workshop_net %>%
  left_join(null_row, by = join_by(setting,run_ID,parameter)) %>%
  mutate(incremental_derived = baseline - value) %>%
  ungroup() 

#join back incremental
workshop =  workshop_incremental %>%
  rename(incremental = value) %>%
  left_join(workshop_net,by = join_by(setting, booster_vax_scenario, antiviral_type, antiviral_target_group, run_ID, parameter)) %>%
  select(setting,booster_vax_scenario,antiviral_type,antiviral_target_group,run_ID,parameter,value,baseline,incremental_derived,incremental) %>%
  mutate(comparison_abs = round(incremental - incremental_derived,digits = 2),
         comparison_rel = round((incremental - incremental_derived)/incremental,digits=2)) %>%
  select(-run_ID,-setting) %>%
  filter(comparison_abs != 0)
#opposite sign for intervention cost of net and incermental
#_______________________________________________________________________________





### CEA_worker ##########################################################
workshop = CommandDeck_result_long %>%
  filter(run_ID == unique(CommandDeck_result_long$run_ID)[1] &
           # !(antiviral_target_group %in% c("all_adults", "unvaccinated_adults")) &
           # !(antiviral_type ==  "molunipiravir 2023-01-01" ) &
           # outcome == "QALYs" &
           setting == "PNG")  %>%
  mutate(
    netCost =
      case_when(
        evaluation_level == "incremental" ~ healthcareCostAverted + productivityLoss - interventionCost, #healthCostAverted and productivityLoss averted but interventioncost spent
        evaluation_level == "net" ~ healthcareCostAverted + productivityLoss + interventionCost
      )
  ) %>% 
  pivot_longer(cols = c("interventionCost"  ,"healthcareCostAverted",    "productivityLoss" , "QALYs","death","hosp","netCost"),
               names_to = "parameter",
               values_to = "value")

workshop_net = workshop %>% filter(evaluation_level == "net") %>% select(-evaluation_level)
workshop_incremental =  workshop %>% filter(evaluation_level != "net") %>% select(-evaluation_level)

null_row = workshop_net %>% 
  filter(booster_vax_scenario == "no booster dose" & antiviral_type == "no antiviral") %>%
  ungroup() %>%
  select(-booster_vax_scenario,-antiviral_type,-antiviral_target_group) %>%
  rename(baseline = value)

workshop_net = workshop_net %>%
  left_join(null_row, by = join_by(setting,run_ID,parameter)) %>%
  mutate(incremental_derived = baseline - value) %>%
  ungroup() 

#join back incremental
workshop =  workshop_incremental %>%
  rename(incremental = value) %>%
  left_join(workshop_net,by = join_by(setting, booster_vax_scenario, antiviral_type, antiviral_target_group, run_ID, parameter)) %>%
  select(setting,booster_vax_scenario,antiviral_type,antiviral_target_group,run_ID,parameter,value,baseline,incremental_derived,incremental) %>%
  mutate(comparison_abs = round(incremental - incremental_derived,digits = 2),
         comparison_rel = round((incremental - incremental_derived)/incremental,digits=2)) %>%
  select(-run_ID,-setting) %>%
  filter(comparison_abs != 0 &
           comparison_rel > 0.01 & 
           comparison_rel != 2)
#opposite sign for intervention cost of net and incermental
#_______________________________________________________________________________






### Final results form
#(1/2) CommandDeck_result
workshop = CommandDeck_result %>%
  ungroup() %>%
  filter(variable_type != "ICER") %>%
  select(evaluation_level,setting,booster_vax_scenario,antiviral_type,antiviral_target_group,variable_type,variable,mean)#only checking one run!

workshop_net = workshop %>% filter(evaluation_level == "net") %>% select(-evaluation_level)
workshop_incremental =  workshop %>% filter(evaluation_level != "net") %>% select(-evaluation_level)

null_row = workshop_net %>% 
  filter(booster_vax_scenario == "no booster dose" & antiviral_type == "no antiviral") %>%
  ungroup() %>%
  select(-booster_vax_scenario,-antiviral_type,-antiviral_target_group) %>%
  rename(baseline = mean)

workshop_net = workshop_net %>%
  rename(net = mean) %>%
  left_join(null_row,by = join_by( setting, variable_type, variable)) %>%
  mutate(incremental_derived = baseline - net) %>%
  ungroup() 

#join back incremental
workshop =  workshop_incremental %>%
  rename(incremental = mean) %>%
  left_join(workshop_net,by = join_by(setting, booster_vax_scenario, antiviral_type, antiviral_target_group, variable_type, variable)) %>%
  select(setting,booster_vax_scenario,antiviral_type,antiviral_target_group,variable_type, variable,net,baseline,incremental_derived,incremental) %>%
  mutate(comparison_abs = round(incremental - incremental_derived,digits = 2),
         comparison_rel = round((incremental - incremental_derived)/incremental,digits=2))

workshop  = workshop %>% filter(round(incremental_derived,digits=2) != round(incremental,digits=2) &
                                  round(comparison_rel,digits=1) != 0 &
                                  round(comparison_rel,digits=1) != 2)
################################################################################



#(2/2) CommandDeck_result_long
workshop = CommandDeck_result_long %>%
  filter(run_ID == unique(CommandDeck_result_long$run_ID)[1] &
           !(antiviral_target_group %in% c("all_adults", "unvaccinated_adults")) &
           !(antiviral_type ==  "molunipiravir 2023-01-01" ) &
           outcome == "QALYs" &
           setting == "PNG") %>% #let's check one run
  select(-antiviral_cost,-discounting_rate,-perspective,-cost_per_outcome_averted) %>% 
  pivot_longer(cols = c("interventionCost"  ,"healthcareCostAverted",    "productivityLoss" , "count_outcomes","netCost"),
               names_to = "parameter",
               values_to = "value")

workshop_net = workshop %>% filter(evaluation_level == "net") %>% select(-evaluation_level)
workshop_incremental =  workshop %>% filter(evaluation_level != "net") %>% select(-evaluation_level)

null_row = workshop_net %>% 
  filter(booster_vax_scenario == "no booster dose" & antiviral_type == "no antiviral") %>%
  ungroup() %>%
  select(-booster_vax_scenario,-antiviral_type,-antiviral_target_group) %>%
  rename(baseline = value)

workshop_net = workshop_net %>%
  left_join(null_row, by = join_by(setting,run_ID,outcome,parameter)) %>%
  mutate(incremental_derived = baseline - value) %>%
  ungroup() 

#join back incremental
workshop =  workshop_incremental %>%
  rename(incremental = value) %>%
  left_join(workshop_net,by = join_by(setting, booster_vax_scenario, antiviral_type, antiviral_target_group, run_ID, outcome, parameter)) %>%
  select(setting,booster_vax_scenario,antiviral_type,antiviral_target_group,run_ID,outcome,parameter,value,baseline,incremental_derived,incremental) %>%
  mutate(comparison_abs = round(incremental - incremental_derived,digits = 2),
         comparison_rel = round((incremental - incremental_derived)/incremental,digits=2)) %>%
  select(-outcome,-run_ID,-setting)

#workshop  = workshop %>% filter(round(incremental_derived,digits=2) != round(incremental,digits=2))




simulationSummary <- function(DECISION_include_net,
                              outcomesAvertedEstimation,
                            interventionCost_estimates,
                            healthcareCostEstimation,
                            productivityCosts)
{
  colnames(outcomesAvertedEstimation$outcomes_averted)
  #"evaluation_level"       "setting"                "outcome"                "booster_vax_scenario"   "antiviral_type"     "antiviral_target_group" "count_outcomes" (12/07/2023)
  #"evaluation_level"       "setting"                "outcome"                "booster_vax_scenario"    "intervention"          "intervention_target_group "count_outcomes" (14/07/2023) 
  
  colnames(interventionCost_estimates) #need to copy for both evaluation_levels
  #"setting"                   "booster_vax_scenario"      "intervention"              "intervention_target_group" "cost"  
  
  colnames(healthcareCostEstimation$healthcareCosts_averted); colnames(productivityCosts) #both the same!
  #"evaluation_level" "setting"                   "booster_vax_scenario"      "intervention"              "intervention_target_group" "cost"   
  
  
  
  ### PART ONE: Join all data sets together! ###################################
  Combined_0 = crossing(interventionCost_estimates,
                        evaluation_level = unique(outcomesAvertedEstimation$outcomes_averted$evaluation_level)) 
  if ("net" %in% unique(outcomesAvertedEstimation$outcomes_averted$evaluation_level)){
    null_row = crossing(setting = unique(Combined_0$setting),
                        booster_vax_scenario = "no booster dose",
                        intervention = "no intervention",
                        intervention_target_group = NA,
                        cost = 0,
                        evaluation_level = "net")
    Combined_0 = rbind(Combined_0,null_row)
  }
  Combined_0 = Combined_0%>%
    rename(interventionCost = cost) %>%
    left_join(healthcareCostEstimation$healthcareCosts_averted, by = join_by(evaluation_level,setting, booster_vax_scenario, intervention, intervention_target_group)) %>%
    rename(healthcareCostAverted = cost) %>%
    left_join(outcomesAvertedEstimation$outcomes_averted,by = join_by(evaluation_level,setting, booster_vax_scenario, intervention, intervention_target_group),
              relationship = "many-to-many") #because of outcomes
  if (nrow(productivityCosts)>0){
    Combined_0 = Combined_0 %>%
      left_join(productivityCosts, by = join_by(evaluation_level,setting, booster_vax_scenario, intervention, intervention_target_group)) %>%
      rename(productivityLoss = cost)
  } else{
    Combined_0 = Combined_0 %>%
      mutate(productivityLoss = 0)
  }
  ##############################################################################
  
  
  
  ### PART TWO: Recreate booster-antiviral scenarios ###########################
  if (DECISION_include_net == "N"){
    #NB: the incremental effect of booster doses and antiviral scenarios were kept separate in (antiviral) run, let's combine back all combination!  
    Combined_1 = Combined_0 %>% 
      filter(evaluation_level == "incremental")
    
    Combined = data.frame()
    ##vax no antiviral
    this_row = Combined_1 %>% 
      filter(intervention == "booster dose 2023-03-01") %>%
      mutate(antiviral_type = "no antiviral",
             antiviral_target_group = NA)  %>%
      select(evaluation_level,setting,booster_vax_scenario,antiviral_type,antiviral_target_group,
             interventionCost, healthcareCostAverted, productivityLoss,
             outcome,count_outcomes)
    Combined = rbind(Combined,this_row)
    
    ##vax with antiviral
    for (this_antiviral in unique(Combined_1$intervention[Combined_1$intervention != "booster dose 2023-03-01"])){     # cycle through the types of antivirals
      for (this_antiviral_target in unique(Combined_1$intervention_target_group)){                                     # cycle through antiviral target groups
        
        this_row = Combined_1 %>% 
          filter(intervention %in% c("booster dose 2023-03-01",this_antiviral)) %>%
          filter(intervention_target_group == this_antiviral_target | intervention == "booster dose 2023-03-01") %>%
          group_by(evaluation_level,setting,booster_vax_scenario,outcome) %>%
          summarise(interventionCost = sum(interventionCost),
                    healthcareCostAverted = sum(healthcareCostAverted),
                    count_outcomes = sum(count_outcomes),
                    productivityLoss = sum(productivityLoss),
                    .groups = "keep") %>%
          mutate(antiviral_type = this_antiviral,
                 antiviral_target_group = this_antiviral_target) %>%
          select(evaluation_level,setting,booster_vax_scenario,antiviral_type,antiviral_target_group,
                 interventionCost, healthcareCostAverted,productivityLoss,
                 outcome,count_outcomes)
        Combined = rbind(Combined,this_row)
      }
    }
    
    #make wider because makes object 60% smaller
    Combined  = Combined %>% pivot_wider(values_from = "count_outcomes", names_from = "outcome")
    
  } else if (DECISION_include_net == "Y"){
    Combined = Combined_0 %>%
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
      pivot_wider(values_from = "count_outcomes", names_from = "outcome") %>%
      pivot_longer(cols = c("interventionCost"  ,"healthcareCostAverted",    "productivityLoss" , "QALYs","death","hosp"),
                   names_to = "parameter",
                   values_to = "value")
    
    #Let's derive incremental from the difference between net scenarios
    null_row = Combined %>% 
      filter(booster_vax_scenario == "no booster dose" & antiviral_type == "no antiviral") %>%
      ungroup() %>%
      select(-booster_vax_scenario,-antiviral_type,-antiviral_target_group) %>%
      rename(baseline = value)
    
    workshop_incremental = Combined %>%
      rename(net = value) %>%
      left_join(null_row,by = join_by(setting, evaluation_level, parameter)) %>%
      mutate(incremental_derived = baseline - net) %>%
      ungroup() %>%
      mutate(evaluation_level = "incremental") %>%
      rename(value = incremental_derived) %>%
      select(-baseline,-net)
    workshop_incremental$value[workshop_incremental$parameter == "interventionCost"] <- workshop_incremental$value[workshop_incremental$parameter == "interventionCost"] * -1 #expect this to be positive
    
    Combined = rbind(Combined,workshop_incremental) %>%
      pivot_wider(values_from = "value", names_from = "parameter")
  }

 
  rm(Combined_0,Combined_1,Combined_net,this_row)
  
  return(Combined)
}

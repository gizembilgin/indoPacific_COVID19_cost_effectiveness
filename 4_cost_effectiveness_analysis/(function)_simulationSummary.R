

simulationSummary <- function(outcomesAvertedEstimation,
                            interventionCost_estimates,
                            healthcareCostEstimation,
                            productivityCosts)
{
  colnames(outcomesAvertedEstimation$outcomes_averted)
  #"evaluation_level"       "setting"                "outcome"                "booster_vax_scenario"   "antiviral_scenario"     "antiviral_target_group" "count_outcomes" (12/07/2023)
  #"evaluation_level"       "setting"                "outcome"                "booster_vax_scenario"    "intervention"          "intervention_target_group "count_outcomes" (14/07/2023) 
  
  colnames(interventionCost_estimates) #need to copy for both evaluation_levels
  #"setting"                   "booster_vax_scenario"      "intervention"              "intervention_target_group" "cost"  
  
  colnames(healthcareCostEstimation$healthcareCosts_averted); colnames(productivityCosts) #both the same!
  #"evaluation_level" "setting"                   "booster_vax_scenario"      "intervention"              "intervention_target_group" "cost"   
  
  
  
  ### PART ONE: Join all data sets together! ###################################
  Combined_0 = crossing(interventionCost_estimates,
                        evaluation_level = unique(outcomesAvertedEstimation$outcomes_averted$evaluation_level)) %>%
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
  #NB: the incremental effect of booster doses and antiviral scenarios were kept separate in (antiviral) run, let's combine back all combination!  
  Combined_1 = Combined_0 %>% 
    filter(evaluation_level == "incremental")
  
  Combined = data.frame()
  ##vax no antiviral
  this_row = Combined_1 %>% 
    filter(intervention == "booster dose 2023-03-01") %>%
    mutate(antiviral_scenario = "no antiviral",
           antiviral_target_group = NA)  %>%
    select(evaluation_level,setting,booster_vax_scenario,antiviral_scenario,antiviral_target_group,
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
        mutate(antiviral_scenario = this_antiviral,
               antiviral_target_group = this_antiviral_target) %>%
        select(evaluation_level,setting,booster_vax_scenario,antiviral_scenario,antiviral_target_group,
               interventionCost, healthcareCostAverted,productivityLoss,
               outcome,count_outcomes)
      Combined = rbind(Combined,this_row)
    }
  }
  
  ##add back "net"
  Combined_net = Combined_0 %>%
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
      ))
  Combined = rbind(Combined,Combined_net)
  
  #make wider because makes object 60% smaller
  Combined  = Combined %>% pivot_wider(values_from = "count_outcomes", names_from = "outcome")
  rm(Combined_0,Combined_1,Combined_net,this_row)
  
  return(Combined)
}

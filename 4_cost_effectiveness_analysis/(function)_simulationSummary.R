

simulationSummary <- function(outcomesAvertedEstimation,
                            interventionCost_estimates,
                            healthcareCostEstimation)
{
  colnames(outcomesAvertedEstimation$outcomes_averted)
  #"setting"                   "outcome"                   "booster_vax_scenario"      "intervention"    "intervention_target_group" "mean" 
  
  colnames(interventionCost_estimates)
  #[1] "setting"                   "booster_vax_scenario"      "intervention"              "intervention_target_group" "cost"  
  
  colnames(healthcareCostEstimation$healthcareCosts_averted)
  #"setting"                   "booster_vax_scenario"      "intervention"              "intervention_target_group" "cost"   
  
  Combined_0 = interventionCost_estimates %>%
    rename(interventionCost = cost) %>%
    left_join(healthcareCostEstimation$healthcareCosts_averted, by = c("setting","booster_vax_scenario","intervention","intervention_target_group")) %>%
    rename(healthcareCostAverted = cost) %>%
    left_join(outcomesAvertedEstimation$outcomes_averted,by = join_by(setting, booster_vax_scenario, intervention, intervention_target_group))
  
  Combined = data.frame()
  ###vax no antiviral
  row = Combined_0 %>% 
    filter(intervention == "booster dose 2023-03-01") %>%
    mutate(antiviral_scenario = "no antiviral")  %>%
    select(setting,booster_vax_scenario,antiviral_scenario,
           interventionCost, healthcareCostAverted,
           outcome,count_outcomes_averted)
  Combined = rbind(Combined,row)
  
  ###vax with antiviral
  for (this_antiviral in unique(Combined_0$intervention[Combined_0$intervention != "booster dose 2023-03-01"])){ 
    # cycle through the types of antivirals
    for (this_antiviral_target in unique(Combined_0$intervention_target_group)){
      # cycle through antiviral target groups
      row = Combined_0 %>% 
        filter(intervention %in% c("booster dose 2023-03-01",this_antiviral)) %>%
        filter(intervention_target_group == this_antiviral_target | intervention == "booster dose 2023-03-01") %>%
        group_by(setting,booster_vax_scenario,outcome) %>%
        summarise(interventionCost = sum(interventionCost),
                  healthcareCostAverted = sum(healthcareCostAverted),
                  count_outcomes_averted = sum(count_outcomes_averted),
                  .groups = "keep") %>%
        mutate(antiviral_scenario = paste(this_antiviral,this_antiviral_target)) %>%
        select(setting,booster_vax_scenario,antiviral_scenario,
               interventionCost, healthcareCostAverted,
               outcome,count_outcomes_averted)
      Combined = rbind(Combined,row)
    }
  }
  
  #make wider because makes object 40% smaller
  Combined  = Combined %>% pivot_wider(values_from = "count_outcomes_averted", names_from = "outcome")
  
  return(Combined)
}

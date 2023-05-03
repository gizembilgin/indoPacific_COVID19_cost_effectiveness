

CEA_calculation <- function(outcomesAvertedEstimation,
                            interventionCost_estimates,
                            healthcareCostEstimation)
{
  colnames(outcomesAvertedEstimation$outcomes_averted)
  #"setting"                   "outcome"                   "booster_vax_scenario"      "intervention"    "intervention_target_group" "mean" 
  
  colnames(interventionCost_estimates)
  #[1] "setting"                   "booster_vax_scenario"      "intervention"              "intervention_target_group" "cost"  
  
  colnames(healthcareCostEstimation$healthcareCosts_averted)
  #"setting"                   "booster_vax_scenario"      "intervention"              "intervention_target_group" "cost"   
  
  result = interventionCost_estimates %>%
    rename(interventionCost = cost) %>%
    left_join(healthcareCostEstimation$healthcareCosts_averted, by = c("setting","booster_vax_scenario","intervention","intervention_target_group")) %>%
    rename(healthcareCostAverted = cost) %>%
    mutate(cost = interventionCost - healthcareCostAverted)
  
  return(result)
}

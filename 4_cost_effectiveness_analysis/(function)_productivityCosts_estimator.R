### This function calculates the productivity losses due to illness and premature mortality 
### NB: no sampling for this cost category since underlying uncertainty not available for multiple informing data sources

productivityCosts_estimator <- function(
    LIST_CEA_settings,
    MASTER_antiviral_simulations,
    toggle_discounting_rate = 0.03, 
    this_risk_group = "adults_with_comorbidities"
){
  
  ### PART ONE: loading productivity loss estimates#############################
  if (!(toggle_discounting_rate %in% seq(0,0.1,by = 0.01))){
    stop('Go back to (mech shop) productivity loss and ensure that productivity losses by this discounting rate are calculated')
  }
  
  load(file = "2_inputs/productivity_loss_reference_df.Rdata")
  productivity_loss_df <- productivity_loss_reference_df %>%
    filter(discounting_rate == toggle_discounting_rate) %>%
    ungroup() %>%
    select(-discounting_rate)
  ##############################################################################
  
  
  
  ### PART TWO: load antiviral simulation ######################################
  TRANSLATED_antiviral_simulations = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == FALSE) #we want age-specific incidence -> to infere age-specific productivity losses
  ##############################################################################
  
  
  
  ### PART THREE: apply productivity losses by outcome #########################
  productivity_loss_breakdown = TRANSLATED_antiviral_simulations %>%
    filter(outcome %in% unique(productivity_loss_df$outcome)) %>%
    left_join(productivity_loss_df, by = c("setting","age_group","outcome")) %>%
    group_by(evaluation_level,setting,booster_vax_scenario,intervention,intervention_target_group,outcome) %>%
    summarise(cost = sum(productivity_loss), .groups = "keep")
  
  productivity_loss_breakdown = TRANSLATED_antiviral_simulations %>%
    filter(outcome %in% unique(productivity_loss_df$outcome)) %>%
    left_join(productivity_loss_df, by = c("setting","age_group","outcome")) %>%
    mutate(productivity_loss = productivity_loss * value,
           productivity_loss_category = case_when(
             outcome == "death" ~ "death",
             TRUE ~ "illness")) %>%
    group_by(evaluation_level,setting,booster_vax_scenario,intervention,intervention_target_group,productivity_loss_category) %>%
    summarise(cost = sum(productivity_loss), .groups = "keep")
  ##############################################################################
  

  productivity_loss = productivity_loss_breakdown %>%
    group_by(evaluation_level,setting,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(cost = sum(cost), .groups = "keep")
  
  return(productivity_loss)
  #interestingly higher cost due to illness than death!
}
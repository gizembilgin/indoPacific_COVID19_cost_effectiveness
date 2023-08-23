# This function calculates the productivity losses due to illness and premature mortality 
# NB: no sampling for this cost category since underlying uncertainty not available for multiple informing data sources

estimate_productivity_costs <- function(
    LIST_CEA_settings,
    MASTER_antiviral_simulations,
    TORNADO_PLOT_OVERRIDE,
    list_discounting_rate = 0.03, 
    this_risk_group = "adults_with_comorbidities"
) {
  
  
  ### PART ONE: loading productivity loss estimates#############################
  if (length(list_discounting_rate[!list_discounting_rate %in% seq(0,0.1,by = 0.01)])>0){
    stop("Can't find productivity losses with this discounting rate. 
         Go back to (mech shop) productivity loss and ensure that productivity losses by this discounting rate are calculated")
  }
  
  load(file = "02_inputs/productivity_loss_reference_df.Rdata")
  productivity_loss_df <- productivity_loss_reference_df %>%
    filter(discounting_rate %in% list_discounting_rate) %>%
    ungroup() 
  rm(productivity_loss_reference_df)
  
  if (length(TORNADO_PLOT_OVERRIDE)>0){
    if ("productivity_loss_illness" %in% names(TORNADO_PLOT_OVERRIDE)) {
      productivity_loss_df$productivity_loss[productivity_loss_df$outcome != "death"] = 
        productivity_loss_df$productivity_loss[productivity_loss_df$outcome != "death"] * TORNADO_PLOT_OVERRIDE$productivity_loss_illness
    }
    if ("productivity_loss_death" %in% names(TORNADO_PLOT_OVERRIDE)) {
      productivity_loss_df$productivity_loss[productivity_loss_df$outcome == "death"] = 
        productivity_loss_df$productivity_loss[productivity_loss_df$outcome == "death"] * TORNADO_PLOT_OVERRIDE$productivity_loss_death
    }
  }
  ##############################################################################
  
  
  
  ### PART TWO: load antiviral simulation ######################################
  TRANSLATED_antiviral_simulations = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == FALSE) #we want age-specific incidence -> to infere age-specific productivity losses
  ##############################################################################
  
  
  
  ### PART THREE: apply productivity losses by outcome #########################
  # productivity_loss_breakdown = TRANSLATED_antiviral_simulations %>%
  #   filter(outcome %in% unique(productivity_loss_df$outcome)) %>%
  #   left_join(productivity_loss_df, by = c("setting","age_group","outcome")) %>%
  #   group_by(evaluation_level,setting,booster_vax_scenario,intervention,intervention_target_group,outcome) %>%
  #   summarise(cost = sum(productivity_loss), .groups = "keep")
  
  productivity_loss_breakdown = TRANSLATED_antiviral_simulations %>%
    filter(outcome %in% unique(productivity_loss_df$outcome)) %>%
    left_join(productivity_loss_df, by = c("setting","age_group","outcome"),
              relationship = "many-to-many") %>% #if length(list_discounting_rate)>1
    mutate(productivity_loss = productivity_loss * value,
           productivity_loss_category = outcome
           # productivity_loss_category = case_when(
           #   outcome == "death" ~ "death",
           #   TRUE ~ "illness")
           ) %>%
    group_by(evaluation_level, discounting_rate, setting, booster_vax_scenario, intervention, intervention_target_group, productivity_loss_category) %>%
    summarise(cost = sum(productivity_loss), .groups = "keep")
  ##############################################################################
  

  productivity_loss = productivity_loss_breakdown %>%
    group_by(evaluation_level, discounting_rate, setting, booster_vax_scenario, intervention, intervention_target_group) %>%
    summarise(cost = sum(cost), .groups = "keep")
  
  result = list(productivity_loss = productivity_loss,
                productivity_loss_breakdown = productivity_loss_breakdown)  
  
  return(result)
}
require(readr); require(ggplot2); require(tidyverse)

### This function calculates the productivity losses due to illness and premature mortality 

productivityCosts_estimator <- function(
    LIST_CEA_settings,
    MASTER_antiviral_simulations,
    toggle_discounting_rate = 0.03, 
    this_risk_group = "adults_with_comorbidities"
){
  
  if (!(toggle_discounting_rate %in% seq(0,0.1,by = 0.01))){
    stop('Go back to (mech shop) productivity loss and ensure that productivity losses by this discounting rate are calculated')
  }
  
  # Step One: load inputs
  setting_list = LIST_CEA_settings
  load(file = "2_inputs/productivity_loss_reference_df.Rdata")
  productivity_loss_df <- productivity_loss_reference_df %>%
    filter(discounting_rate == toggle_discounting_rate) %>%
    ungroup() %>%
    select(-discounting_rate)
  
  TRANSLATED_antiviral_simulations = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == FALSE) %>%
    
    #created shorten name to describe booster dose eligibility
    mutate(booster_vax_scenario = case_when( 
      vax_scenario == "catchup campaign for high-risk adults: assume booster to high-risk adults who have previously completed their primary schedule but have not recieved a booster"  ~ "booster dose catch-up campaign for high-risk adults",           
      vax_scenario == "catchup campaign for all adults: assume booster to all adults who have previously completed their primary schedule but have not recieved a booster" ~ "booster dose catch-up campaign for all adults",                       
      vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults" ~ "booster to all adults, prioritised to high-risk adults",                                                           
      vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~ "booster to all adults previously willing to be vaccinated",                      
      vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~ "booster to all high-risk adults previously willing to be vaccinated",  
      vax_scenario == "all willing adults vaccinated with a primary schedule" ~ "no booster dose"           
    )) %>%
    filter(is.na(booster_vax_scenario) == FALSE) %>%
    
    #DECISION - CEA for antivirals as of 01/01/2023 
    filter(intervention %in% c('vaccine','antiviral 2023-01-01')) %>%
    mutate(intervention = case_when(
      intervention == "vaccine" ~ "booster dose 2023-03-01",
      antiviral_type == "molunipiravir" ~ "molunipiravir 2023-01-01",
      antiviral_type == "nirmatrelvir_ritonavir" ~ "nirmatrelvir_ritonavir 2023-01-01"
    )) %>%
    
    mutate(intervention_target_group = 
             case_when(
               intervention %in% c("molunipiravir 2023-01-01","nirmatrelvir_ritonavir 2023-01-01") ~ antiviral_target_group,
               booster_vax_scenario %in% c("booster dose catch-up campaign for high-risk adults","booster to all high-risk adults previously willing to be vaccinated") ~ vax_scenario_risk_group,
               booster_vax_scenario %in% c("booster dose catch-up campaign for all adults","booster to all adults, prioritised to high-risk adults", "booster to all adults previously willing to be vaccinated") ~ "all_adults"
             )) %>%
    
    filter(result %in% c("n"))
  #_______________________________________________________________________________
  
  
  # Step Two: apply productivity losses by outcome
  productivity_loss_breakdown = TRANSLATED_antiviral_simulations %>%
    filter(outcome %in% unique(productivity_loss_df$outcome)) %>%
    left_join(productivity_loss_df, by = c("setting","age_group","outcome")) %>%
    group_by(setting,booster_vax_scenario,intervention,intervention_target_group,outcome) %>%
    summarise(cost = sum(productivity_loss), .groups = "keep")
  
  productivity_loss_breakdown = TRANSLATED_antiviral_simulations %>%
    filter(outcome %in% unique(productivity_loss_df$outcome)) %>%
    left_join(productivity_loss_df, by = c("setting","age_group","outcome")) %>%
    mutate(productivity_loss = productivity_loss * value,
           productivity_loss_category = case_when(
             outcome == "death" ~ "death",
             TRUE ~ "illness")) %>%
    group_by(setting,booster_vax_scenario,intervention,intervention_target_group,productivity_loss_category) %>%
    summarise(cost = sum(productivity_loss), .groups = "keep")
  #_______________________________________________________________________________
  

  productivity_loss = productivity_loss_breakdown %>%
    group_by(setting,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(cost = sum(cost), .groups = "keep")
  
  return(productivity_loss)
  #interestingly higher cost due to illness than death!
}
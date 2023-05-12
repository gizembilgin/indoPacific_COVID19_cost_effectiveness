
interventionCost_estimator <- function(LIST_CEA_settings,
                                       MASTER_antiviral_simulations,
                                       antiviral_cost_scenario = "low_generic_cost",
                                       wastage_rate_antiviralSchedule = 0,
                                       toggle_uncertainty = TOGGLE_uncertainty,
                                       TORNADO_PLOT_OVERRIDE = list(),
                                       fitted_distributions = fitted_distributions){
  
  #NB: we include a wastage factor for RAT tests (i.e., how many RATs needed to led to a dispensation of oral antivirals),
  #     but we include wastage rates for all other components.
  #     These wastage rates then need to be converted to wastage factors by 1/(1-wastage_rate)
  
  
  ### Load RECORD_antiviral_model_simulations ####################################
  # We would like a data set with the following columns:
  # setting, booster_vax_scenario, intervention, intervention target group, intervention_doses_delivered
  
  ## Step Two: subset 
  TRANSLATED_antiviral_simulations = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == TRUE) %>%
    filter(evaluation_group == "pop_level") %>%
    select(-country,-setting_beta) %>%
    
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
    
    #ensure one value per simulation
    filter(result == "n" & outcome == "death") %>%
    
    select(setting, booster_vax_scenario, intervention, intervention_target_group,intervention_doses_delivered)
  
  if (nrow(TRANSLATED_antiviral_simulations) != nrow(na.omit(TRANSLATED_antiviral_simulations))){stop("NA introduced")}
  ##############################################################################
  
  
  
  
  ### Calculate intervention costs ##############################################
  interventionCost_estimates = data.frame()
  
  for (i in 1:length(LIST_CEA_settings)){
    this_setting = LIST_CEA_settings[[i]]
    
    ## Booster dose costs
    #(A/E) price_per_boosterDose
    if (this_setting %in% c("FJI","IDN","TLS")){
      price_per_boosterDose = 0.70 # Moderna - UNICEF Supply Division COVID-19 vaccine price data (2022 USD)
    } else if (this_setting == "PNG"){
      price_per_boosterDose = 1.50 # Johnson & Johnson - UNICEF Supply Division COVID-19 vaccine price data (2022 USD)
    }
    
    #(B/E) wastage_rate_boosterDose
    wastage_rate_boosterDose = 0.1
    
    #(C/E) price_per_injectionEquipmentDose
    price_per_injectionEquipmentDose = 0.0351 #UNICEF Supply Division syringe and safety box bundles price data (2022 USD)
    
    #(D/E) wastage_rate_injectionEquipment
    wastage_rate_injectionEquipment = 0.1
    
    #(E/E) operational_cost
    #NB: would sample # from distribution, but currently fixed estimate
    # This is an estimate per dose across a whole program (thinking through the meta-analysis), not a distribution of costs per individual
    # Hence, sample once for whole program, NOT for # of individuals
    if (toggle_uncertainty == "rand"){
      op_fitted_distributions = fitted_distributions %>% filter(parameter == "operation_cost_vaccine")
      operational_cost = rlnorm(1,op_fitted_distributions$param1,op_fitted_distributions$param2)
    } else if (toggle_uncertainty == "fixed"){
      operational_cost = 2.85
    }

    #use any overrides for tornado plot
    if (length(TORNADO_PLOT_OVERRIDE)>0){
      if("price_per_boosterDose" %in% names(TORNADO_PLOT_OVERRIDE)){price_per_boosterDose = TORNADO_PLOT_OVERRIDE$price_per_boosterDose}
      if("wastage_rate_boosterDose" %in% names(TORNADO_PLOT_OVERRIDE)){wastage_rate_boosterDose = TORNADO_PLOT_OVERRIDE$wastage_rate_boosterDose}
      if("price_per_injectionEquipmentDose" %in% names(TORNADO_PLOT_OVERRIDE)){price_per_injectionEquipmentDose = TORNADO_PLOT_OVERRIDE$price_per_injectionEquipmentDose}
      if("wastage_rate_injectionEquipment" %in% names(TORNADO_PLOT_OVERRIDE)){wastage_rate_injectionEquipment = TORNADO_PLOT_OVERRIDE$wastage_rate_injectionEquipment}
      if("operational_cost" %in% names(TORNADO_PLOT_OVERRIDE)){operational_cost = TORNADO_PLOT_OVERRIDE$operational_cost}
    }
    
    #calculate!
    static_costs = price_per_boosterDose*(1/(1-wastage_rate_boosterDose)) +
      price_per_injectionEquipmentDose*(1/(1-wastage_rate_injectionEquipment))
    
    vax_estimates = TRANSLATED_antiviral_simulations  %>%
      filter(intervention == "booster dose 2023-03-01") %>%
      mutate(cost = intervention_doses_delivered * (operational_cost + static_costs)) %>%
      select(setting,booster_vax_scenario,intervention,intervention_target_group,cost)
    
    interventionCost_estimates = rbind(interventionCost_estimates,vax_estimates)
    #___________________________________________________________________________
    
    
    ## Antiviral dose costs
    #(A/E) price_per_antiviralDose
    if (antiviral_cost_scenario == "low_generic_cost"){
      price_per_antiviralDose = 25 # Pfizer agreement for LMIC
    } else if (antiviral_cost_scenario == "middle_income_cost"){
      price_per_antiviralDose = 250 # Malaysian government
    } else if(antiviral_cost_scenario == "high_income_cost"){
      price_per_antiviralDose = 530 # US government
    }
    
    #(B/E) wastage_rate_antiviralSchedule - built in as a function parameter

    #(C/E) price_per_RAT
    price_per_RAT = 2.225 #Median price across 8 products listed on UNICEF catalogue 
    
    #(D/E) wastage_factor_RAT
    wastage_factor_RAT = 6
    
    #(E/E) operational_cost
    #WHO CHOICE estimates provide the distribution for individual costs, therefore sample from this distribution for the number of individuals and sum across
    op_fitted_distributions = fitted_distributions %>% filter(parameter == "outpatient_visit_cost" &
                                                                setting == this_setting)
    
    antiviral_estimates = TRANSLATED_antiviral_simulations  %>%
      filter(intervention != "booster dose 2023-03-01") %>%
      mutate(operational_cost = 0)
    
    if (toggle_uncertainty == "rand"){
      for (row in 1:nrow(antiviral_estimates)){
        this_sample = data.frame(est = rlnorm (antiviral_estimates$intervention_doses_delivered[row],meanlog = op_fitted_distributions$param1, sdlog = op_fitted_distributions$param2)) %>%
          mutate(est = case_when(
            est <0 ~ 0,
            TRUE ~ est
          ))
        antiviral_estimates$operational_cost[row] = sum(this_sample$est)
      }
      rm(this_sample)

    } else if (toggle_uncertainty == "fixed"){
      antiviral_estimates$operational_cost = antiviral_estimates$intervention_doses_delivered * outpatient$mean_cost
    }

    #use any overrides for tornado plot
    if (length(TORNADO_PLOT_OVERRIDE)>0){
      if("price_per_antiviralDose" %in% names(TORNADO_PLOT_OVERRIDE)){price_per_antiviralDose = TORNADO_PLOT_OVERRIDE$price_per_antiviralDose}
      if("wastage_rate_antiviralSchedule" %in% names(TORNADO_PLOT_OVERRIDE)){wastage_rate_antiviralSchedule = TORNADO_PLOT_OVERRIDE$wastage_rate_antiviralSchedule}
      if("price_per_RAT" %in% names(TORNADO_PLOT_OVERRIDE)){price_per_RAT = TORNADO_PLOT_OVERRIDE$price_per_RAT}
      if("wastage_factor_RAT" %in% names(TORNADO_PLOT_OVERRIDE)){wastage_factor_RAT = TORNADO_PLOT_OVERRIDE$wastage_factor_RAT}
      if("operational_cost" %in% names(TORNADO_PLOT_OVERRIDE)){antiviral_estimates$operational_cost = antiviral_estimates$intervention_doses_delivered * TORNADO_PLOT_OVERRIDE$operational_cost}
    }
    
    #calculate!
    
    static_costs = price_per_antiviralDose*(1/(1-wastage_rate_antiviralSchedule)) +
      price_per_RAT*wastage_factor_RAT
    
    antiviral_estimates = antiviral_estimates  %>%
      mutate(cost = operational_cost + intervention_doses_delivered * static_costs) %>%
      select(setting,booster_vax_scenario,intervention,intervention_target_group,cost)
    
    interventionCost_estimates = rbind(interventionCost_estimates,antiviral_estimates)
    #___________________________________________________________________________
    
  }
  
  return(interventionCost_estimates)
}

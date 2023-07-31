
interventionCost_estimator <- function(
    LIST_CEA_settings,
    MASTER_antiviral_simulations,
    TORNADO_PLOT_OVERRIDE,
    LIST_antiviral_cost_scenario = "low_generic_cost",
    wastage_rate_antiviralSchedule = 0,
    toggle_uncertainty = TOGGLE_uncertainty,
    fitted_distributions = local_fitted_distributions
    ){
  
  #NB: we include a wastage factor for RAT tests (i.e., how many RATs needed to led to a dispensation of oral antivirals),
  #     but we include wastage rates for all other components.
  #     These wastage rates then need to be converted to wastage factors by 1/(1-wastage_rate)
  
  
  ### PART ONE: load antiviral simulation ######################################
  TRANSLATED_antiviral_simulations = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == TRUE) %>%   #select overall value of intervention doses delivered, not age-specific
    filter(evaluation_level == "incremental") %>% #the incremental number of doses delivered will be the same for "net" and "pop_level"
    filter(outcome == "death") %>% #ensure one value per simulation, NB: we don't care about the outcome, only the intervention_doses_delivered column
    select(setting, booster_vax_scenario, intervention, intervention_target_group,intervention_doses_delivered)
  
  if (nrow(TRANSLATED_antiviral_simulations) != nrow(na.omit(TRANSLATED_antiviral_simulations))){stop("NA introduced")}
  ##############################################################################
  
  
  
  ### PART TWO: calculate intervention costs ###################################
  interventionCost_estimates = data.frame()
  
  for (this_setting in LIST_CEA_settings){
    
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
      if("vax_operational_cost" %in% names(TORNADO_PLOT_OVERRIDE)){operational_cost = TORNADO_PLOT_OVERRIDE$vax_operational_cost}
    }
    
    #calculate!
    static_costs = price_per_boosterDose*(1/(1-wastage_rate_boosterDose)) +
      price_per_injectionEquipmentDose*(1/(1-wastage_rate_injectionEquipment))
    
    vax_estimates = TRANSLATED_antiviral_simulations  %>%
      filter(intervention == "booster dose 2023-03-01" & setting == this_setting) %>%
      mutate(cost = intervention_doses_delivered * (operational_cost + static_costs)) %>%
      select(setting,booster_vax_scenario,intervention,intervention_target_group,cost)
    
    interventionCost_estimates = bind_rows(interventionCost_estimates,vax_estimates); rm(vax_estimates)
    #___________________________________________________________________________
    
    
    ## Antiviral dose costs
    #(A/E) wastage_rate_antiviralSchedule - built in as a function parameter
    
    #(B/E) price_per_RAT
    price_per_RAT = 2.225 #Median price across 8 products listed on UNICEF catalog 
    
    #(C/E) wastage_factor_RAT
    wastage_factor_RAT = 6
    
    #(D/E) operational_cost
    #WHO CHOICE estimates provide the distribution for individual costs, therefore sample from this distribution for the number of individuals and sum across
    op_fitted_distributions = fitted_distributions %>% 
      filter(parameter == "outpatient_visit_cost" & setting == this_setting)
    
    antiviral_estimates = TRANSLATED_antiviral_simulations  %>%
      filter(setting == this_setting) %>%
      filter(intervention != "booster dose 2023-03-01") %>%
      mutate(operational_cost = 0)
    
    if (toggle_uncertainty == "rand"){
      for (row_num in 1:nrow(antiviral_estimates)){
        this_sample = data.frame(est = rlnorm (antiviral_estimates$intervention_doses_delivered[row_num],meanlog = op_fitted_distributions$param1, sdlog = op_fitted_distributions$param2)) %>%
          mutate(est = case_when(
            est <0 ~ 0,
            TRUE ~ est
          ))
        antiviral_estimates$operational_cost[row_num] = sum(this_sample$est)
        rm(this_sample)
      }
      
    } else if (toggle_uncertainty == "fixed"){
      antiviral_estimates$operational_cost = antiviral_estimates$intervention_doses_delivered * op_fitted_distributions$mean
    }
    
    static_costs = data.frame()
    for (this_antiviral_cost_scenario in LIST_antiviral_cost_scenario){
        #(E/E) price_per_antiviralDose
        if (this_antiviral_cost_scenario == "low_generic_cost"){
          price_per_antiviralDose = 25 # Pfizer agreement for LMIC
        } else if (this_antiviral_cost_scenario == "middle_income_cost"){
          price_per_antiviralDose = 250 # Malaysian government
        } else if(this_antiviral_cost_scenario == "high_income_cost"){
          price_per_antiviralDose = 530 # US government
        }
      #use any overrides for tornado plot
      if (length(TORNADO_PLOT_OVERRIDE)>0){
        if("price_per_antiviralDose" %in% names(TORNADO_PLOT_OVERRIDE)){price_per_antiviralDose = TORNADO_PLOT_OVERRIDE$price_per_antiviralDose}
        if("wastage_rate_antiviralSchedule" %in% names(TORNADO_PLOT_OVERRIDE)){wastage_rate_antiviralSchedule = TORNADO_PLOT_OVERRIDE$wastage_rate_antiviralSchedule}
        if("price_per_RAT" %in% names(TORNADO_PLOT_OVERRIDE)){price_per_RAT = TORNADO_PLOT_OVERRIDE$price_per_RAT}
        if("wastage_factor_RAT" %in% names(TORNADO_PLOT_OVERRIDE)){wastage_factor_RAT = TORNADO_PLOT_OVERRIDE$wastage_factor_RAT}
        if("antiviral_operational_cost" %in% names(TORNADO_PLOT_OVERRIDE)){antiviral_estimates$operational_cost = antiviral_estimates$operational_cost * TORNADO_PLOT_OVERRIDE$antiviral_operational_cost}
      }
      
      #calculate!
      this_static_cost = price_per_antiviralDose*(1/(1-wastage_rate_antiviralSchedule)) +
        price_per_RAT*wastage_factor_RAT
      this_static_cost = data.frame(static_cost = this_static_cost,
                                antiviral_cost_scenario = this_antiviral_cost_scenario)
      static_costs = rbind(static_costs,this_static_cost); rm(this_static_cost)
    }

    
    antiviral_estimates = crossing(antiviral_estimates,
                                   antiviral_cost_scenario = LIST_antiviral_cost_scenario) %>%
      left_join(static_costs, by = "antiviral_cost_scenario") %>%
      mutate(cost = operational_cost + intervention_doses_delivered * static_cost) %>%
      select(setting,antiviral_cost_scenario,booster_vax_scenario,intervention,intervention_target_group,cost)
    
    interventionCost_estimates = bind_rows(interventionCost_estimates,antiviral_estimates); rm(antiviral_estimates)
    #___________________________________________________________________________
  }
  rm(TRANSLATED_antiviral_simulations)
  ##############################################################################
  
  return(interventionCost_estimates)
}

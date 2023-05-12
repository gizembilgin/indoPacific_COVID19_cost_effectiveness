
interventionCost_estimator <- function(LIST_CEA_settings,
                                       MASTER_antiviral_simulations,
                                       antiviral_cost_scenario = "low_generic_cost",
                                       wastage_rate_antiviralSchedule = 0,
                                       toggle_uncertainty = TOGGLE_uncertainty,
                                       TORNADO_PLOT_OVERRIDE = list()){
  
  #NB: we include a wastage factor for RAT tests (i.e., how many RATs needed to led to a dispensation of oral antivirals),
  #     but we include wastage rates for all other components.
  #     These wastage rates then need to be converted to wastage factors by 1/(1-wastage_rate)
  
  
  ### Load RECORD_antiviral_model_simulations ####################################
  # We would like a data set with the following columns:
  # setting, booster_vax_scenario, intervention, intervention target group, intervention_doses_delivered
  
  ## Step Two: subset 
  workshop = MASTER_antiviral_simulations %>%
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
  
  if (nrow(workshop) != nrow(na.omit(workshop))){stop("NA introduced")}
  
  #unique(workshop$setting)
  #[1] "FJI" "PNG" "TLS"
  #unique(workshop$booster_vax_scenario)
  # [1] "booster dose catch-up campaign for high-risk adults"                 "booster dose catch-up campaign for all adults"                      
  # [3] "booster to all adults, prioritised to high-risk adults"              "booster to all adults previously willing to be vaccinated"          
  # [5] "booster to all high-risk adults previously willing to be vaccinated" "no booster dose"                                                   
  #unique(workshop$intervention)
  #[1] "nirmatrelvir_ritonavir 2023-01-01" "molunipiravir 2023-01-01"          "booster dose 2023-03-01"          
  #unique(workshop$intervention_target_group)
  #[1] "adults_with_comorbidities"                         "unvaccinated_adults_AND_adults_with_comorbidities" "all_adults"                                       
  #[4] "unvaccinated_adults"                               "pregnant_women"      
  #_______________________________________________________________________________
  
  
  ## Step Three: summarise uncertainty in vax/antiviral model run
  #check normally distributed
  shapiro_tracker = data.frame()
  for (this_intervention in unique(workshop$intervention)){
    for (this_intervention_group in unique(workshop$intervention_target_group[workshop$intervention == this_intervention])){
      this_workshop = workshop %>% 
        filter(intervention == this_intervention &
                 intervention_target_group == this_intervention_group)
      this_test <- shapiro.test(this_workshop$intervention_doses_delivered)
      
      row = data.frame(test = this_test$method, 
             statistic = this_test$statistic,
             p_value = this_test$p.value,
             intervention = this_intervention,
             intervention_target_group = this_intervention_group,
             values_tested = "intervention_doses_delivered")
      shapiro_tracker = rbind(shapiro_tracker,row)
    }
  }
  shapiro_tracker = shapiro_tracker %>% 
    filter(intervention != "booster dose 2023-03-01") %>%
    filter(p_value < 0.05)
  if (nrow(shapiro_tracker)>0){warning(paste(nrow(shapiro_tracker),"rows of intervention doses delivered are not normally distributed"))}
 
  
  #summarise
   TRANSLATED_antiviral_simulations = workshop %>%
    group_by(setting, booster_vax_scenario, intervention, intervention_target_group) %>%
    summarise(mean = mean(intervention_doses_delivered),
              median = median(intervention_doses_delivered),
              sd = sd(intervention_doses_delivered),
              UB = mean + 1.96*sd,
              LB = mean -1.96*sd,
              .groups="keep")
  
  rm(workshop,MASTER_antiviral_simulations)
  #_______________________________________________________________________________
  ##############################################################################
  ##############################################################################
  
  
  
  
  ### Caculate intervention costs ##############################################
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
    #COMEBACK
    wastage_rate_boosterDose = 0.1
    
    #(C/E) price_per_injectionEquipmentDose
    price_per_injectionEquipmentDose = 0.0351 #UNICEF Supply Division syringe and safety box bundles price data (2022 USD)
    
    #(D/E) wastage_rate_injectionEquipment
    wastage_rate_injectionEquipment = 0.1
    
    #(E/E) operational_cost
    #NB: would sample # from distribution, but currently fixed estimate
    if (this_setting == "FJI"){
      operational_cost = 11.61
    } else if (this_setting == "IDN"){
      operational_cost = 1.28
    } else if (this_setting == "PNG"){
      operational_cost = 3.93
    } else if (this_setting == "TLS"){
      operational_cost = 4.17
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
      mutate(cost = mean * (operational_cost + static_costs)) %>%
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
    load(file = "2_inputs/WHO_CHOICE_2022.Rdata")
    outpatient = WHO_CHOICE_2022 %>% 
      filter(patient_type == "outpatient" &
               care_setting == "Health centre (no beds)" &
               currency_short == "USD" &
               statistic %in% c("model_prediction","SD")) %>%
      pivot_wider(names_from = statistic,values_from=value) %>%
      filter(ISO3_code == this_setting) %>%
      rename(mean_cost = model_prediction,
             sd_cost = SD) %>%
      select(mean_cost,sd_cost)
    
    antiviral_estimates = TRANSLATED_antiviral_simulations  %>%
      filter(intervention != "booster dose 2023-03-01") %>%
      mutate(operational_cost = 0)
    
    if (toggle_uncertainty == "rand"){
      #COMEBACK - once fitted lognormal or gamma distribution to WHO_CHOICE
      for (row in 1:nrow(antiviral_estimates)){
        this_sample = data.frame(est = rnorm (antiviral_estimates$mean[row],mean = outpatient$mean_cost, sd = outpatient$sd_cost  )) %>%
          mutate(est = case_when(
            est <0 ~ 0,
            TRUE ~ est
          ))
        antiviral_estimates$operational_cost[row] = sum(this_sample$est)
      }
      rm(this_sample)

    } else if (toggle_uncertainty == "fixed"){
      antiviral_estimates$operational_cost = antiviral_estimates$mean * outpatient$mean_cost
    }

    #use any overrides for tornado plot
    if (length(TORNADO_PLOT_OVERRIDE)>0){
      if("price_per_antiviralDose" %in% names(TORNADO_PLOT_OVERRIDE)){price_per_antiviralDose = TORNADO_PLOT_OVERRIDE$price_per_antiviralDose}
      if("wastage_rate_antiviralSchedule" %in% names(TORNADO_PLOT_OVERRIDE)){wastage_rate_antiviralSchedule = TORNADO_PLOT_OVERRIDE$wastage_rate_antiviralSchedule}
      if("price_per_RAT" %in% names(TORNADO_PLOT_OVERRIDE)){price_per_RAT = TORNADO_PLOT_OVERRIDE$price_per_RAT}
      if("wastage_factor_RAT" %in% names(TORNADO_PLOT_OVERRIDE)){wastage_factor_RAT = TORNADO_PLOT_OVERRIDE$wastage_factor_RAT}
      if("operational_cost" %in% names(TORNADO_PLOT_OVERRIDE)){antiviral_estimates$operational_cost = antiviral_estimates$mean * TORNADO_PLOT_OVERRIDE$operational_cost}
    }
    
    #calculate!
    
    static_costs = price_per_antiviralDose*(1/(1-wastage_rate_antiviralSchedule)) +
      price_per_RAT*wastage_factor_RAT
    
    antiviral_estimates = antiviral_estimates  %>%
      mutate(cost = operational_cost + mean * static_costs) %>%
      select(setting,booster_vax_scenario,intervention,intervention_target_group,cost)
    
    interventionCost_estimates = rbind(interventionCost_estimates,antiviral_estimates)
    #___________________________________________________________________________
    
  }
  
  return(interventionCost_estimates)
}

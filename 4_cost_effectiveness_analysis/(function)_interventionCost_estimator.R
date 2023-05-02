
interventionCost_estimator <- function(LIST_CEA_settings,
                                       antiviral_cost_scenario = "low_generic_cost",
                                       wastage_rate_antiviralSchedule = 0,
                                       toggle_uncertainty = TOGGLE_uncertainty,
                                       TORNADO_PLOT_OVERRIDE = list()){
  
  ### Load RECORD_antiviral_model_simulations ####################################
  # We would like a data set with the following columns:
  # setting, booster_vax_scenario, intervention, intervention target group, intervention_doses_delivered
  
  ## Step One: load all antiviral results
  rootpath = str_replace(getwd(), "GitHub_vaxAllocation/4_cost_effectiveness_analysis","")
  MASTER_antiviral_simulations = data.frame()
  
  for (i in 1:length(LIST_CEA_settings)){
    this_setting = LIST_CEA_settings[[i]]
    
    list_poss_Rdata = list.files(
      path = paste(rootpath, "x_results/", sep = ''),
      pattern = paste("AntiviralRun_", this_setting, "_", this_risk_group, "*", sep ="")
    )
    if (length(list_poss_Rdata) > 0) {
      list_poss_Rdata_details = double()
      for (j in 1:length(list_poss_Rdata)) {
        list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                        file.info(paste(rootpath, 'x_results/', list_poss_Rdata[[j]], sep = ''))$mtime)
      }
      latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
      load(file = paste(rootpath, "x_results/", latest_file, sep = ''))
    } else{
      stop(paste("no results for",this_setting,"with",this_risk_group,"see Translator"))
    }
    
    if (this_setting == "PNG_low_beta" & !("PNG_high_beta" %in% settings_to_plot)){this_setting = "PNG"}
    
    df_this_setting = RECORD_antiviral_model_simulations %>% mutate(setting = this_setting)
    MASTER_antiviral_simulations = bind_rows(MASTER_antiviral_simulations,df_this_setting)
  }
  rm(RECORD_antiviral_model_simulations)
  #_______________________________________________________________________________
  
  
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
    #sample per dose once, or from distribution for the number of doses administered?
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
    static_costs = price_per_boosterDose*(1+wastage_rate_boosterDose) +
      price_per_injectionEquipmentDose*(1+wastage_rate_injectionEquipment)
    
    vax_estimates = TRANSLATED_antiviral_simulations  %>%
      filter(intervention == "booster dose 2023-03-01") %>%
      mutate(cost_mean = mean * (operational_cost + static_costs),
             cost_median = median * (operational_cost + static_costs),
             cost_UB = UB* (operational_cost + static_costs),
             cost_LB = LB * (operational_cost + static_costs))
    
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
    
    #(D/E) wastage_rate_RAT
    wastage_rate_RAT = 5 #COMEBACK??
    
    #(E/E) operational_cost
    if (toggle_uncertainty == "on"){
      if (this_setting == "FJI"){
        operational_cost <- function(x){rnorm(x,mean=4.36,sd = )}
      } else if (this_setting == "IDN"){
        operational_cost = 2.57
      } else if (this_setting == "PNG"){
        operational_cost = 3.91
      } else if (this_setting == "TLS"){
        operational_cost = 1.83
      }
      
    } else if (toggle_uncertainty == "of"){
      #sample per dose once, or from distribution for the number of doses administered?
      if (this_setting == "FJI"){
        operational_cost = 4.36
      } else if (this_setting == "IDN"){
        operational_cost = 2.57
      } else if (this_setting == "PNG"){
        operational_cost = 3.91
      } else if (this_setting == "TLS"){
        operational_cost = 1.83
      }
    }

    #use any overrides for tornado plot
    if (length(TORNADO_PLOT_OVERRIDE)>0){
      if("price_per_antiviralDose" %in% names(TORNADO_PLOT_OVERRIDE)){price_per_antiviralDose = TORNADO_PLOT_OVERRIDE$price_per_antiviralDose}
      if("wastage_rate_antiviralSchedule" %in% names(TORNADO_PLOT_OVERRIDE)){wastage_rate_antiviralSchedule = TORNADO_PLOT_OVERRIDE$wastage_rate_antiviralSchedule}
      if("price_per_RAT" %in% names(TORNADO_PLOT_OVERRIDE)){price_per_RAT = TORNADO_PLOT_OVERRIDE$price_per_RAT}
      if("wastage_rate_RAT" %in% names(TORNADO_PLOT_OVERRIDE)){wastage_rate_RAT = TORNADO_PLOT_OVERRIDE$wastage_rate_RAT}
      if("operational_cost" %in% names(TORNADO_PLOT_OVERRIDE)){operational_cost = TORNADO_PLOT_OVERRIDE$operational_cost}
    }
    
    #calculate!
    
    static_costs = price_per_antiviralDose*(1+wastage_rate_antiviralSchedule) +
      price_per_RAT*(1+wastage_rate_RAT)
    
    antiviral_estimates = TRANSLATED_antiviral_simulations  %>%
      filter(intervention != "booster dose 2023-03-01") %>%
      mutate(cost_mean = mean * (operational_cost + static_costs),
             cost_median = median * (operational_cost + static_costs),
             cost_UB = UB* (operational_cost + static_costs),
             cost_LB = LB * (operational_cost + static_costs))
    
    interventionCost_estimates = rbind(interventionCost_estimates,antiviral_estimates)
    #___________________________________________________________________________
    
  }
  
  return(interventionCost_estimates)
}

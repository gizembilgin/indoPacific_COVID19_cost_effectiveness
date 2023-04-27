
interventionCost_estimator <- function(LIST_CEA_settings,
                                       antiviral_cost_scenario = "low_generic_cost",
                                       wastage_rate_antiviralSchedule = 0){
  
  interventionCost_estimates = data.frame()
  
  for (i in 1:length(LIST_CEA_settings)){
    this_setting = LIST_CEA_settings[[i]]
    
    ### Booster dose costs
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
    
    #calculate!
    row = data.frame(setting = this_setting,
                     intervention = "booster_dose",
                     cost = price_per_boosterDose*(1+wastage_rate_boosterDose) +
                       price_per_injectionEquipmentDose*(1+wastage_rate_injectionEquipment) +
                       operational_cost
                    )
    interventionCost_estimates = rbind(interventionCost_estimates,row)
    
    
    
    ### Antiviral dose costs
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
    
    #calculate!
    row = data.frame(setting = this_setting,
                     intervention = "antiviral_dose",
                     cost = price_per_antiviralDose*(1+wastage_rate_antiviralSchedule) +
                       price_per_RAT*(1+wastage_rate_RAT) +
                       operational_cost
    )
    interventionCost_estimates = rbind(interventionCost_estimates,row)

    
  }
  
  return(interventionCost_estimates)
}

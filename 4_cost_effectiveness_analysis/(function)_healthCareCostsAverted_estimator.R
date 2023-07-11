
healthCareCostsAverted_estimator <- function(LIST_CEA_settings,
                                             MASTER_antiviral_simulations,
                                             TORNADO_PLOT_OVERRIDE,
                                             toggle_uncertainty = TOGGLE_uncertainty,
                                             fitted_distributions = local_fitted_distributions){
  
  ### Load RECORD_antiviral_model_simulations ####################################
  # We would like a data set with the following columns:
  # setting, booster_vax_scenario, outcome, count_outcomes
  
  ## Step Two: subset 
  TRANSLATED_antiviral_simulations = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == TRUE) %>%
    filter(evaluation_group == "pop_level") %>%
    
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
    
    filter(result %in% c("n")) %>%
    rename(count_outcomes = value) %>%
    filter(outcome %in% c("hosp","hosp_after_antivirals","mild")) %>%
    
    select(setting, outcome, booster_vax_scenario, intervention, intervention_target_group, count_outcomes)
    
  if (nrow(TRANSLATED_antiviral_simulations) != nrow(na.omit(TRANSLATED_antiviral_simulations))){stop("NA introduced")}
  ##############################################################################
  
  
  ## Reduced LOS (outcome == "hosp_after_antivirals")
  #analytically propogating error, see scanned proof 12/05/2023 in 1_derivation
  X = cost_per_extra_LOS_MEAN = 0.09
  sdX = cost_per_extra_LOS_SD = 0.01
  
  Y = extra_LOS_MEAN = fitted_distributions$param1[fitted_distributions$parameter == "reduced_LOS_days"]
  sdY = extra_LOS_SD = fitted_distributions$param2[fitted_distributions$parameter == "reduced_LOS_days"]
  
  #use any overrides for tornado plot
  if (length(TORNADO_PLOT_OVERRIDE)>0){
    if("cost_per_extra_LOS" %in% names(TORNADO_PLOT_OVERRIDE)){X = X*TORNADO_PLOT_OVERRIDE$cost_per_extra_LOS}
    if("extra_LOS" %in% names(TORNADO_PLOT_OVERRIDE)){Y = Y * TORNADO_PLOT_OVERRIDE$extra_LOS}
  }
  
  new_sd = (exp(X) - 1) * Y * sqrt(((exp(X)*sdX)/(exp(X)-1))^2+(sdY/Y)^2)
  new_mean = (exp(X) - 1) * Y
  
  reduced_LOS_estimates = TRANSLATED_antiviral_simulations  %>%
    filter(outcome == "hosp_after_antivirals") %>%
    mutate(estimate = 0)
  
  if (toggle_uncertainty == "rand"){
    for (row in 1:nrow(reduced_LOS_estimates)){
      #sampling reduced LOS per hospitalised individual who had received antivirals
      this_sample = data.frame(est = rnorm (reduced_LOS_estimates$count_outcomes[row],mean = new_mean, sd = new_sd)) %>%
        mutate(est = case_when(
          est <0 ~ 0,
          TRUE ~ est
        ))
      reduced_LOS_estimates$estimate[row] = sum(this_sample$est)
    }
    rm(this_sample)
    
  } else if (toggle_uncertainty == "fixed"){
    reduced_LOS_estimates$estimate = reduced_LOS_estimates$count_outcomes * (exp(X) - 1) * Y
  }
  

  workshop = reduced_LOS_estimates %>%
    mutate(count_outcomes = estimate,
           outcome = "hosp") %>%
    select(-estimate)
  TRANSLATED_antiviral_simulations = TRANSLATED_antiviral_simulations[TRANSLATED_antiviral_simulations$outcome %in% c("hosp","mild"),]
  TRANSLATED_antiviral_simulations =   rbind(TRANSLATED_antiviral_simulations,workshop) %>%
    group_by(setting,outcome,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(count_outcomes = sum(count_outcomes), .groups="keep") #including hosp $ from reduced LOS of inpatients who recieved antivirals with other inpatients
  ##############################################################################
  
  
  
  ### Calculate healthcare costs ##############################################
  load(file = "2_inputs/hosp_adm.Rdata")
  
  ##inputs
  hosp_adm = hosp_adm %>%
    rename(setting = ISO3_code,
           sd_cost = sd,
           mean_cost = estimate) %>%
    mutate(patient_type = "inpatient",
           outcome = "hosp") %>%
    select(setting,patient_type,outcome,mean_cost,sd_cost) %>%
    rename(param2 = sd_cost) %>%
    mutate(param1 = mean_cost)
  
  outpatient = local_fitted_distributions %>% 
    filter(parameter == "outpatient_visit_cost") %>%
    rename(mean_cost = mean) %>%
    mutate(patient_type = "outpatient",
           outcome = "mild") %>%
    select(setting,patient_type,outcome,param1,param2,mean_cost) 
  
  cost_estimates = rbind(hosp_adm,outpatient) %>%
    filter(setting %in% LIST_CEA_settings)
  
  healthcare_access = data.frame(setting = c("FJI","IDN","PNG","TLS"),
                                 access = c(0.816,0.7,0.7,0.766))
  

  ## calculation
  cost_estimates = TRANSLATED_antiviral_simulations %>%
    filter(outcome %in% c("hosp","mild")) %>%
    ungroup() %>%
    right_join(cost_estimates,by=c("setting","outcome"),relationship = "many-to-many") %>%
    
    #modifying outpatient costs by access to care
    left_join(healthcare_access, by = "setting", relationship = "many-to-many") %>%
    mutate(
      count_outcomes = case_when(
        patient_type == "outpatient" ~ count_outcomes * access,
        TRUE ~ count_outcomes
      )) %>%
    mutate(cost = 0)
  
  if (toggle_uncertainty == "rand"){
    #sampling cost of each outpatient visit and each hospital admission
    for (row in 1:nrow(cost_estimates)){
      if (cost_estimates$patient_type[row] == "inpatient"){
        if (cost_estimates$count_outcomes[row]<0){multiplier = -1
        } else{multiplier = 1}
        this_sample = data.frame(est = rnorm(abs(cost_estimates$count_outcomes[row]), mean = cost_estimates$param1[row], sd = cost_estimates$param2[row])) %>%
          mutate(est = case_when(
            est <0 ~ 0,
            TRUE ~ est
          ))
        cost_estimates$cost[row] = sum(this_sample$est)*multiplier
        
        if (length(TORNADO_PLOT_OVERRIDE)>0){
          if("inpatient" %in% names(TORNADO_PLOT_OVERRIDE)){cost_estimates$cost[row] = sum(this_sample$est)*TORNADO_PLOT_OVERRIDE$inpatient}
        }
        
      } else if (cost_estimates$patient_type[row] == "outpatient"){
        this_sample = data.frame(est = rlnorm(cost_estimates$count_outcomes[row], meanlog = cost_estimates$param1[row], sdlog = cost_estimates$param2[row])) %>%
          mutate(est = case_when(
            est <0 ~ 0,
            TRUE ~ est
          ))
        cost_estimates$cost[row] = sum(this_sample$est)
        
        if (length(TORNADO_PLOT_OVERRIDE)>0){
          if("outpatient" %in% names(TORNADO_PLOT_OVERRIDE)){cost_estimates$cost[row] = sum(this_sample$est)*TORNADO_PLOT_OVERRIDE$outpatient}
        }
      }
      rm(this_sample)
    }
  } else if (toggle_uncertainty == "fixed"){
    cost_estimates = cost_estimates %>%
      mutate(cost = count_outcomes * mean_cost)
    
    if (length(TORNADO_PLOT_OVERRIDE)>0){
      if("inpatient" %in% names(TORNADO_PLOT_OVERRIDE)){
        cost_estimates$cost[cost_estimates$patient_type == "inpatient"] = cost_estimates$cost[cost_estimates$patient_type == "inpatient"]*TORNADO_PLOT_OVERRIDE$inpatient
      }
      if("outpatient" %in% names(TORNADO_PLOT_OVERRIDE)){
        cost_estimates$cost[cost_estimates$patient_type == "outpatient"] = cost_estimates$cost[cost_estimates$patient_type == "outpatient"]*TORNADO_PLOT_OVERRIDE$outpatient
      }
    }
  }
  #___________________________________________________________________________

  
  
  ### Export result  ###########################################################
  healthcareCosts_averted = cost_estimates %>%
    group_by(setting,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(cost = sum(cost), .groups= "keep")
  
  healthcareCosts_breakdown = cost_estimates %>%
    select(setting,booster_vax_scenario,intervention,intervention_target_group,patient_type,cost)
  
  # ggplot(healthcareCosts_breakdown) + geom_col(aes(x=patient_type,y=cost)) +
  #   facet_grid(booster_vax_scenario ~.)
  
  result = list(healthcareCosts_averted = healthcareCosts_averted,
                healthcareCosts_breakdown = healthcareCosts_breakdown)  
  return(result)
}

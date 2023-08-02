
healthCareCostsAverted_estimator <- function(
    LIST_CEA_settings,
    MASTER_antiviral_simulations,
    TORNADO_PLOT_OVERRIDE,
    toggle_uncertainty = TOGGLE_uncertainty,
    fitted_distributions = local_fitted_distributions,
    cutoff_sampling = 1000000 #cutoff number at which we do not sample from the distribution
    ){
  
  ### PART ONE: load antiviral simulation ######################################
  TRANSLATED_antiviral_simulations = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == TRUE) %>% #don't care about age-specific incidence since we don't have age-specific costs for access to healthcare
    rename(count_outcomes = value) %>%
    filter(outcome %in% c("hosp","hosp_after_antivirals","mild")) %>%
    select(evaluation_level,setting, outcome, booster_vax_scenario, intervention, intervention_target_group, count_outcomes)
    
  if (nrow(TRANSLATED_antiviral_simulations[!(TRANSLATED_antiviral_simulations$intervention == "no intervention" ),]) #intervention_target_group is understandably NA 
      != nrow(na.omit(TRANSLATED_antiviral_simulations))){stop("NA introduced")}
  ##############################################################################
  
  
  
  ### <intermission>  Reduced LOS (outcome == "hosp_after_antivirals") #########
  #analytically propagating error, see scanned proof 12/05/2023 in 1_derivation
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
    for (row_num in 1:nrow(reduced_LOS_estimates)){
      #sampling reduced LOS per hospitalised individual who had received antivirals
      if (reduced_LOS_estimates$count_outcomes[row_num] < cutoff_sampling){ #if sampling less than a million times
        this_sample = data.frame(est = rnorm (reduced_LOS_estimates$count_outcomes[row_num],mean = new_mean, sd = new_sd)) %>%
          mutate(est = case_when(
            est <0 ~ 0,
            TRUE ~ est
          ))
        reduced_LOS_estimates$estimate[row_num] = sum(this_sample$est)
        rm(this_sample)
      } else{
        reduced_LOS_estimates$estimate[row_num] = reduced_LOS_estimates$count_outcomes[row_num] * new_mean
      }
    }
    
  } else if (toggle_uncertainty == "fixed"){
    reduced_LOS_estimates$estimate = reduced_LOS_estimates$count_outcomes * (exp(X) - 1) * Y
  }
  
  #adding reduced LOS to TRANSLATED_antiviral_simulations for this cost to be calculated alongside reduced cost due to hospital admissions prevented
  workshop = reduced_LOS_estimates %>%
    mutate(count_outcomes = estimate,
           outcome = "hosp") %>%
    select(-estimate)
  rm(reduced_LOS_estimates)
  
  if ("net" %in% unique(MASTER_antiviral_simulations$evaluation_level)){
    workshop = workshop %>% select(-evaluation_level)
    workshop = crossing(workshop,evaluation_level = c("incremental","net")) #incremental and net will be the same!
    workshop$count_outcomes[workshop$evaluation_level == "net"] = workshop$count_outcomes[workshop$evaluation_level == "net"] * -1 #net healthcare cost will be net hosp + net outpatient - reduced LOS
  }

  TRANSLATED_antiviral_simulations = TRANSLATED_antiviral_simulations[TRANSLATED_antiviral_simulations$outcome %in% c("hosp","mild"),]
  TRANSLATED_antiviral_simulations =   rbind(TRANSLATED_antiviral_simulations,workshop) %>%
    group_by(evaluation_level,setting,outcome,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(count_outcomes = sum(count_outcomes), .groups="keep") #including hosp $ from reduced LOS of inpatients who recieved antivirals with other inpatients
  rm(workshop)
  ##############################################################################
  
  
  
  ### PART TWO: calculating healthcare costs ###################################
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
  rm(TRANSLATED_antiviral_simulations)
  
  if (toggle_uncertainty == "rand"){
    #sampling cost of each outpatient visit and each hospital admission
    for (row_num in 1:nrow(cost_estimates)){
      if (cost_estimates$patient_type[row_num] == "inpatient"){
        if (cost_estimates$count_outcomes[row_num]<0){multiplier = -1
        } else {multiplier = 1}
        
        if (cost_estimates$count_outcomes[row_num] < cutoff_sampling){ #if sampling less than a million times
          this_sample = data.frame(est = rnorm(abs(cost_estimates$count_outcomes[row_num]), mean = cost_estimates$param1[row_num], sd = cost_estimates$param2[row_num])) %>%
            mutate(est = case_when(
              est <0 ~ 0,
              TRUE ~ est
            ))
          cost_estimates$cost[row_num] = sum(this_sample$est)*multiplier
          rm(this_sample)
        } else{
          cost_estimates$cost[row_num] = cost_estimates$count_outcomes[row_num] * cost_estimates$mean_cost[row_num] * multiplier
        }
      
        if (length(TORNADO_PLOT_OVERRIDE)>0){
          if("inpatient" %in% names(TORNADO_PLOT_OVERRIDE)){cost_estimates$cost[row_num] = cost_estimates$cost[row_num] *TORNADO_PLOT_OVERRIDE$inpatient}
        }

      } else if (cost_estimates$patient_type[row_num] == "outpatient"){
        if (cost_estimates$count_outcomes[row_num] < cutoff_sampling){ #if sampling less than a million times
          this_sample = data.frame(est = rlnorm(cost_estimates$count_outcomes[row_num], meanlog = cost_estimates$param1[row_num], sdlog = cost_estimates$param2[row_num])) %>%
            mutate(est = case_when(
              est <0 ~ 0,
              TRUE ~ est
            ))
          cost_estimates$cost[row_num] = sum(this_sample$est)
          rm(this_sample)
        } else{
          cost_estimates$cost[row_num] = cost_estimates$count_outcomes[row_num] * cost_estimates$mean_cost[row_num]
        }
        
        if (length(TORNADO_PLOT_OVERRIDE)>0){
          if("outpatient" %in% names(TORNADO_PLOT_OVERRIDE)){cost_estimates$cost[row_num] = cost_estimates$cost[row_num]*TORNADO_PLOT_OVERRIDE$outpatient}
        }
      }
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
  ##############################################################################

  
  
  ### Export result  ###########################################################
  healthcareCosts_breakdown = cost_estimates %>%
    select(evaluation_level,setting,booster_vax_scenario,intervention,intervention_target_group,patient_type,cost)
  # ggplot(healthcareCosts_breakdown) + geom_col(aes(x=patient_type,y=cost)) +
  #   facet_grid(booster_vax_scenario ~.)
  rm(cost_estimates)
  
  healthcareCosts_averted = healthcareCosts_breakdown %>%
    group_by(evaluation_level,setting,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(cost = sum(cost), .groups= "keep")
  
  result = list(healthcareCosts_averted = healthcareCosts_averted,
                healthcareCosts_breakdown = healthcareCosts_breakdown)  
  
  return(result)
}
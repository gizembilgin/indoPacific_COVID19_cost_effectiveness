
healthCareCostsAverted_estimator <- function(LIST_CEA_settings,
                                       toggle_uncertainty = TOGGLE_uncertainty,
                                       TORNADO_PLOT_OVERRIDE = list()){
  
  ### Load RECORD_antiviral_model_simulations ####################################
  # We would like a data set with the following columns:
  # setting, booster_vax_scenario, outcome, count_outcomes_averted
  
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
    rename(count_outcomes_averted = value) %>%
    filter(outcome %in% c("hosp","hosp_after_antivirals","mild")) %>%
    
    select(setting, outcome, booster_vax_scenario, intervention, intervention_target_group, count_outcomes_averted)
    
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
    group_by(setting, outcome, booster_vax_scenario, intervention, intervention_target_group) %>%
    summarise(mean = mean(count_outcomes_averted),
              median = median(count_outcomes_averted),
              sd = sd(count_outcomes_averted),
              UB = mean + 1.96*sd,
              LB = mean -1.96*sd,
              .groups="keep")
  
  rm(workshop,MASTER_antiviral_simulations)
  #_______________________________________________________________________________
  ##############################################################################
  ##############################################################################
  
  
  
  
  ### Calculate healthcare costs ##############################################
  load(file = "2_inputs/hosp_adm.Rdata")
  hosp_adm = hosp_adm %>%
    rename(setting = ISO3_code,
           sd_cost = sd,
           mean_cost = estimate) %>%
    mutate(patient_type = "inpatient",
           outcome = "hosp") %>%
    select(setting,patient_type,outcome,mean_cost,sd_cost)
  
  load(file = "2_inputs/WHO_CHOICE_2022.Rdata")
  outpatient = WHO_CHOICE_2022 %>% 
    filter(patient_type == "outpatient" &
             care_setting == "Health centre (no beds)" &
             currency_short == "USD" &
             statistic %in% c("model_prediction","SD")) %>%
    pivot_wider(names_from = statistic,values_from=value) %>%
    rename(setting = ISO3_code,
           mean_cost = model_prediction,
           sd_cost = SD) %>%
    mutate(outcome = "mild") %>%
    select(setting,patient_type,outcome,mean_cost,sd_cost)
  
  healthcare_access = data.frame(setting = c("FJI","IDN","PNG","TLS"),
                                 access = c(0.816,0.7,0.7,0.766))
  
  cost_estimates = rbind(hosp_adm,outpatient)
  cost_estimates = TRANSLATED_antiviral_simulations %>%
    filter(outcome %in% c("hosp","mild")) %>%
    ungroup() %>%
    right_join(cost_estimates,by=c("setting","outcome"),relationship = "many-to-many") %>%
    
    #analytically propagating the error
    mutate(mean_propogated= mean * mean_cost,
           sd_propogated = mean_propogated * sqrt((sd/mean)^2 + (sd_cost/mean_cost)^2)) %>%
    select(setting,booster_vax_scenario,intervention,intervention_target_group,patient_type,mean_propogated,sd_propogated) %>%
    
    #modifying outpatient costs by access to care
    left_join(healthcare_access, by = "setting", relationship = "many-to-many") %>%
    mutate(
      mean_propogated = case_when(
        patient_type == "outpatient" ~ mean_propogated * access,
        TRUE ~ mean_propogated
      ),
      sd_propogated = case_when(
        patient_type == "outpatient" ~ sd_propogated * access,
        TRUE ~ sd_propogated
      )
    ) 
  #___________________________________________________________________________
  
  
  ## Reduced LOS (outcome == "hosp_after_antivirals")
  
  

  
  return()
}

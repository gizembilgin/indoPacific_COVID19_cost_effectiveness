
sample_compartmentalModel_run <- function(LIST_CEA_settings,
                                          LIST_booster_vax_scenarios,
                                          LIST_antiviral_elig_groups,
                                          LIST_antiviral_types,
                                          sampling_strategy = "empirical_distribution",
                                          toggle_uncertainty = TOGGLE_uncertainty){
  
  rootpath = str_replace(getwd(), "GitHub_vaxAllocation/4_cost_effectiveness_analysis","")
  MASTER_antiviral_simulations = data.frame()
  
  for (i in 1:length(LIST_CEA_settings)){
    this_setting = LIST_CEA_settings[[i]]
    if (this_setting == "PNG"){this_setting = "PNG_low_beta"}
    
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
    
    if (this_setting == "PNG_low_beta"){this_setting = "PNG"}
    
    df_this_setting = RECORD_antiviral_model_simulations %>% mutate(setting = this_setting)
    MASTER_antiviral_simulations = bind_rows(MASTER_antiviral_simulations,df_this_setting)
  }
  rm(RECORD_antiviral_model_simulations)
  
  #CHOICE - sampling from the empirical distribution of each parameter created by 100 simulations; OR take one stochasticmodel run 
  if (toggle_uncertainty == "fixed"){
    sampled_df = MASTER_antiviral_simulations %>%
      group_by(outcome, antiviral_type, antiviral_target_group, intervention, evaluation_group, vax_scenario, vax_scenario_risk_group, age_group, result,country,setting_beta,setting) %>%
      summarise(intervention_doses_delivered = mean(intervention_doses_delivered),
                value = mean(value),
                .groups="keep") %>%
      ungroup()
  } else if (sampling_strategy == "empirical_distribution"){
    sampled_df = MASTER_antiviral_simulations %>%
      group_by(outcome, antiviral_type, antiviral_target_group, intervention, evaluation_group, vax_scenario, vax_scenario_risk_group, age_group, result,country,setting_beta,setting) %>%
      summarise(intervention_doses_delivered = sample(intervention_doses_delivered, size = 1),
                value = sample(value, size = 1),
                .groups="keep") %>%
      ungroup()
  } else if (sampling_strategy == "single_run"){
    sampled_df = MASTER_antiviral_simulations %>%
      group_by(vax_scenario_risk_group,setting_beta) %>%
      summarise(this_run = sample(run_ID, size = 1),
                .groups = "keep")
    sampled_df = MASTER_antiviral_simulations %>%
      filter(run_ID %in% sampled_df$this_run)
  }
  if (nrow(sampled_df) != nrow(MASTER_antiviral_simulations)/100){stop("Did you not run 100 simulations?")}

  sampled_df = sampled_df %>%
    filter(vax_scenario %in% LIST_booster_vax_scenarios) %>%
    filter(antiviral_type %in% LIST_antiviral_types | is.na(antiviral_type)) %>%
    filter(antiviral_target_group %in% LIST_antiviral_elig_groups | is.na(antiviral_target_group)) %>%
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
    select(-vax_scenario)%>%
    
    mutate(evaluation_level = 
             case_when(
               is.na(evaluation_group) ~ "incremental", #is.na() when antiviral_type and antiviral_target_group is.na(),
               evaluation_group == "pop_level" ~ "incremental", #rename, used to be "pop_level" to distinguish between pop-level and high-risk incremental changes
               TRUE ~ evaluation_group
             )) %>%
    filter(evaluation_level %in% c("incremental","net")) %>%
    select(-evaluation_group) %>%
    
    #DECISION - CEA of antivirals for a whole year (as of 01/01/2023)
    filter(!(intervention %in% c(' "antiviral after booster 2023-01-01","antiviral prior to booster 2023-01-01"'))) %>%
    mutate(intervention = case_when(
      is.na(intervention) & evaluation_level == "net" & booster_vax_scenario == "no booster dose" ~ "no intervention",
      is.na(intervention) & evaluation_level == "net" ~ "booster dose 2023-03-01",
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
    
    filter(result == "n")

  return(sampled_df)
}
                                       

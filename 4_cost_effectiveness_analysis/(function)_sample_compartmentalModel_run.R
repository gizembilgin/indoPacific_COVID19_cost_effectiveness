
sample_compartmentalModel_run <- function(LIST_CEA_settings,
                                          sampling_strategy = "empirical_distribution"){
  
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
    
    if (this_setting == "PNG_low_beta" & !("PNG_high_beta" %in% settings_to_plot)){this_setting = "PNG"}
    
    df_this_setting = RECORD_antiviral_model_simulations %>% mutate(setting = this_setting)
    MASTER_antiviral_simulations = bind_rows(MASTER_antiviral_simulations,df_this_setting)
  }
  rm(RECORD_antiviral_model_simulations)
  
  #CHOICE - sampling from the empirical distribution of each parameter created by 100 simulations; OR take one stochasticmodel run 
  if (sampling_strategy == "empirical_distribution"){
    sampled_df = MASTER_antiviral_simulations %>%
      group_by(outcome, antiviral_type, antiviral_target_group, intervention, evaluation_group, vax_scenario, vax_scenario_risk_group, age_group, result,country,setting_beta,setting) %>%
      summarise(intervention_doses_delivered = sample(intervention_doses_delivered, size = 1),
                value = sample(value, size = 1),
                .groups="keep") %>%
      ungroup()
  } else if (sampling_strategy == "single_run"){
    sampled_df = MASTER_antiviral_simulations %>%
      group_by(vax_scenario_risk_group,setting_beta) %>%
      summarise(this_run = sample(run_ID, size = 1))
    sampled_df = MASTER_antiviral_simulations %>%
      filter(run_ID %in% sampled_df$this_run)
  }
  if (nrow(sampled_df) != nrow(MASTER_antiviral_simulations)/100){stop("Did you not run 100 simulations?")}


  return(sampled_df)
}
                                       

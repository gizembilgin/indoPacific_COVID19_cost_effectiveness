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
    latest_file = RECORD_antiviral_model_simulations 
    
    prev_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)-1]]
    load(file = paste(rootpath, "x_results/", prev_file, sep = ''))
    prev_file = RECORD_antiviral_model_simulations
    
  } 
  
  if (this_setting == "PNG_low_beta"){this_setting = "PNG"}
  latest_file = latest_file %>% mutate(setting = paste(this_setting,"latest"))
  prev_file = prev_file %>% mutate(setting = paste(this_setting,"prev"))
  MASTER_antiviral_simulations = bind_rows(MASTER_antiviral_simulations,prev_file,latest_file)
    
}
rm(RECORD_antiviral_model_simulations)

sampled_df = MASTER_antiviral_simulations %>%
  group_by(outcome, antiviral_type, antiviral_target_group, intervention, evaluation_group, vax_scenario, vax_scenario_risk_group, age_group, result,country,setting_beta,setting) %>%
  summarise(intervention_doses_delivered = mean(intervention_doses_delivered),
            value = mean(value),
            .groups="keep") %>%
  ungroup()

check = sampled_df %>% 
  pivot_wider(names_from = setting, values_from = value) %>% 
  filter( (`PNG latest` > `PNG prev`*1.2|`PNG latest` < `PNG prev`*0.8))

check = sampled_df %>% filter(evaluation_group == "net") %>%
  select(-setting_beta,-country,-evaluation_group,-result,-vax_scenario_risk_group,-age_group)

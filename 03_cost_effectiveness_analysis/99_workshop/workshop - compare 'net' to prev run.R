# This workshop compares the latest transmission model run to the previous
# It was used to check that nothing had changed when net outcomes were saved for the first time

LIST_CEA_settings = c("PNG_low_beta")
rootpath = str_replace(getwd(), "GitHub_vaxAllocation/03_cost_effectiveness_analysis","")
MASTER_antiviral_simulations = data.frame()

for (i in 1:length(LIST_CEA_settings)){
  this_setting = LIST_CEA_settings[[i]]
  if (this_setting == "PNG"){this_setting = "PNG_low_beta"}
  
  list_poss_Rdata = list.files(
    path = paste0(rootpath, "x_results/"),
    pattern = paste0("AntiviralRun_", this_setting, "_", this_risk_group, "*")
  )
  if (length(list_poss_Rdata) > 0) {
    list_poss_Rdata_details = double()
    for (j in 1:length(list_poss_Rdata)) {
      list_poss_Rdata_details = rbind(list_poss_Rdata_details,file.info(paste0(rootpath, "x_results/", list_poss_Rdata[[j]]))$mtime)
    }
    latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
    load(file = paste0(rootpath, "x_results/", latest_file))
    latest_file = RECORD_antiviral_model_simulations 
    
    prev_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)-1]]
    load(file = paste0(rootpath, "x_results/", prev_file))
    prev_file = RECORD_antiviral_model_simulations
    
  } 
  
  if (this_setting == "PNG_low_beta"){this_setting = "PNG"}
  latest_file = latest_file %>% mutate(setting = paste(this_setting,"latest",sep="_"))
  prev_file = prev_file %>% mutate(setting = paste(this_setting,"prev",sep="_"))
  MASTER_antiviral_simulations = bind_rows(MASTER_antiviral_simulations,prev_file,latest_file)
    
}
rm(RECORD_antiviral_model_simulations)

sampled_df = MASTER_antiviral_simulations %>%
  group_by(outcome, antiviral_type, antiviral_target_group, intervention, evaluation_level, booster_vax_scenario, vax_scenario_risk_group, age_group, result,setting) %>%
  summarise(intervention_doses_delivered = mean(intervention_doses_delivered),
            value = mean(value),
            .groups="keep") %>%
  ungroup()

check = sampled_df %>% 
  pivot_wider(names_from = setting, values_from = value) %>% 
  filter( round(PNG_latest) != round(PNG_prev) & (PNG_latest > PNG_prev*1.2|PNG_latest < PNG_prev*0.8)) %>%
  mutate(comparison = (PNG_latest-PNG_prev)/PNG_latest)

check = sampled_df %>% filter(evaluation_level == "net") %>%
  select(-evaluation_level,-result,-vax_scenario_risk_group,-age_group)

#workshop - plot stochatic vs det.R



### Check for internal consistency
check = MASTER_antiviral_simulations %>%
  filter(setting == "PNG_latest" & evaluation_level == "net")
subset_ageSpecific = check %>% filter(is.na(age_group)==FALSE) %>% group_by(outcome,antiviral_type,antiviral_target_group,intervention,booster_vax_scenario,vax_scenario_risk_group) %>% summarise(n_ageSpecific=sum(value)) 
subset_overall = check %>% filter(is.na(age_group)) %>% group_by(outcome,antiviral_type,antiviral_target_group,intervention,booster_vax_scenario,vax_scenario_risk_group) %>% summarise(n_overall=sum(value))

compare = subset_overall %>% 
  left_join(subset_ageSpecific)%>% 
  filter(round(n_overall) != round(n_ageSpecific))



### COMPARISON OF OUTCOMES_WITHOUT_ANTIVIRALS
this_risk_group_name = "pregnant_women" #adults_with_comorbidities,pregnant_women
setting_beta = "TLS"

if (exists("master_toggles")){setting_beta = master_toggles$setting_beta} 
setting = this_setting = substr(setting_beta,1,3)

#PREVIOUS FILE
list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = paste("antiviralSetUp_",setting_beta,"_",this_risk_group_name,"_*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste(rootpath,'x_results/',list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste(rootpath,"x_results/",latest_file,sep=''))
previous_file = RECORD_antiviral_setup$outcomes_without_antivirals %>%
  mutate(overall_prev = overall,
         high_risk_prev = high_risk)

#LATEST FILE
list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = paste("antiviralSetUp_",setting_beta,"_",this_risk_group_name,"_*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste(rootpath,'x_results/',list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste(rootpath,"x_results/",latest_file,sep=''))
latest_file = RECORD_antiviral_setup$outcomes_without_antivirals %>%
  mutate(overall_latest = overall,
         high_risk_latest = high_risk)


#COMPARISON 
comparison = latest_file %>%
  left_join(previous_file, by = c('outcome','vax_scenario','vax_scenario_risk_group')) %>%
  select(outcome,vax_scenario,vax_scenario_risk_group,overall_latest,overall_prev,high_risk_latest,high_risk_prev)
comparison %>% filter(round(overall_latest) != round(overall_prev) | round(high_risk_latest) != round(high_risk_prev))
View(comparison)
#_______________________________________________________________________________



## CHECK INTERNAL CONSISTENCY OF OUTCOMES_WITHOUT_ANTIVIRALS and AGESPECIFIC_OUTCOMES_WITHOUT_ANTIVIRALS
popLevel = RECORD_antiviral_setup$outcomes_without_antivirals
ageSpecific = RECORD_antiviral_setup$ageSpecific_outcomes_without_antivirals %>%
  group_by(outcome,vax_scenario,vax_scenario_risk_group) %>%
  summarise(ageSpecific_overall = sum(overall))
comparison = popLevel %>%
  left_join(ageSpecific, by = c('outcome','vax_scenario','vax_scenario_risk_group'))
comparison %>% filter(round(ageSpecific_overall) != round(overall))
comparison

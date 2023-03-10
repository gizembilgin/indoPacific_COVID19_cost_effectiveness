setting_beta = "PNG_low_beta"

setting = substr(setting_beta,1,3)
this_risk_group_name = "adults_with_comorbidities"

#load latest antiviralSetUp_* (transmission model run for 1 year)
list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = paste("antiviralSetUp_",setting_beta,"_",this_risk_group_name,"_*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste(rootpath,'x_results/',list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste(rootpath,"x_results/",latest_file,sep=''))

RECORD_outcomes_without_antivirals = RECORD_antiviral_setup$outcomes_without_antivirals %>%
  filter(vax_scenario != "all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults"  )
RECORD_likelihood_severe_outcome = RECORD_antiviral_setup$likelihood_severe_outcome %>%
  filter(vax_scenario != "all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults"  )
RECORD_incidence_log_tidy = RECORD_antiviral_setup$incidence_log_tidy %>%
  filter(vax_scenario != "all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults"  )
RECORD_exposed_log = RECORD_antiviral_setup$exposed_log %>%
  filter(vax_scenario != "all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults"  )
RECORD_incidence_log = RECORD_antiviral_setup$incidence_log %>%
  filter(vax_scenario != "all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults"  )
RECORD_vaccination_history_FINAL = RECORD_antiviral_setup$vaccination_history_FINAL %>%
  filter(vax_scenario != "all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults"  )



#run setup up to queue
#run ticket = 6

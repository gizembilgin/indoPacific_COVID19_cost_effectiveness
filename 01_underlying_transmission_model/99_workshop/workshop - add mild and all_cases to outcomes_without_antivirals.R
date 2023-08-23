
for (setting_beta in c('IDN','TLS','FJI','PNG_low_beta','PNG_high_beta')){
  for (this_risk_group_name in c("adults_with_comorbidities","pregnant_women")){
    
    if (setting_beta == "PNG_high_beta" & this_risk_group_name == "pregnant_women"){
      
    } else{
      ### load latest antiviralSetUp_* 
      list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = paste("antiviralSetUp_",setting_beta,"_",this_risk_group_name,"_*",sep=""))
      list_poss_Rdata_details = double()
      for (i in 1:length(list_poss_Rdata)){
        list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                        file.info(paste(rootpath,'x_results/',list_poss_Rdata[[i]],sep=''))$mtime)
      }
      latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
      load(file = paste(rootpath,"x_results/",latest_file,sep=''))
      #_______________________________________________________________________________
      
      
      
      ### addition for CEA
      #colnames: outcome = c(total_cases,mild), overall, high_risk, vax_scenario,vax_scenario_risk_group
      total_cases = RECORD_antiviral_setup$incidence_log_tidy %>%
        group_by(risk_group, vax_scenario, vax_scenario_risk_group) %>%
        summarise(incidence = sum(incidence))
      workshop = data.frame()
      for (this_scenario in unique(total_cases$vax_scenario)){
        row = data.frame(outcome = "total_cases",
                         overall = sum(total_cases$incidence[total_cases$vax_scenario == this_scenario]),
                         high_risk = total_cases$incidence[total_cases$risk_group == this_risk_group_name & total_cases$vax_scenario == this_scenario],
                         vax_scenario = this_scenario,
                         vax_scenario_risk_group = this_risk_group_name)
        workshop = rbind(workshop,row)
      }
      total_cases = workshop
      
      mild = RECORD_antiviral_setup$incidence_log_tidy %>%
        group_by(risk_group, age_group, vax_scenario, vax_scenario_risk_group) %>%
        summarise(incidence = sum(incidence)) %>%
        rename(agegroup = age_group) %>%
        left_join(RECORD_antiviral_setup$prop_sympt, by = c("agegroup")) %>%
        mutate(incidence = incidence * value) %>%
        group_by(risk_group, vax_scenario, vax_scenario_risk_group) %>%
        summarise(incidence = sum(incidence))
      workshop = data.frame()
      for (this_scenario in unique(mild$vax_scenario)){
        row = data.frame(outcome = "mild",
                         overall = sum(mild$incidence[mild$vax_scenario == this_scenario]),
                         high_risk = mild$incidence[mild$risk_group == this_risk_group_name & mild$vax_scenario == this_scenario],
                         vax_scenario = this_scenario,
                         vax_scenario_risk_group = this_risk_group_name)
        workshop = rbind(workshop,row)
      }
      mild = workshop
      
      RECORD_antiviral_setup$outcomes_without_antivirals = rbind(RECORD_antiviral_setup$outcomes_without_antivirals,mild,total_cases)
      #_______________________________________________________________________________
      
      
      ### save
      save(RECORD_antiviral_setup, file = paste(rootpath,"x_results/antiviralSetUp_",setting_beta,"_",this_risk_group_name,"_",Sys.Date(),".Rdata",sep=''))
      #_______________________________________________________________________________
    }
  }
}
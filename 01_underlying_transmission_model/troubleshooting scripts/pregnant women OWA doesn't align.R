setting = this_setting = setting_beta = "IDN"

### LOAD #######################################################################
### LOAD adults_with_comorbidities
this_risk_group_name = "adults_with_comorbidities"

#load latest antiviralSetUp_* (transmission model run for 1 year)
list_poss_Rdata = list.files(path=paste("x_results/",sep=''),pattern = paste("antiviralSetUp_",setting_beta,"_",this_risk_group_name,"_*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste('x_results/',list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste("x_results/",latest_file,sep=''))

RAS_adults_with_comorbidities = RECORD_antiviral_setup


### LOAD pregnant_women
this_risk_group_name = "pregnant_women"

#load latest antiviralSetUp_* (transmission model run for 1 year)
list_poss_Rdata = list.files(path=paste("x_results/",sep=''),pattern = paste("antiviralSetUp_",setting_beta,"_",this_risk_group_name,"_*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste('x_results/',list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste("x_results/",latest_file,sep=''))

RAS_pregnant_women = RECORD_antiviral_setup
#_______________________________________________________________________________




### COMPARE INCIDENCE LOG ######################################################
ggplot() + 
  geom_point(data= RAS_adults_with_comorbidities$incidence_log[RAS_adults_with_comorbidities$incidence_log$vax_scenario == "all willing adults vaccinated with a primary schedule",], 
              aes(x=date,y=rolling_average, color = "red")) +
  geom_point(data= RAS_pregnant_women$incidence_log[RAS_pregnant_women$incidence_log$vax_scenario == "all willing adults vaccinated with a primary schedule",], 
             aes(x=date,y=rolling_average, color = "blue"))
#_______________________________________________________________________________




### COMPARE outcomes_without_antiviral #########################################
ggplot() + 
  geom_point(data= RAS_adults_with_comorbidities$exposed_log[RAS_adults_with_comorbidities$exposed_log$vax_scenario == "all willing adults vaccinated with a primary schedule",], 
             aes(x=date,y=reinfection_ratio , color = as.factor(vax_scenario_risk_group))) +
  geom_point(data= RAS_pregnant_women$exposed_log[RAS_pregnant_women$exposed_log$vax_scenario == "all willing adults vaccinated with a primary schedule",], 
             aes(x=date,y=reinfection_ratio , color = as.factor(vax_scenario_risk_group)))
#_______________________________________________________________________________



### COMPARE outcomes_without_antiviral #########################################
RAS_adults_with_comorbidities$outcomes_without_antivirals %>% 
  filter(vax_scenario == "all willing adults vaccinated with a primary schedule")

RAS_pregnant_women$outcomes_without_antivirals %>% 
  filter(vax_scenario == "all willing adults vaccinated with a primary schedule")
#_______________________________________________________________________________



### COMPARE antiviral setup completely #########################################
to_join = RAS_pregnant_women$outcomes_without_antivirals %>%
  select(-vax_scenario_risk_group) %>%
  rename(overall_preg = overall,
         preg = high_risk)

compare =  RAS_adults_with_comorbidities$outcomes_without_antivirals %>%
  select(-vax_scenario_risk_group) %>%
  rename(overall_comorb = overall) %>% 
  left_join(to_join, by = c("outcome","vax_scenario")) %>%
  select(outcome,vax_scenario,overall_comorb,overall_preg,high_risk,preg)

View(compare)


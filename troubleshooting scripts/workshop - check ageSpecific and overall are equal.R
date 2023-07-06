workshop_1 = OWA_with_booster_doses %>% 
  group_by(outcome) %>% 
  summarise(n = sum(overall))
workshop_2 = AS_OWA_with_booster_doses%>% 
  group_by(outcome) %>% 
  summarise(n = sum(overall))
#nrow = 0

workshop_1 = prevented_by_antivirals %>% 
  group_by(outcome,antiviral_type,antiviral_target_group,antiviral_start_date,intervention,evaluation_group) %>% 
  summarise(n = sum(n))
workshop_2 = ageSpecific_prevented_by_antivirals%>% 
  group_by(outcome,antiviral_type,antiviral_target_group,antiviral_start_date,intervention,evaluation_group) %>% 
  summarise(n = sum(n))
#nrow = 0

vaccine_only_row = OWA_with_booster_doses %>%
  rename(n=overall) %>%
  select(-high_risk,-vax_scenario,-vax_scenario_risk_group) %>%
  mutate(evaluation_group = "net")
workshop_1 = prevented_by_antivirals %>%
  filter(antiviral_start_date == "2023-01-01" & evaluation_group == "overall" & outcome != "hosp_after_antivirals") %>%
  filter(intervention == "antiviral 2023-01-01" | is.na(intervention) == TRUE) %>%
  mutate(evaluation_group = "net") %>%
  rename(prevented = n) %>% 
  group_by(outcome,antiviral_type,antiviral_target_group,antiviral_start_date,intervention,evaluation_group) %>% 
  summarise(n = sum(prevented))
vaccine_only_row = AS_OWA_with_booster_doses %>%
  rename(n=overall) %>%
  select(-vax_scenario,-vax_scenario_risk_group) %>%
  mutate(evaluation_group = "net")
workshop_2 = ageSpecific_prevented_by_antivirals %>%
  filter(antiviral_start_date == "2023-01-01" & evaluation_group == "overall" & outcome != "hosp_after_antivirals") %>%
  filter(intervention == "antiviral 2023-01-01" | is.na(intervention) == TRUE) %>%
  mutate(evaluation_group = "net") %>%
  rename(prevented = n)%>% 
  group_by(outcome,antiviral_type,antiviral_target_group,antiviral_start_date,intervention,evaluation_group) %>% 
  summarise(n = sum(prevented))
#nrow = 0




vaccine_only_row = OWA_with_booster_doses %>%
  rename(n=overall) %>%
  select(-high_risk,-vax_scenario,-vax_scenario_risk_group) %>%
  mutate(evaluation_group = "net")
workshop_1 = prevented_by_antivirals %>% 
  filter(antiviral_start_date == "2023-01-01" & evaluation_group == "overall" & outcome != "hosp_after_antivirals") %>%
  filter(intervention == "antiviral 2023-01-01" | is.na(intervention) == TRUE) %>%
  mutate(evaluation_group = "net") %>%
  rename(prevented = n) %>%
  left_join(vaccine_only_row, by = join_by(outcome, evaluation_group)) %>%
  mutate(n=n-prevented) %>%
  select(-prevented)%>% 
  group_by(outcome,antiviral_type,antiviral_target_group,antiviral_start_date,intervention,evaluation_group) %>% 
  summarise(n = sum(n))

vaccine_only_row = AS_OWA_with_booster_doses %>%
  rename(n=overall) %>%
  select(-vax_scenario,-vax_scenario_risk_group) %>%
  mutate(evaluation_group = "net")
workshop_2 = ageSpecific_prevented_by_antivirals %>% filter(evaluation_group != "net")%>%
  filter(antiviral_start_date == "2023-01-01" & evaluation_group == "overall" & outcome != "hosp_after_antivirals") %>%
  filter(intervention == "antiviral 2023-01-01" | is.na(intervention) == TRUE) %>%
  mutate(evaluation_group = "net") %>%
  rename(prevented = n) %>%
  right_join(vaccine_only_row[vaccine_only_row$outcome %in% unique(ageSpecific_prevented_by_antivirals$outcome),]
             ,by = join_by(outcome, age_group, evaluation_group)) 
expanding_underage_rows = workshop_2 %>%
  filter(is.na(antiviral_type)) %>%
  select(-antiviral_type,-antiviral_target_group,-antiviral_start_date,-intervention,-prevented)
expanding_underage_rows = crossing(expanding_underage_rows,
                                   antiviral_type = unique(ageSpecific_prevented_by_antivirals$antiviral_type),
                                   antiviral_target_group = unique(ageSpecific_prevented_by_antivirals$antiviral_target_group),
                                   antiviral_start_date = unique(ageSpecific_prevented_by_antivirals$antiviral_start_date),
                                   intervention = unique(ageSpecific_prevented_by_antivirals$intervention),
                                   prevented = 0)
workshop_2 = workshop_2 %>%
  filter(is.na(antiviral_type) == FALSE)
workshop_2 = rbind(workshop_2,expanding_underage_rows) %>%
  mutate(n=n-prevented) %>%
  select(-prevented)%>% 
  group_by(outcome,antiviral_type,antiviral_target_group,antiviral_start_date,intervention,evaluation_group) %>% 
  summarise(n = sum(n))
#nrow = 0
 

prevented_by_antivirals = prevented_by_antivirals %>% filter(evaluation_group != "net")
ageSpecific_prevented_by_antivirals = ageSpecific_prevented_by_antivirals  %>% filter(evaluation_group != "net")

workshop_1 = prevented_by_antivirals %>%
  filter(evaluation_group == "net")
workshop_2 = ageSpecific_prevented_by_antivirals %>%
  filter(evaluation_group == "net") %>%
  group_by(outcome,antiviral_start_date,antiviral_type,antiviral_target_group,intervention,evaluation_group) %>%
  summarise(n = sum(n))
#nrow = 0

version_1 = workshop_1 %>%
  rename(n_1 = n)
version_2 = workshop_2 %>%
  rename(n_2 = n)

check = version_1 %>% left_join(version_2) %>%
  mutate(compare = n_1 - n_2) %>%
  filter(round(compare,digits=5) != 0 | is.na(compare))
nrow(check)



###CHECK rows taken out of antiviral_model_worker
# subset = this_worker_result %>% filter(evaluation_group == "net")
# #subset = bind_rows(one_complete_run,one_complete_ageSpecific_run)
# # subset = bind_rows(prevented_by_antivirals,ageSpecific_prevented_by_antivirals) %>% 
# #   mutate(vax_scenario = toggle_vax_scenario,
# #          vax_scenario_risk_group = toggle_vax_scenario_risk_group) 
# 
# subset_ageSpecific = subset %>% filter(is.na(age_group)==FALSE) %>% group_by(outcome,antiviral_type,antiviral_target_group,intervention,vax_scenario) %>% summarise(n_ageSpecific=sum(n)) 
# subset_overall = subset %>% filter(is.na(age_group)) %>% group_by(outcome,antiviral_type,antiviral_target_group,intervention,vax_scenario) %>% summarise(n_overall=sum(n))
# 
# compare = subset_overall %>% left_join(subset_ageSpecific)
# compare = compare %>% filter(round(n_overall) != round(n_ageSpecific))
# if(nrow(compare)>0){stop()}
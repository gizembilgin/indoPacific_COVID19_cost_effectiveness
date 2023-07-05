workshop_1 = OWA_with_booster_doses %>% 
  group_by(outcome) %>% 
  summarise(n = sum(overall))
workshop_2 = AS_OWA_with_booster_doses%>% 
  group_by(outcome) %>% 
  summarise(n = sum(overall))
#nrow = 0

workshop_1 = prevented_by_antivirals %>% 
  group_by(outcome,antiviral_type,antiviral_target_group) %>% 
  summarise(n = sum(n))
workshop_2 = ageSpecific_prevented_by_antivirals%>% 
  group_by(outcome,antiviral_type,antiviral_target_group) %>% 
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
  group_by(outcome,antiviral_type,antiviral_target_group) %>% 
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
  group_by(outcome,antiviral_type,antiviral_target_group) %>% 
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
  group_by(outcome,antiviral_type,antiviral_target_group) %>% 
  summarise(n = sum(prevented))
vaccine_only_row = AS_OWA_with_booster_doses %>%
  rename(n=overall) %>%
  select(-vax_scenario,-vax_scenario_risk_group) %>%
  mutate(evaluation_group = "net")
workshop_2 = ageSpecific_prevented_by_antivirals %>%
  filter(antiviral_start_date == "2023-01-01" & evaluation_group == "overall" & outcome != "hosp_after_antivirals") %>%
  filter(intervention == "antiviral 2023-01-01" | is.na(intervention) == TRUE) %>%
  mutate(evaluation_group = "net") %>%
  rename(prevented = n) %>%
  left_join(vaccine_only_row,by = join_by(outcome, age_group, evaluation_group)) %>%
  mutate(n=n-prevented) %>%
  select(-prevented)%>% 
  group_by(outcome,antiviral_type,antiviral_target_group) %>% 
  summarise(n = sum(prevented))


version_1 = workshop_1 %>%
  rename(n_1 = n)
version_2 = workshop_2 %>%
  rename(n_2 = n)

version_1 %>% left_join(version_2) %>%
  mutate(compare = n_1 - n_2) %>%
  filter(round(compare,digits=5) != 0)

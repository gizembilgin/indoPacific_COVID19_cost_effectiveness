
cases_averted_log = data.frame()
vaccine_coverage_log = data.frame()

settings_to_plot = c("FJI","PNG_low_beta","TLS","IDN")
risk_group_name = "adults_with_comorbidities" #options: pregnant_women, adults_with_comorbidities


for (j in 1:length(settings_to_plot)){
  setting_beta = this_setting = settings_to_plot[j]
  if (setting_beta == "PNG_low_beta"){this_setting = "PNG"}
  
  rootpath = rootpath = gsub( "indoPacific_COVID19_cost_effectiveness/02_stochastic_outcomes_projections","",getwd())
  list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = paste("antiviralSetUp_",setting_beta,"_",this_risk_group_name,"_*",sep=""))
  list_poss_Rdata_details = double()
  for (i in 1:length(list_poss_Rdata)){
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste(rootpath,'x_results/',list_poss_Rdata[[i]],sep=''))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste(rootpath,"x_results/",latest_file,sep=''))
  
  workshop_pt1 = RECORD_antiviral_setup$incidence_log %>% 
    filter(vax_scenario_risk_group == risk_group_name) %>%
    filter(date>=as.Date('2023-01-01') & date<as.Date('2024-01-01')) %>%
    group_by(vax_scenario) %>%
    summarise(total_cases = sum(daily_cases))
  workshop_pt1$abs_cases_averted      = workshop_pt1$total_cases[workshop_pt1$vax_scenario == "all willing adults vaccinated with a primary schedule"] - workshop_pt1$total_cases
  workshop_pt1$percentage_cases_averted = workshop_pt1$abs_cases_averted/workshop_pt1$total_cases[workshop_pt1$vax_scenario == "all willing adults vaccinated with a primary schedule"]
  
  workshop_pt2 = RECORD_antiviral_setup$incidence_log_tidy %>% 
    filter(vax_scenario_risk_group == risk_group_name & risk_group == risk_group_name) %>%
    filter(date>=as.Date('2023-01-01') & date<as.Date('2024-01-01')) %>%
    group_by(vax_scenario) %>%
    summarise(total_high_risk_cases = sum(incidence)) 
  workshop_pt2$abs_high_risk_cases_averted      = workshop_pt2$total_high_risk_cases[workshop_pt2$vax_scenario == "all willing adults vaccinated with a primary schedule"] - workshop_pt2$total_high_risk_cases
  workshop_pt2$percentage_high_risk_case_averted = workshop_pt2$abs_high_risk_cases_averted/workshop_pt2$total_high_risk_cases[workshop_pt2$vax_scenario == "all willing adults vaccinated with a primary schedule"]
  
  workshop_pt3 = RECORD_antiviral_setup$incidence_log_tidy %>% 
    filter(vax_scenario_risk_group == risk_group_name & 
             age_group %in% c('60 to 69','70 to 100')) %>%
    filter(date>=as.Date('2023-01-01') & date<as.Date('2024-01-01')) %>%
    group_by(vax_scenario) %>%
    summarise(total_older_adult_cases = sum(incidence)) 
  workshop_pt3$abs_older_cases_averted        = workshop_pt3$total_older_adult_cases[workshop_pt3$vax_scenario == "all willing adults vaccinated with a primary schedule"] - workshop_pt3$total_older_adult_cases
  workshop_pt3$percentage_older_cases_averted = workshop_pt3$abs_older_cases_averted/workshop_pt3$total_older_adult_cases[workshop_pt3$vax_scenario == "all willing adults vaccinated with a primary schedule"]
  
  
  workshop = workshop_pt1 %>%
    left_join(workshop_pt2) %>%
    left_join(workshop_pt3)
  
  workshop_pt4 = RECORD_antiviral_setup$outcomes_without_antivirals %>%
    filter(outcome == 'booster_doses_delivered') %>%
    select(overall,vax_scenario)
  
  workshop = workshop %>% 
    left_join(workshop_pt4) %>%
    mutate(doses_per_case_averted = abs_cases_averted/overall,
           doses_per_high_risk_case_averted = abs_high_risk_cases_averted/overall,
           doses_per_older_case_averted = abs_older_cases_averted/overall) %>%
    mutate(setting = setting_beta)%>%
    mutate(vax_scenario_short = case_when(
      vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved two booster doses"  ~
        "booster to all 2nd booster",
      vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved two booster doses" ~
        "booster to high-risk prev 2nd booster",
      vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~
        "booster to all prev primary",
      vax_scenario ==  "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a first booster dose" ~
        "booster to all 1st booster",
      vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~
        "booster to high-risk prev primary",
      vax_scenario ==  "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a first booster dose" ~
        "booster to high-risk prev 1st booster",
      vax_scenario == "all willing adults vaccinated with a primary schedule" ~ 
        "no booster"
    )) %>%
    select(-vax_scenario)
  
  cases_averted_log = rbind(cases_averted_log,workshop)
  
  # row = RECORD_antiviral_setup$vaccination_history_FINAL %>%
  #   filter(age_group %in% c('60 to 69','70 to 100') &
  #            date <= as.Date('2023-01-01') &
  #            vax_scenario == "all willing adults vaccinated with a primary schedule") %>%
  #   group_by(dose) %>%
  #   summarise(doses_delivered = sum(doses_delivered_this_date)) %>%
  #   mutate(coverage = doses_delivered/sum(UN_pop_est$PopTotal[UN_pop_est$AgeGrp >= 60 & UN_pop_est$ISO3_code == this_setting])) %>%
  #   mutate(setting = this_setting,
  #          age_group = "60+")
  # vaccine_coverage_log = rbind(vaccine_coverage_log,row)
  # 
  # row = RECORD_antiviral_setup$vaccination_history_FINAL %>%
  #   filter(age_group %in% c("18 to 29","30 to 44","45 to 59","60 to 69","70 to 100") &
  #            date <= as.Date('2023-01-01') &
  #            vax_scenario == "all willing adults vaccinated with a primary schedule") %>%
  #   group_by(dose) %>%
  #   summarise(doses_delivered = sum(doses_delivered_this_date)) %>%
  #   mutate(coverage = doses_delivered/sum(UN_pop_est$PopTotal[UN_pop_est$AgeGrp >= 18 & UN_pop_est$ISO3_code == this_setting])) %>%
  #   mutate(setting = this_setting,
  #          age_group = "18+")
  # vaccine_coverage_log = rbind(vaccine_coverage_log,row)
  
  row = RECORD_antiviral_setup$vaccination_history_FINAL %>%
    filter(age_group %in% c('60 to 69','70 to 100') &
             date <= as.Date('2023-01-01') &
             vax_scenario == "all willing adults vaccinated with a primary schedule" &
             ((dose == 2 & vaccine_type != "Johnson & Johnson") | (dose == 1 & vaccine_type == "Johnson & Johnson"))) %>%
    ungroup() %>%
    summarise(doses_delivered = sum(doses_delivered_this_date)) %>%
    mutate(coverage = doses_delivered/sum(UN_pop_est$PopTotal[UN_pop_est$AgeGrp >= 60 & UN_pop_est$ISO3_code == this_setting])) %>%
    mutate(setting = this_setting,
           age_group = "60+")
  vaccine_coverage_log = rbind(vaccine_coverage_log,row)
  
  row = RECORD_antiviral_setup$vaccination_history_FINAL %>%
    filter(age_group %in% c("18 to 29","30 to 44","45 to 59","60 to 69","70 to 100") &
             date <= as.Date('2023-01-01') &
             vax_scenario == "all willing adults vaccinated with a primary schedule"&
             ((dose == 2 & vaccine_type != "Johnson & Johnson") | (dose == 1 & vaccine_type == "Johnson & Johnson"))) %>%
    ungroup() %>%
    summarise(doses_delivered = sum(doses_delivered_this_date)) %>%
    mutate(coverage = doses_delivered/sum(UN_pop_est$PopTotal[UN_pop_est$AgeGrp >= 18 & UN_pop_est$ISO3_code == this_setting])) %>%
    mutate(setting = this_setting,
           age_group = "18+")
  vaccine_coverage_log = rbind(vaccine_coverage_log,row)
}


 
cases_averted_log %>%
  rename(booster_doses_delivered = overall) %>%
  filter(is.na(doses_per_case_averted) == FALSE) %>%
  select(setting,vax_scenario_short,doses_per_case_averted,doses_per_high_risk_case_averted,doses_per_older_case_averted)
cases_averted_log %>%
  rename(booster_doses_delivered = overall) %>%
  filter(is.na(doses_per_case_averted) == FALSE) %>%
  filter(vax_scenario_short %in% c('booster to all prev primary','booster to high-risk prev primary')) %>%
  mutate(percentage_cases_averted = percentage_cases_averted*100,
         percentage_high_risk_case_averted = percentage_high_risk_case_averted * 100,
         percentage_older_cases_averted = percentage_older_cases_averted *100) %>%
  select(setting,vax_scenario_short,percentage_cases_averted,percentage_high_risk_case_averted,percentage_older_cases_averted)
# ggplot(vaccine_coverage_log[vaccine_coverage_log$dose<4,]) +
#   geom_col(aes(x=setting,y=coverage,fill=as.factor(age_group)),position="dodge") + 
#   facet_grid(dose~.)
ggplot(vaccine_coverage_log) +
  geom_col(aes(x=setting,y=coverage*100,fill=as.factor(age_group)),position="dodge") +
  labs(fill = "") +
  xlab("") +
  ylab("primary schedule coverage (%)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.box = "vertical") 

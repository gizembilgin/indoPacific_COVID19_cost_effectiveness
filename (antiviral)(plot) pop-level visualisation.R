


### eligible population by setting
settings_to_plot = c("PNG","FJI","TLS","IDN")
risk_group_name = "adults_with_comorbidities"
age_groups_num = c(0,4,9,17,29,44,59,69,110)
age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100')

#Step 1: load whole pop
num_age_groups = J = length(age_group_labels)          
age_group_order = data.frame(age_group = age_group_labels, age_group_num = seq(1:J))

load(file = "1_inputs/UN_world_population_prospects/UN_pop_est.Rdata")

pop_raw <- UN_pop_est %>% 
  filter(ISO3_code %in% settings_to_plot) %>%
  rename(country = ISO3_code,
         country_long = Location,
         population = PopTotal,
         population_female = PopFemale,
         age = AgeGrp) 
pop_orig = pop_raw %>%
  mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
  group_by(age_group,country) %>%
  summarise(pop = as.numeric(sum(population)))

#Step 2: separate into risk groups
if (risk_group_name %in% c('adults_with_comorbidities')) {
  workshop <-  read.csv('1_inputs/risk_group_distribution_Clarke_et_al_2020.csv')
  workshop = workshop %>%
    filter(age_group_charac != 'all ages') %>%
    filter(country %in% settings_to_plot) %>%
    rename(agegroup_RAW = age_group_charac,
           value = high_risk) # CHOICE between high_risk and increased_risk 
      
    underlying_age_grouping <- c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,110)
      
    pop_RAW =  pop_raw %>% 
        mutate(agegroup_RAW = cut(age,breaks = underlying_age_grouping, include.lowest = T, labels = unique(workshop$agegroup_RAW)),
               agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
        ungroup() %>%
        group_by(country,agegroup_MODEL) %>%
        mutate(model_group_percent = population/sum(population))
      
    toggle_upper_cut_off = 60  # CHOICE
    toggle_lower_cut_off = 18
      
    risk_dn = pop_RAW %>% 
        left_join(workshop, by = c("country", "agegroup_RAW")) %>% 
        mutate(value = case_when(
          #age_group_num >= toggle_upper_cut_off ~ 1,
          age_group_num < toggle_lower_cut_off ~ 0,
          TRUE ~ value
        )) %>%
        mutate(interim = model_group_percent * value) %>%
        group_by(country,agegroup_MODEL) %>%
        summarise(prop = sum(interim)) %>%
        rename(age_group = agegroup_MODEL)
      
} else if (risk_group_name %in% c('pregnant_women')){
    load(file = "1_inputs/prevalence_pregnancy.Rdata")
    risk_dn = prevalence_pregnancy %>%
      filter(country %in% settings_to_plot)
} else {
    stop('risk_group_name not a valid value')
}

  
pop_high_risk = pop_orig %>%
  left_join(risk_dn, by = c("age_group","country")) %>%
  mutate(risk_group = risk_group_name,
         pop = round(pop * prop)) %>%
  select(country,risk_group, age_group, pop)

pop_general_public   = pop_orig %>%
  left_join(risk_dn, by = c("age_group","country")) %>%
  mutate(risk_group = case_when(
    age_group %in% c("60 to 69","70 to 100") ~ 'adults aged 60+',
    TRUE ~ 'general_public'),
         pop = round(pop * (1 - prop))) %>%
  select(country,risk_group, age_group, pop)

pop_risk_group_dn = rbind(pop_general_public, pop_high_risk) 

pop_risk_group = pop_risk_group_dn %>%
  group_by(country,risk_group) %>%
  summarise(pop = sum(pop)) %>%
  group_by(country) %>%
  mutate(prop = pop/sum(pop)) %>%
  filter(risk_group != 'general_public')

ggplot(pop_risk_group) + 
  geom_col(aes(x=country,y=prop*100,fill=as.factor(risk_group))) +
  xlab("") +
  ylab("% of total population")


pop_orig %>% 
  group_by(country) %>%
  mutate(prop = pop/sum(pop)) %>%
  filter(age_group %in% c('60 to 69','70 to 100')) %>%
  summarise(prop_aged_over_60 = sum(prop))
#_____________________________________________________________




### impact by coverage
MASTER_RECORD_antiviral_model_simulations = data.frame()
MASTER_outcomes_without_antivirals = data.frame()
settings_to_plot = c("PNG_high_beta","PNG_low_beta")
for (i in c(1:length(settings_to_plot))){
  list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = paste("AntiviralRun_",settings_to_plot[i],"*",sep=""))
  list_poss_Rdata_details = double()
  for (j in 1:length(list_poss_Rdata)){
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste(rootpath,'x_results/',list_poss_Rdata[[j]],sep=''))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste(rootpath,"x_results/",latest_file,sep=''))
  
  #load latest antiviralSetUp_* (transmission model run for 1 year)
  list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = paste("antiviralSetUp_",settings_to_plot[i],"_",this_risk_group_name,"_*",sep=""))
  list_poss_Rdata_details = double()
  for (k in 1:length(list_poss_Rdata)){
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste(rootpath,'x_results/',list_poss_Rdata[[k]],sep=''))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste(rootpath,"x_results/",latest_file,sep=''))
  
  
  this_setting = RECORD_antiviral_model_simulations %>% mutate(setting_beta = settings_to_plot[i])
  MASTER_RECORD_antiviral_model_simulations = rbind(MASTER_RECORD_antiviral_model_simulations,this_setting)
  
  this_setting = RECORD_antiviral_setup$outcomes_without_antivirals %>% mutate(setting_beta = settings_to_plot[i])
  MASTER_outcomes_without_antivirals = rbind(MASTER_outcomes_without_antivirals,this_setting)
}

test_in_time_pt_est = 0.5

MASTER_outcomes_without_antivirals = MASTER_outcomes_without_antivirals %>%
  mutate(vax_scenario_short = case_when(
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to all prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to all first booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to high-risk prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to high-risk prev first booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule" ~ 
      "no booster"
  )) %>%
  filter(vax_scenario_short == "no booster" & outcome != "booster_doses_delivered") %>%
  mutate(prop = high_risk/overall,
         detected_by_RAT = prop * test_in_time_pt_est)

poss_effect = data.frame()
for (cov in seq(0.2,1,by=0.2)){
  this_effect = MASTER_outcomes_without_antivirals %>% mutate(effect = detected_by_RAT*cov, cov = cov)
  poss_effect = rbind(poss_effect,this_effect)
}


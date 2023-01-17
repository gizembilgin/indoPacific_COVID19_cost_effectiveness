#This program is our best guess as to the distribution of doses delivered in Timor-Leste by age/risk/vaccine type based on the Oxford tracker, and UNICEF Market Dashboard
#NB: no publicly available gov reports since May 2022
this_setting = setting = "TLS"
workshop = vaccination_history_DOSE_NUMBER %>%
  mutate(dose = as.numeric(dose)) %>%
  filter(doses_delivered_this_date>0) %>%
  select(date,dose,doses_delivered_this_date)


### SUPPLY
# Note: UNICEF’s COVID-19 Market Dashboard has supply equal to only half of number of doses delivered; same with WHO dashboard has doses administered ~ two times doses received
setting_vaccine_dn <- read.csv("1_inputs/vaccination/vaccine_setting_dn.csv",header=TRUE)%>%
  filter(setting == this_setting)
setting_vaccine_dn$last_update = as.Date(setting_vaccine_dn$last_update,format = '%d/%m/%Y')
setting_vaccine_dn <- setting_vaccine_dn %>% 
  select(last_update,age_floor,vaccine_type,dose,delivered)

#check ggplot
# ggplot(setting_vaccine_dn) + geom_point(aes(x=last_update,y=delivered, color = as.factor(dose),shape=as.factor(age_floor))) +
#   facet_grid(vaccine_type ~ .) 
#_______________________________________________________________________________



### LOAD Oxford COVID-19 Government Response Tracker
strategy <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_vaccines_full.csv")
strategy = strategy %>% filter(CountryCode %in% this_setting)
strategy$Date = as.Date(as.character(strategy$Date),"%Y%m%d")  
strategy = strategy %>% 
  select(-contains("V4")) %>% # we aren't interested in mandates
  select(-contains("V3")) %>% # note: all six settings are V3_Vaccine_Financial_Support_summary ==5 -> all categories fully funded by the government
  select(CountryCode,Date,contains("summary")) %>%
  rename(iso_code = CountryCode,
         date = Date)
colnames(strategy) = gsub(" ","_", colnames(strategy))
colnames(strategy) = gsub("-","_", colnames(strategy))
colnames(strategy) = gsub("[()+/]","", colnames(strategy))

# workshop_num = strategy %>%
#    pivot_longer(cols = c('V1_Vaccine_Prioritisation_summary','V2_Vaccine_Availability_summary'),
#                names_to = "measure",
#                values_to = "value")
# workshop_charac = strategy %>%
#    pivot_longer(cols = c('V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary','V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary'),
#                names_to = "measure",
#                values_to = "value")
# ggplot() + 
#   geom_point(data=workshop_num,aes(x=date,y=value))+ 
#   facet_grid(measure ~ .) 
# ggplot() + 
#   geom_point(data=workshop_charac,aes(x=date,y=value))+ 
#   facet_grid(measure ~ .) 
#Note: uniform/stable after last gov report (May 2022)
#_______________________________________________________________________________



### Doses by dose, vaccine type, age group (12-17 & 18+)
setting_vaccine_dn_INCREMENTAL = setting_vaccine_dn %>% 
  filter(is.na(vaccine_type) == FALSE & vaccine_type != "") %>%
  group_by(age_floor,vaccine_type,dose) %>%
  arrange(last_update) %>%
  mutate(delivered_here = delivered - lag(delivered,default = 0))%>%
  mutate(delivered_here = case_when(
    delivered_here <0 ~ 0,
    TRUE ~ delivered_here
  )) %>%
  group_by(last_update,dose) %>%
  mutate(prop = delivered_here/sum(delivered_here))

#correct
append = setting_vaccine_dn_INCREMENTAL %>% 
  filter(last_update == min(setting_vaccine_dn_INCREMENTAL$last_update) & dose == 1) %>%
  mutate(dose = 2)
setting_vaccine_dn_INCREMENTAL = rbind(setting_vaccine_dn_INCREMENTAL,append)  


#(1/3) up to May 2022 (available data)
workshop_pt1 = workshop %>%
  filter(date <= max(setting_vaccine_dn_INCREMENTAL$last_update)) %>%
  mutate(last_update = as.Date('1900-01-01')) #dummy value to be replaced

#find latest update
for (this_update in 1:length(unique(setting_vaccine_dn_INCREMENTAL$last_update))){
  this_update_date = unique(setting_vaccine_dn_INCREMENTAL$last_update)[this_update]
  workshop_pt1$last_update[workshop_pt1$date <= this_update_date & workshop_pt1$last_update == as.Date('1900-01-01')] = this_update_date
}

#join on dn of vaccine_type by dose and age (12-17,18+)
workshop_pt1 = workshop_pt1 %>%
  left_join(setting_vaccine_dn_INCREMENTAL, by = c("last_update","dose"))  
if(nrow(workshop_pt1[is.na(workshop_pt1$prop),])>0){stop('doses will be lost!')}
workshop_pt1 = workshop_pt1 %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date * prop) %>%
  select(date,vaccine_type,dose,age_floor,doses_delivered_this_date) 

#split to finer age groups and risk_group by strategy?
# strategy = strategy %>%
#   filter(date <= max(setting_vaccine_dn_INCREMENTAL$last_update)) %>%
#   mutate(age_floor_risk = case_when(
#     V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary == "16-19 yrs" ~ 18,
#     V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary == "35-39 yrs" ~ 35,
#     V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary == "80+ yrs" ~ 80,
#     V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary == "5-15 yrs" ~ 12,
#   ),
#   age_floor_general_public = case_when(
#     V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary == "16-19 yrs" ~ 18,
#     V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary == "35-39 yrs" ~ 35,
#     V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary == "80+ yrs" ~ 80,
#     V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary == "5-15 yrs" ~ 12,
#   ))
# workshop_charac = strategy %>%
#    pivot_longer(cols = c('age_floor_general_public','age_floor_risk'),
#                names_to = "measure",
#                values_to = "value")
# ggplot() +
#   geom_point(data=workshop_charac,aes(x=date,y=value))+
#   facet_grid(measure ~ .)
# distinct_period = strategy %>%
#   filter(is.na(age_floor_risk) == FALSE) %>%
#   group_by(age_floor_risk) %>%
#   summarise(date = min(date)) %>%
#   left_join(strategy[,c('date','age_floor_general_public')],by='date') %>%
#   select(date,age_floor_risk,age_floor_general_public)
#three distinct periods, BUT
#(1) model definition of 'at risk' (comorbidity) != OxTracker definition of 'at risk' (health care workers + comorbidity + age)
#(2) split between 12-17 & 18+ already captured
#(3) this would only be applicable to PRIMARY doses




#let's find distribution between 18-59 & 60+ at known date of 10/22
known_time_point = setting_vaccine_dn %>% 
  filter(is.na(vaccine_type) | vaccine_type == "")%>%
  select(dose,age_floor,delivered)
total_doses = workshop %>%
  filter(date<=as.Date('2022-10-15')) %>%
  group_by(dose) %>%
  summarise(total = sum(doses_delivered_this_date), .groups = "keep")
other_groups = known_time_point %>%
  group_by(dose) %>%
  summarise(other_ages = sum(delivered), .groups = "keep")
filler = total_doses %>%
  left_join(other_groups,by='dose') %>%
  mutate(delivered = total - other_ages,
         age_floor = 18) %>%
  select(dose,age_floor,delivered)
known_time_point = rbind(known_time_point,filler)
ratio = known_time_point %>%
  filter(age_floor>=18) %>%
  group_by(dose) %>%
  mutate(prop = delivered/sum(delivered)) %>%
  rename(new_age_floor = age_floor) %>%
  mutate(age_floor = 18)

# pop_setting  %>%
#   mutate(age_floor = case_when(
#     age_group %in% c('0 to 4', '5 to 9') ~ 0,
#     age_group %in% c('10 to 17') ~ 12,
#     age_group %in% c('60 to 69','70 to 100') ~ 60,
#     TRUE ~ 18
#   )) %>%
#   filter(age_floor >= 18) %>%
#   group_by(age_floor) %>%
#   summarise(pop = sum(pop)) %>%
#   ungroup() %>%
#   mutate(prop = pop/sum(pop))

#make to new age floor
workshop_pt1 = workshop_pt1 %>%
  left_join(ratio, by = c('age_floor', 'dose')) %>%
  mutate(
    age_floor = case_when(age_floor == 12 ~ 12,
                          TRUE ~ new_age_floor),
    doses_delivered_this_date = case_when(age_floor == 12 ~ doses_delivered_this_date,
                                          TRUE ~ doses_delivered_this_date * prop)
  ) %>%
  select(date,vaccine_type,dose,doses_delivered_this_date,age_floor)



#model age/risk groups
workshop_dn = pop_risk_group_dn %>%
  mutate(age_floor = case_when(
    age_group %in% c('0 to 4', '5 to 9') ~ 0,
    age_group %in% c('10 to 17') ~ 12,
    age_group %in% c('60 to 69','70 to 100') ~ 60,
    TRUE ~ 18
  )) %>%
  group_by(age_floor) %>%
  mutate(prop = pop/sum(pop)) %>%
  select(risk_group,age_group,age_floor,prop)
workshop_pt1 = workshop_pt1 %>%
  left_join(workshop_dn, by = 'age_floor') %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date * prop) %>%
  select(date,vaccine_type,dose,risk_group,age_group,doses_delivered_this_date)










#(2/3) projecting past available government report
workshop_pt2 = workshop %>%
  filter(date > max(setting_vaccine_dn_INCREMENTAL$last_update))

#(A/C) split into age groups
#see end point of workshop_pt1 & check not more delivered than people
end_pt1 = workshop_pt1 %>%
  mutate(age_floor = case_when(
    age_group %in% c('0 to 4', '5 to 9') ~ 0,
    age_group %in% c('10 to 17') ~ 12,
    age_group %in% c('60 to 69','70 to 100') ~ 60,
    TRUE ~ 18
  )) %>%
  group_by(age_floor,dose) %>%
  summarise(total = sum(doses_delivered_this_date), .groups = "keep")
# pop_setting  %>%
#   mutate(age_floor = case_when(
#     age_group %in% c('0 to 4', '5 to 9') ~ 0,
#     age_group %in% c('10 to 17') ~ 12,
#     age_group %in% c('60 to 69','70 to 100') ~ 60,
#     TRUE ~ 18
#   )) %>%
#   group_by(age_floor) %>%
#   summarise(pop = sum(pop))

#dn of next step
this_dn = known_time_point %>%
  left_join(end_pt1,by=c('age_floor','dose')) %>%
  mutate(delivered = delivered - total)  %>%
  group_by(dose) %>%
  mutate(prop = delivered/sum(delivered))%>%
  select(-total,-delivered)

workshop_pt2 = workshop_pt2 %>%
  left_join(this_dn, by = 'dose') %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date *prop) %>%
  select(-prop)



#(B/C) split into model age groups and risk groups
#model age/risk groups
workshop_dn = pop_risk_group_dn %>%
  mutate(age_floor = case_when(
    age_group %in% c('0 to 4', '5 to 9') ~ 0,
    age_group %in% c('10 to 17') ~ 12,
    age_group %in% c('60 to 69','70 to 100') ~ 60,
    TRUE ~ 18
  )) %>%
  group_by(age_floor) %>%
  mutate(prop = pop/sum(pop)) %>%
  select(risk_group,age_group,age_floor,prop)
workshop_pt2 = workshop_pt2 %>%
  left_join(workshop_dn, by = 'age_floor') %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date * prop) %>%
  select(date,dose,risk_group,age_group,doses_delivered_this_date)


#(C/C) split into vaccine types
this_dn = workshop_pt1 %>% 
  group_by(age_group,vaccine_type,dose) %>% 
  summarise(delivered = sum(doses_delivered_this_date), .groups = "keep") %>% 
  group_by(age_group,dose) %>% 
  mutate(prop = delivered/sum(delivered))

workshop_pt2 = workshop_pt2 %>%
  left_join(this_dn, by = c("dose", "age_group")) %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date * prop) %>%
  select(date,vaccine_type,dose,risk_group,age_group,doses_delivered_this_date)








#(3/3) Bring together!
workshop = rbind(workshop_pt1,workshop_pt2); rm(workshop_pt1,workshop_pt2)
if(round(sum(workshop$doses_delivered_this_date)) != round(sum(vaccination_history_DOSE_NUMBER$doses_delivered_this_date))){stop('pt1 and pt2 != whole')}
check = workshop %>%
  group_by(age_group,dose) %>%
  summarise(total = sum(doses_delivered_this_date), .groups = "keep") %>%
  left_join(pop_setting) %>%
  filter(pop<total)
if (nrow(check)>0){stop('more delivered than people exist!')}



### CREATE FINAL DATASET
vaccination_history_TRUE = workshop %>%
  filter(doses_delivered_this_date > 0) %>%
  mutate(schedule = case_when(
    dose > 2 ~ "booster",
    dose == 2 & vaccine_type == "Johnson & Johnson" ~ "booster",
    TRUE ~ "primary"
  )) %>%
  mutate(
    dose = as.numeric(dose),
    vaccine_mode = case_when(
      vaccine_type == 'Pfizer' ~ 'mRNA',
      vaccine_type == 'Moderna' ~ 'mRNA',
      vaccine_type == 'AstraZeneca' ~ 'viral_vector',
      vaccine_type == 'Sinopharm' ~ 'viral_inactivated',
      vaccine_type == 'Sinovac' ~ 'viral_inactivated',
      vaccine_type == 'Johnson & Johnson' ~ 'viral_vector'
    ),
    FROM_vaccine_type = vaccine_type,
    FROM_dose = dose - 1
  ) %>%
  left_join(pop_risk_group_dn, by = c("age_group", "risk_group")) %>%
  group_by(risk_group, age_group, vaccine_type, dose) %>%
  mutate(coverage_this_date = case_when(pop > 0 ~ cumsum(doses_delivered_this_date) /pop,
                                        TRUE ~ 0)) %>%
  select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date,age_group,risk_group,FROM_vaccine_type,FROM_dose)
#_______________________________________________________________________________



rm(vaccination_history_DOSE_NUMBER,workshop)


# This program is our best guess as to the distribution of doses delivered in Timor-Leste by age/risk/vaccine type based on TLS government reporting
#
# Notes on data:
#     - Both the UNICEF’s COVID-19 Market Dashboard and WHO Dashboard record doses received as half of doses delivered
#     - Government reports of vaccine delivery include information on delivery by vaccine type, dose, age group (primarily 12-17 vs 18+)
#             However, government reports (at least publicly available) ceased in May 2022
#     - Thankfully, the Oxford COVID-19 Government Response Tracker indicates uniform availability from the 10/27/2021 (see below)



this_setting = setting = "TLS"

workshop = vaccination_history_DOSE_NUMBER %>%
  mutate(dose = as.numeric(dose)) %>%
  filter(doses_delivered_this_date>0) %>%
  select(date,dose,doses_delivered_this_date)



### Inspect: Oxford COVID-19 Government Response Tracker
# strategy <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_vaccines_full.csv")
# strategy = strategy %>% filter(CountryCode %in% this_setting)
# strategy$Date = as.Date(as.character(strategy$Date),"%Y%m%d")  
# strategy = strategy %>% 
#   select(-contains("V4")) %>% # we aren't interested in mandates
#   select(-contains("V3")) %>% # note: all six settings are V3_Vaccine_Financial_Support_summary ==5 -> all categories fully funded by the government
#   select(CountryCode,Date,contains("summary")) %>%
#   rename(iso_code = CountryCode,
#          date = Date)
# colnames(strategy) = gsub(" ","_", colnames(strategy))
# colnames(strategy) = gsub("-","_", colnames(strategy))
# colnames(strategy) = gsub("[()+/]","", colnames(strategy))
#
# strategy = strategy %>%
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
#
# NOTES:three distinct periods, BUT
# (1) model definition of 'at risk' (co-morbidity) != OxTracker definition of 'at risk' (health care workers + co-morbidity + age)
# (2) split between 12-17 & 18+ already captured in government reporting
# (3) this strategy captures only strategy of PRIMARY doses
#_______________________________________________________________________________



### Reflect government reporting
#Step One: load setting_vaccine_dn
setting_vaccine_dn <- read.csv("01_inputs/vaccination/vaccine_setting_dn.csv",header=TRUE)%>%
  filter(setting == this_setting)
setting_vaccine_dn$last_update = as.Date(setting_vaccine_dn$last_update,format = '%d/%m/%Y')


#Step Two: load government reporting on doses split between dose number, vaccine type, and age group (12-17 & 18+)
setting_vaccine_dn_INCREMENTAL = setting_vaccine_dn %>% 
  filter(is.na(vaccine_type) == FALSE & vaccine_type != "") %>% #selecting government reports only, blanks are WHO dashboard snapshot of age dn
  group_by(age_floor,vaccine_type,dose) %>%
  arrange(last_update) %>%
  mutate(
    delivered_here = delivered - lag(delivered, default = 0),
    delivered_here = case_when(delivered_here < 0 ~ 0,
                               TRUE ~ delivered_here)
  ) %>% 
  group_by(last_update,dose) %>%
  mutate(prop = delivered_here/sum(delivered_here))
append = setting_vaccine_dn_INCREMENTAL %>% #add row for d=2 in first interval
  filter(last_update == min(setting_vaccine_dn_INCREMENTAL$last_update) & dose == 1) %>%
  mutate(dose = 2)
setting_vaccine_dn_INCREMENTAL = rbind(setting_vaccine_dn_INCREMENTAL,append); rm(append)  


#Step Three: apply government reporting on distribution
workshop_pt1 = workshop %>%
  filter(date <= max(setting_vaccine_dn_INCREMENTAL$last_update)) %>% # up to May 2022 (available data)
  mutate(last_update = as.Date('1900-01-01')) #dummy value to be replaced

for (this_update in 1:length(unique(setting_vaccine_dn_INCREMENTAL$last_update))){ #find latest update
  this_update_date = unique(setting_vaccine_dn_INCREMENTAL$last_update)[this_update]
  workshop_pt1$last_update[workshop_pt1$date <= this_update_date & workshop_pt1$last_update == as.Date('1900-01-01')] = this_update_date #replace only if dummy value
} 
rm(this_update_date)

workshop_pt1 = workshop_pt1 %>%
  left_join(setting_vaccine_dn_INCREMENTAL, by = c("last_update","dose"))  #join on dn of vaccine_type by dose and age (12-17,18+)
if(nrow(workshop_pt1[is.na(workshop_pt1$prop),])>0){stop('doses will be lost!')}
workshop_pt1 = workshop_pt1 %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date * prop) %>%
  select(date,vaccine_type,dose,age_floor,doses_delivered_this_date) 


#Step Four: split 18+ into 18-59 and 60+ based on dn at known date of 10/22
#NB: known_time_point includes total doses delivered for 12-17 and 60+, hence need to reconstruct 18-59
known_time_point = setting_vaccine_dn %>% 
  filter(is.na(vaccine_type) | vaccine_type == "") %>%
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

workshop_pt1 = workshop_pt1 %>%
  left_join(ratio, by = c('age_floor', 'dose')) %>%
  mutate( 
    age_floor = case_when(age_floor == 12 ~ 12,
                          TRUE ~ new_age_floor),# split into 18-59 and 60+
    doses_delivered_this_date = case_when(age_floor == 12 ~ doses_delivered_this_date,
                                          TRUE ~ doses_delivered_this_date * prop)
  ) %>%
  select(date,vaccine_type,dose,doses_delivered_this_date,age_floor)
rm(total_doses,other_groups,filler,ratio)


#Step Five: convert 12-17,18-59,60+ to model age/risk groups
this_dn = pop_risk_group_dn %>%
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
  left_join(this_dn, by = 'age_floor') %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date * prop) %>%
  select(date,vaccine_type,dose,risk_group,age_group,doses_delivered_this_date)


#Step Six: project past available government report
workshop_pt2 = workshop %>%
  filter(date > max(workshop_pt1$date))

#(A/C) rough age groups
end_pt1 = workshop_pt1 %>%
  mutate(age_floor = case_when(
    age_group %in% c('0 to 4', '5 to 9') ~ 0,
    age_group %in% c('10 to 17') ~ 12,
    age_group %in% c('60 to 69','70 to 100') ~ 60,
    TRUE ~ 18
  )) %>%
  group_by(age_floor,dose) %>%
  summarise(total = sum(doses_delivered_this_date), .groups = "keep")

this_dn = known_time_point %>%
  left_join(end_pt1,by=c('age_floor','dose')) %>%
  mutate(delivered = delivered - total)  %>%
  group_by(dose) %>%
  mutate(prop = delivered/sum(delivered))%>%
  select(-total,-delivered)
rm(end_pt1,known_time_point)

workshop_pt2 = workshop_pt2 %>%
  left_join(this_dn, by = 'dose') %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date *prop) %>%
  select(-prop)

#(B/C) model age groups and risk groups
this_dn = pop_risk_group_dn %>%
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
  left_join(this_dn, by = 'age_floor') %>%
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


#Step Seven: bring together!
workshop = rbind(workshop_pt1,workshop_pt2); rm(workshop_pt1,workshop_pt2)
if(round(sum(workshop$doses_delivered_this_date)) != round(sum(vaccination_history_DOSE_NUMBER$doses_delivered_this_date))){stop('pt1 and pt2 != whole')}
check = workshop %>%
  group_by(age_group,dose) %>%
  summarise(total = sum(doses_delivered_this_date), .groups = "keep") %>%
  left_join(pop_setting, by = "age_group") %>%
  filter(pop<total)
if (nrow(check)>0){stop('more delivered than people exist!')}

#Step Eight: Split booster doses across vaccine types
workshop_booster_pool =  workshop %>% 
  filter(dose == 2) %>%
  group_by(age_group,vaccine_type) %>%
  summarise(total = sum(doses_delivered_this_date), .groups = "keep") %>%
  group_by(age_group) %>%
  mutate(prop = total/sum(total)) %>%
  select(-total) %>%
  rename(FROM_vaccine_type = vaccine_type)
  
workshop_booster = workshop %>% 
  filter(dose == 3) %>%
  left_join(workshop_booster_pool,by=c("age_group")) %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date*prop)

workshop = rbind(workshop[workshop$dose != 3,],workshop_booster); rm(workshop_booster,workshop_booster_pool)
#_______________________________________________________________________________



### CREATE FINAL DATASET
vaccination_history_TRUE = workshop %>%
  filter(doses_delivered_this_date > 0) %>%
  mutate(schedule = case_when(
    dose > 2 ~ "booster",
    dose == 2 & vaccine_type == "Johnson & Johnson" ~ "booster",
    TRUE ~ "primary"
  ),
  dose = as.numeric(dose),
  vaccine_mode = case_when(
      vaccine_type == 'Pfizer' ~ 'mRNA',
      vaccine_type == 'Moderna' ~ 'mRNA',
      vaccine_type == 'AstraZeneca' ~ 'viral_vector',
      vaccine_type == 'Sinopharm' ~ 'viral_inactivated',
      vaccine_type == 'Sinovac' ~ 'viral_inactivated',
      vaccine_type == 'Johnson & Johnson' ~ 'viral_vector'
    ),
    FROM_dose = dose - 1
  ) %>%
  left_join(pop_risk_group_dn, by = c("age_group", "risk_group")) %>%
  group_by(risk_group, age_group, vaccine_type, dose) %>%
  mutate(coverage_this_date = case_when(pop > 0 ~ cumsum(doses_delivered_this_date) /pop,
                                        TRUE ~ 0)) %>%
  select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date,age_group,risk_group,FROM_vaccine_type,FROM_dose)
vaccination_history_TRUE$FROM_vaccine_type[is.na(vaccination_history_TRUE$FROM_vaccine_type)] <- vaccination_history_TRUE$vaccine_type[is.na(vaccination_history_TRUE$FROM_vaccine_type)]
#_______________________________________________________________________________

rm(vaccination_history_DOSE_NUMBER,workshop,setting_vaccine_dn,setting_vaccine_dn_INCREMENTAL,check,this_dn)


###CHECK
#1) CHECKED: plot of coverage by age_group by dose
# to_plot = vaccination_history_TRUE %>%
#   group_by(date,age_group,dose) %>%
#   summarise(doses_delivered_this_date = sum(doses_delivered_this_date))%>%
#   left_join(pop_setting, by = c("age_group")) %>%
#   group_by(age_group,dose) %>%
#   mutate(coverage_this_date = case_when(pop > 0 ~ cumsum(doses_delivered_this_date) /pop,
#                                         TRUE ~ 0))
# ggplot(to_plot) + geom_point(aes(x=date,y=coverage_this_date,color=as.factor(age_group)))+ 
#   plot_standard + 
#   facet_grid(dose ~ .)
#
#2) CHECKED: plot of doses delivered by vaccine type over time
# to_plot = vaccination_history_TRUE %>%
#   group_by(date,vaccine_type,dose) %>%
#   summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>%
#   group_by(vaccine_type,dose) %>%
#   mutate(cum_doses = cumsum(doses_delivered_this_date))
# ggplot(to_plot) + geom_line(aes(x=date,y=doses_delivered_this_date,color=as.factor(vaccine_type)))+ 
#   plot_standard + 
#   facet_grid(dose ~ .)
# ggplot(to_plot) + geom_point(aes(x=date,y=cum_doses,color=as.factor(vaccine_type)))+ 
#   plot_standard + 
#   facet_grid(dose ~ .)
#
#3) CHECKED: our estimate compared to government reporting of cumulative doses delivered
# check = vaccination_history_TRUE%>%
#   mutate(age_floor = case_when(
#     age_group %in% c('0 to 4', '5 to 9') ~ 0,
#     age_group %in% c('10 to 17') ~ 12,
#     age_group %in% c('60 to 69','70 to 100') ~ 60,
#     TRUE ~ 18
#   )) %>%
#   group_by(date, age_floor, vaccine_type, dose) %>%
#   summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>%
#   group_by(age_floor, vaccine_type, dose) %>%
#   mutate(delivered_model = cumsum(doses_delivered_this_date)) %>%
#   rename(last_update = date) %>%
#   left_join(setting_vaccine_dn_INCREMENTAL, by = c('age_floor','vaccine_type','dose','last_update')) %>%
#   filter(is.na(delivered) == FALSE) %>%
#   select(last_update,age_floor,vaccine_type,dose,delivered_model,delivered) %>%
#   mutate(diff = (delivered_model-delivered)/delivered)
# ggplot(check) + geom_point(aes(x=last_update,y=diff,color=as.factor(vaccine_type)))+ 
#      facet_grid(dose ~ .)
#
#4) CHECKED: all doses in vaccination_history_DOSE_NUMBER in vaccination_history_TRUE
# sum(vaccination_history_TRUE$doses_delivered_this_date)
# sum(vaccination_history_DOSE_NUMBER$doses_delivered_this_date)
#
#5) CORRECTED: dose 2 not delivered before dose 1, and dose 3 not before dose 2
# d=2
# workshop_prev_dose = vaccination_history_TRUE %>%
#   filter(dose == d - 1) %>% ungroup() %>%
#   select(date,vaccine_type,age_group,risk_group,doses_delivered_this_date)%>%
#   group_by(vaccine_type,age_group,risk_group) %>%
#   mutate(prev_avaliable = cumsum(doses_delivered_this_date))
# workshop_next_dose = vaccination_history_TRUE %>%
#   filter(dose == d) %>% ungroup() %>%
#   select(date,vaccine_type,age_group,risk_group,doses_delivered_this_date)%>%
#   group_by(vaccine_type,age_group,risk_group) %>%
#   mutate(next_delivered = cumsum(doses_delivered_this_date),
#          date = date-vaxCovDelay$delay[vaxCovDelay$dose == (d-1)])  %>%
#   select(-doses_delivered_this_date)
# check = workshop_prev_dose %>%
#   left_join(workshop_next_dose, by = c("date", "vaccine_type", "age_group", "risk_group")) %>%
#   filter(prev_avaliable < next_delivered)
# 
# #dose 2 for Pfizer provided to AstraZeneca dose 1 recipients (18+)
# vaccination_history_TRUE %>%
#   filter(vaccine_type %in% c("Pfizer","AstraZeneca")) %>%
#     mutate(age_floor = case_when(
#       age_group %in% c('0 to 4', '5 to 9') ~ 0,
#       age_group %in% c('10 to 17') ~ 12,
#       age_group %in% c('60 to 69','70 to 100') ~ 60,
#       TRUE ~ 18
#     ))%>%
#   group_by(vaccine_type,age_floor,dose) %>%
#   summarise(sum = sum(doses_delivered_this_date))
#
#identify excess Pfizer dose 2 where we expect heterogeneous dose 1
excess_Pfizer = vaccination_history_TRUE %>%
  filter(vaccine_type == "Pfizer" &
           dose < 3 & age_group != '10 to 17') %>%
  group_by(vaccine_type, age_group, dose) %>%
  summarise(total_delivered = sum(doses_delivered_this_date),
            .groups = "keep") %>%
  pivot_wider(names_from = dose,
              names_prefix = "dose_",
              values_from = total_delivered) %>% 
  mutate(from_AZ = (dose_2-dose_1)*(1+(1-(dose_2-dose_1)/dose_1)),
         Pfizer_complete = dose_2-from_AZ,
         prop_from_AZ = from_AZ/dose_2) %>%
  select(age_group,prop_from_AZ)

#split Pfizer d=2 across hetero and homo combinations
workshop = vaccination_history_TRUE %>%
  filter(vaccine_type == "Pfizer" & dose == 2 & age_group != "10 to 17" & doses_delivered_this_date>0) %>%
  left_join(excess_Pfizer, by = c("vaccine_type", "age_group"))
from_Pfizer = workshop %>% mutate(doses_delivered_this_date = doses_delivered_this_date * (1-prop_from_AZ))
from_AZ =  workshop %>% mutate(doses_delivered_this_date = doses_delivered_this_date * prop_from_AZ,
                               FROM_vaccine_type = "AstraZeneca")

#reform vaccination_history_TRUE
vaccination_history_TRUE = vaccination_history_TRUE %>%
  filter(!(vaccine_type == "Pfizer" & dose == 2 & age_group != "10 to 17" & doses_delivered_this_date>0))
vaccination_history_TRUE = rbind(vaccination_history_TRUE,from_Pfizer,from_AZ); rm(from_Pfizer,from_AZ,excess_Pfizer,workshop)
vaccination_history_TRUE = vaccination_history_TRUE %>%
  left_join(pop_risk_group_dn, by = c("age_group", "risk_group")) %>%
  group_by(risk_group, age_group, vaccine_type, dose) %>%
  mutate(coverage_this_date = case_when(pop > 0 ~ cumsum(doses_delivered_this_date) /pop,
                                        TRUE ~ 0)) %>%
  select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date,age_group,risk_group,FROM_vaccine_type,FROM_dose)

#vaccination_history_TRUE %>% group_by(dose,vaccine_type,FROM_vaccine_type) %>% summarise(sum=sum(doses_delivered_this_date))

#This program is our best guess as to the distribution of doses delivered in FIji by age/risk/vaccine type based on governement annoucements, the Oxford tracker, and UNICEF Market Dashboard
this_setting = "FJI"


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
#_______________________________________________________________________________


### LOAD UNICEF’s COVID-19 Market Dashboard
supply <- read.csv("1_inputs/vaccine_market_UNICEF.csv",header=TRUE) 
supply$delivery_month = as.Date(supply$delivery_month, "%d/%m/%Y")
supply$vaccine_type[supply$vaccine_type == "Covishield"] = "AstraZeneca" #The Oxford–AstraZeneca COVID‑19 vaccine, sold under the brand names Covishield and Vaxzevria
supply = supply %>%
  group_by(setting,delivery_month,vaccine_type) %>%
  summarise(doses = sum(doses), .groups = "keep") %>%
  filter(setting == this_setting)
#_______________________________________________________________________________


### ASSIGN AGE & RISK PRIORITISATION OVER TIME
workshop = vaccination_history_DOSE_NUMBER

#split into risk group and general public
workshop_risk = workshop %>% mutate(risk_group = risk_group_name)
workshop_general = workshop %>% mutate(risk_group = "general_public")
workshop = rbind(workshop_risk,workshop_general); rm(workshop_risk,workshop_general)

#FJI has four distinct age/risk allocation periods!
#1st: risk group prioritization
#2nd: 18+ equal prioritization
#3rd: 15+ equal prioritization
#4th: 5+ equal prioritization
time_points = 
  data.frame(start_date = c(min(workshop$date),
                      max(strategy$date[strategy$V1_Vaccine_Prioritisation_summary == 1],na.rm=TRUE) + 1,
                      max(strategy$date[strategy$V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary == "16-19 yrs"],na.rm=TRUE) + 1,
                      min(supply$delivery_month[supply$vaccine_type == "Pfizer"])
                      ),
             end_date = c(max(strategy$date[strategy$V1_Vaccine_Prioritisation_summary == 1],na.rm=TRUE) ,
                          max(strategy$date[strategy$V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary == "16-19 yrs"],na.rm=TRUE),
                          min(supply$delivery_month[supply$vaccine_type == "Pfizer"]) - 1,
                          max(workshop$date)),
             V1_Vaccine_Prioritisation_summary = c(1,2,2,2),
             age_floor = c(18,18,15,5))

#step through each of these allocation periods
new_workshop = data.frame()
for(pt in 1:nrow(time_points)){ 
  slice = workshop %>%
    filter(date>= time_points$start_date[pt] & date <= time_points$end_date[pt])
  save_pt = sum(slice$doses_delivered_this_date)/2 #/2 since just doubled with split into risk group and general
  
  if (time_points$V1_Vaccine_Prioritisation_summary[pt] == 1){
    #initial strategy where adults over 60 and healthcare workers were a priority
    #can use pop_risk_group_dn, since our age groups cut off at 18 and 60 AND first cab off the rank
    high_risk_priority = 0.8
    
    #calculate age- and risk- dn of eligible individuals
    prioritisation = pop_risk_group_dn %>%
      filter((risk_group == "general_public" & age_group %in% c("18 to 29",  "30 to 44",  "45 to 59",  "60 to 69",  "70 to 100")) | 
               (risk_group == risk_group_name & age_group %in% c("60 to 69",  "70 to 100")) ) %>%
      mutate(prop_risk = case_when(
        risk_group == risk_group_name ~ high_risk_priority,
        TRUE ~ (1-high_risk_priority)
      )) %>%
      group_by(risk_group) %>%
      mutate(prop_age = pop/sum(pop),
             prop = prop_age * prop_risk) %>%
      select(risk_group,age_group,prop)
    
    slice = slice %>% 
      left_join(prioritisation, by = c('risk_group')) %>%
      mutate(doses_delivered_this_date = doses_delivered_this_date * prop)
    
  } else{ # uniform priority with eligible age groups
    this_floor = time_points$age_floor[pt] #youngest age group from which uniform priority
    
    to_date = new_workshop %>%
      group_by(risk_group,age_group,dose) %>%
      summarise(vaxed = sum(doses_delivered_this_date), .groups = 'keep')
    
    #calculate age- and risk- dn of eligible individuals
    prioritisation = pop_setting_orig %>%
      filter(age >= this_floor) %>%
      mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
      group_by(age_group) %>%
      summarise(pop = as.numeric(sum(population))) %>%
      left_join(risk_dn, by = "age_group")
    this_risk = prioritisation %>%
      mutate(risk_group = risk_group_name,
             pop = prop*pop)
    this_gen = prioritisation %>%
      mutate(risk_group = "general_public",
             pop = (1-prop)*pop)
    prioritisation = rbind(this_risk,this_gen) %>% select(-prop)
    
    prioritisation = crossing(prioritisation, dose = unique(workshop$dose)) %>%
      filter(!(dose %in% c(3,4) & age_group %in% c('5 to 9','10 to 17'))) %>% #booster dose to 18+ ONLY
      left_join(to_date, by = c('age_group','risk_group','dose'))%>%
      mutate(pop = case_when(
        is.na(vaxed) ~ pop,
        TRUE ~ pop - vaxed)) %>%
      ungroup() %>% group_by(dose) %>%
      mutate(prop = pop/sum(pop)) %>%
      select(-pop,-vaxed)
    
    if (nrow(prioritisation[round(prioritisation$prop,digits=4)<0,])>0){stop('negative prop!')}
    
    slice = slice %>% 
      left_join(prioritisation, by = c('risk_group','dose')) %>%
      mutate(doses_delivered_this_date = doses_delivered_this_date * prop) %>%
      select(-prop)

  }
  if (round(save_pt) != round(sum(slice$doses_delivered_this_date))){warning('something has shifted!')}

  #check if more doses delivered than existing population (NB: Fiji's pop is expected < actual pop due to migration, population growth etc.)
  #even the government reports ~104.1% for adults 18+: https://www.health.gov.fj/03-11-2022/#:~:text=The%20Ministry%20of%20Health%20and,a%20safe%20and%20timely%20fashion.
  check = rbind(new_workshop,slice) %>%
    ungroup() %>%
    group_by(risk_group,age_group,dose) %>%
    summarise(vaxed = sum(doses_delivered_this_date), .groups = 'keep') %>%
    left_join(pop_risk_group_dn, by = c("risk_group", "age_group")) %>%
    filter(pop<vaxed) %>%
    mutate(difference = vaxed - pop) %>%
    select(-vaxed,-pop)
  
  #correct if more doses delivered than exist
  if (nrow(check)>1){
    slice_ammend = slice %>%
      ungroup() %>%
      group_by(risk_group,age_group,dose) %>%
      summarise(vaxed = sum(doses_delivered_this_date), .groups = 'keep') %>%
      left_join(check , by = c("risk_group", "age_group", "dose")) %>%
      mutate(prop = 1- difference/vaxed)
    slice_ammend = na.omit(slice_ammend)
    
    slice = slice %>% left_join(slice_ammend) %>% 
      mutate(doses_delivered_this_date = 
               case_when(
                 is.na(prop) ~ doses_delivered_this_date,
                 TRUE ~ doses_delivered_this_date * prop)) %>%
      select(-vaxed,-difference)
  }
  new_workshop = rbind(new_workshop,slice)
}
workshop = new_workshop %>% select(-prop)
workshop[is.na(workshop)] <- 0
#_______________________________________________________________________________


### VACCINE TYPE BY NATIONAL RULES
#Teenagers split between: 10-14 Pfizer, and 15-17 Moderna
moderna_split = pop_setting_orig %>%
  filter(age < 18 & age >= 10) %>%
  mutate(age_group = cut(age,breaks = c(9,14,17), include.lowest = T,labels = c("10 to 14","15 to 17"))) %>%
  group_by(age_group) %>%
  summarise(pop = as.numeric(sum(population))) %>%
  ungroup() %>%
  mutate(prop_age = pop/sum(pop))

workshop_children = workshop %>% filter(age_group == '10 to 17') 
workshop_children_pfizer = workshop_children %>% 
  mutate(doses_delivered_this_date = doses_delivered_this_date * moderna_split$prop_age[moderna_split$age_group == '10 to 14'],
         vaccine_type = "Pfizer")  
workshop_children_moderna = workshop_children %>% 
  mutate(doses_delivered_this_date = doses_delivered_this_date * moderna_split$prop_age[moderna_split$age_group == '15 to 17'],
         vaccine_type = "Moderna")  
workshop_children = rbind(workshop_children_pfizer,workshop_children_moderna); rm(workshop_children_pfizer,workshop_children_moderna)

#Children are Pfizer, primary doses to adults AZ, and booster doses Moderna
workshop = workshop %>%
  filter(age_group != '10 to 17')%>%
  mutate(vaccine_type = case_when(
         dose %in% c(3,4) ~ "Moderna",
         age_group %in% c('5 to 9') ~ "Pfizer",
         TRUE ~ "AstraZeneca"))
workshop = rbind(workshop,workshop_children); rm(workshop_children)
#_______________________________________________________________________________



### CREATE FINAL DATASET
vaccination_history_TRUE = workshop %>%
  filter(doses_delivered_this_date > 0) %>%
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
    FROM_vaccine_type = case_when(
      dose == 4 ~ "Moderna",
      dose == 3 ~ "AstraZeneca",
      TRUE ~ vaccine_type),
    FROM_dose = dose - 1
  ) %>%
  left_join(pop_risk_group_dn, by = c("age_group", "risk_group")) %>%
  group_by(risk_group, age_group, vaccine_type, dose) %>%
  mutate(coverage_this_date = case_when(pop > 0 ~ cumsum(doses_delivered_this_date) /pop,
                                        TRUE ~ 0)) %>%
  select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date,age_group,risk_group,FROM_vaccine_type,FROM_dose)
#_______________________________________________________________________________



# ###CHECK
# d=2
# workshop_prev_dose = vaccination_history_TRUE %>% 
#   filter(dose == d - 1) %>% ungroup() %>%
#   select(date,vaccine_type,age_group,risk_group,doses_delivered_this_date)%>% 
#   mutate(prev_avaliable = cumsum(doses_delivered_this_date)) 
# 
# workshop_next_dose = vaccination_history_TRUE %>% 
#   filter(dose == d) %>% ungroup() %>%
#   select(date,vaccine_type,age_group,risk_group,doses_delivered_this_date)%>% 
#   mutate(next_delivered = cumsum(doses_delivered_this_date)) 
# 
# timing_check = workshop_next_dose %>%
#   mutate(date = date-vaxCovDelay$delay[vaxCovDelay$dose == (d-1)])  %>%
#   select(-doses_delivered_this_date)
# timing_check = workshop_prev_dose %>%
#   left_join(timing_check, by = c("date", "vaccine_type", "age_group", "risk_group")) %>%
#   filter(prev_avaliable < next_delivered)
# 
# check_conserved = round(sum(vaccination_history_TRUE$doses_delivered_this_date)) #save total number of doses as comparison
# while(nrow(timing_check)>0){
#   ammend_date = timing_check$date[1]
#   ammend_vax = timing_check$vaccine_type[1]
#   ammend_age_group = timing_check$age_group[1]
#   ammend_risk_group = timing_check$risk_group[1]
#   ammend_amount = timing_check$next_delivered[1] - timing_check$prev_avaliable[1] 
#   
#   if (ammend_date - 1 < min(vaccination_history_TRUE$date)){#if date to fix before vaccine history starts
#     row = data.frame(date = ammend_date - 1,
#                      vaccine_type = ammend_vax,
#                      vaccine_mode = NA,
#                      dose = d - 1,
#                      coverage_this_date = NA,
#                      doses_delivered_this_date = ammend_amount,
#                      age_group = ammend_age_group,
#                      risk_group - ammend_risk_group,
#                      FROM_vaccine_type = ammend_vax,
#                      FROM_dose = d - 2)
#     vaccination_history_TRUE = rbind(row,vaccination_history_TRUE)
#   } else{
#     vaccination_history_TRUE$doses_delivered_this_date[vaccination_history_TRUE$dose == (d-1) &
#                                                          vaccination_history_TRUE$date == ammend_date - 1 &
#                                                          vaccination_history_TRUE$vaccine_type == ammend_vax &
#                                                          vaccination_history_TRUE$age_group == ammend_age_group & 
#                                                          vaccination_history_TRUE$risk_group == ammend_risk_group] =  
#       vaccination_history_TRUE$doses_delivered_this_date[vaccination_history_TRUE$dose == (d-1) & vaccination_history_TRUE$date == ammend_date - 1 &
#                                                            vaccination_history_TRUE$vaccine_type == ammend_vax &
#                                                            vaccination_history_TRUE$age_group == ammend_age_group & 
#                                                            vaccination_history_TRUE$risk_group == ammend_risk_group] + ammend_amount
#   }
#   
#   trial = 1
#   while(trial > 0){#remove this amount from first sensible interval
#     window = (trial-1) * 7 + 1
#     if (sum(vaccination_history_TRUE$doses_delivered_this_date[vaccination_history_TRUE$dose == (d-1) &
#                                                                vaccination_history_TRUE$vaccine_type == ammend_vax &
#                                                                vaccination_history_TRUE$age_group == ammend_age_group & 
#                                                                vaccination_history_TRUE$risk_group == ammend_risk_group & 
#                                                                vaccination_history_TRUE$date >= ammend_date &
#                                                                vaccination_history_TRUE$date < ammend_date + window])>ammend_amount){
#       vaccination_history_TRUE$doses_delivered_this_date[vaccination_history_TRUE$dose == (d-1) &
#                                                            vaccination_history_TRUE$vaccine_type == ammend_vax &
#                                                            vaccination_history_TRUE$age_group == ammend_age_group & 
#                                                            vaccination_history_TRUE$risk_group == ammend_risk_group &
#                                                            vaccination_history_TRUE$date >= ammend_date  &
#                                                            vaccination_history_TRUE$date < ammend_date + window] =  
#         (sum(vaccination_history_TRUE$doses_delivered_this_date[vaccination_history_TRUE$dose == (d-1) &
#                                                                   vaccination_history_TRUE$vaccine_type == ammend_vax &
#                                                                   vaccination_history_TRUE$age_group == ammend_age_group & 
#                                                                   vaccination_history_TRUE$risk_group == ammend_risk_group & 
#                                                                   vaccination_history_TRUE$date >= ammend_date  &
#                                                                   vaccination_history_TRUE$date < ammend_date + window]) - ammend_amount)/window
#       
#       trial = 0 #end loop if delivered
#     } else{
#       trial = trial + 1
#     }
#   }
#   rm(trial,window,ammend_date,ammend_amount)
#   if (round(check_conserved) != round(sum(vaccination_history_TRUE$doses_delivered_this_date))){stop('doses lost!')}
#   
#   workshop_prev_dose = vaccination_history_TRUE %>% 
#     filter(dose == d - 1) %>% ungroup() %>%
#     select(date,vaccine_type,age_group,risk_group,doses_delivered_this_date)%>% 
#     mutate(prev_avaliable = cumsum(doses_delivered_this_date)) 
#   
#   workshop_next_dose = vaccination_history_TRUE %>% 
#     filter(dose == d) %>% ungroup() %>%
#     select(date,vaccine_type,age_group,risk_group,doses_delivered_this_date)%>% 
#     mutate(next_delivered = cumsum(doses_delivered_this_date)) 
#   
#   timing_check = workshop_next_dose %>%
#     mutate(date = date-vaxCovDelay$delay[vaxCovDelay$dose == (d-1)])  %>%
#     select(-doses_delivered_this_date)
#   timing_check = workshop_prev_dose %>%
#     left_join(timing_check, by = c("date", "vaccine_type", "age_group", "risk_group")) %>%
#     filter(prev_avaliable < next_delivered)
# } #ends when timing_check has no row



rm(supply,strategy)
#This program is our best guess as to the distribution of doses delivered in Papua New Guinea by age/risk/vaccine type based on governement annoucements, the Oxford tracker, and UNICEF Market Dashboard
this_setting = setting = "PNG"


### LOAD UNICEF’s COVID-19 Market Dashboard
supply <- read.csv("1_inputs/vaccine_market_UNICEF.csv",header=TRUE) 
supply$delivery_month = as.Date(supply$delivery_month, "%d/%m/%Y")
supply$vaccine_type[supply$vaccine_type == "Covishield"] = "AstraZeneca" #The Oxford–AstraZeneca COVID‑19 vaccine, sold under the brand names Covishield and Vaxzevria
supply$vaccine_type[supply$vaccine_type == "Janssen"] = "Johnson & Johnson"
supply = supply %>%
  group_by(setting,delivery_month,vaccine_type) %>%
  summarise(doses = sum(doses), .groups = "keep") %>%
  filter(setting == this_setting)
#_______________________________________________________________________________



### LOAD GOVERNMENT REPORT OF DELIVERED VACCINES
#total doses delivered by vaccine type
setting_vaccine <- read.csv("1_inputs/vaccination/vaccine_setting_history.csv",header=TRUE)%>%
  filter(setting == this_setting)
setting_vaccine$last_update = as.Date(setting_vaccine$last_update,format = '%d/%m/%Y')
setting_vaccine <- setting_vaccine %>% filter( last_update == max(setting_vaccine$last_update)) %>%
  select(vaccine_type,doses)

#total doses delivered by age group floor
setting_vaccine_dn <- read.csv("1_inputs/vaccination/vaccine_setting_dn.csv",header=TRUE)%>%
  filter(setting == this_setting)
setting_vaccine_dn$last_update = as.Date(setting_vaccine_dn$last_update,format = '%d/%m/%Y')
setting_vaccine_dn <- setting_vaccine_dn %>% filter( last_update == max(setting_vaccine_dn$last_update)) %>%
  select(age_floor,dose,delivered)
#_______________________________________________________________________________



### SPLIT BETWEEN VACCINE TYPES
workshop = vaccination_history_DOSE_NUMBER 

#dates new vaccine types became available in PNG
vax_type_introduction = supply %>%
  group_by(vaccine_type) %>%
  summarise(date = min(delivery_month)+round(365/12/2)) %>% #added component to make it the middle of the delivery month
  arrange(date)
if (min(vax_type_introduction$date)>min(vaccination_history_DOSE_NUMBER$date)){stop('distribution before supply')}

workshop_type = data.frame()
for (slice in 1:length(vax_type_introduction$date)){
  
  #select slice of doses to allocate
  workshop_slice = workshop %>%
    filter(date >= vax_type_introduction$date[slice] &
             doses_delivered_this_date>0)
  if (slice < length(vax_type_introduction$date)){
    workshop_slice = workshop_slice %>%
      filter(date < vax_type_introduction$date[slice+1])
  }
  
  #create dn between vax_types
  vax_types_here = unique(supply$vaccine_type[supply$delivery_month<=vax_type_introduction$date[slice]])
  vax_types_dn_here = setting_vaccine %>% filter(vaccine_type %in% vax_types_here)
  if (slice>1){
    total_delivered_here = workshop_type %>% group_by(vaccine_type) %>% summarise(delivered = sum(doses_delivered_this_date))
    vax_types_dn_here = vax_types_dn_here %>% 
      left_join(total_delivered_here,by='vaccine_type') 
    vax_types_dn_here$delivered[is.na(vax_types_dn_here$delivered)] <- 0
    vax_types_dn_here = vax_types_dn_here %>% 
      mutate(doses = doses - delivered) %>%
      select(-delivered)
  }
  
  #adjustment for J&J since a single-dose vaccine
  if ("Johnson & Johnson" %in% vax_types_here){
    vax_types_dn_here = vax_types_dn_here %>% 
      mutate(prop = case_when(
        vaccine_type == "Johnson & Johnson" ~ 2*doses/(sum(doses)+doses),
        vaccine_type != "Johnson & Johnson" ~ doses/(sum(doses)+vax_types_dn_here$doses[vax_types_dn_here$vaccine_type == "Johnson & Johnson"])))
  } else{
    vax_types_dn_here = vax_types_dn_here %>% 
      mutate(prop = doses/(sum(doses)))
  }
  if ("Sinopharm" %in% vax_types_dn_here$vaccine_type[vax_types_dn_here$doses>0]){ #override for PNG to deliver Sinopharm when first delivered
    vax_types_dn_here$prop[vax_types_dn_here$vaccine_type == "Sinopharm"] = vax_types_dn_here$doses[vax_types_dn_here$vaccine_type == "Sinopharm"]/sum(workshop_slice$doses_delivered_this_date)
    vax_types_dn_here$prop[vax_types_dn_here$vaccine_type != "Sinopharm"] = vax_types_dn_here$prop[vax_types_dn_here$vaccine_type != "Sinopharm"] - (sum(vax_types_dn_here$prop) - 1)
    if (round(sum(vax_types_dn_here$prop),digits = 1)!=1){stop("override for Sinopharm ill configured")}
  }
  
  #booster doses in PNG are only J&J or AZ
  if (3 %in% unique(workshop_slice$dose[workshop_slice$doses_delivered_this_date>0])){
    vax_types_booster_dn_here = vax_types_dn_here %>% 
      filter(vaccine_type %in% c("AstraZeneca","Johnson & Johnson")) %>% 
        mutate(prop = doses/(sum(doses)))
    for (filler_vax in vax_types_dn_here$vaccine_type[!vax_types_dn_here$vaccine_type %in% vax_types_booster_dn_here$vaccine_type]){
      row = data.frame(vaccine_type = filler_vax,doses=0,prop=0)
      vax_types_booster_dn_here = rbind(vax_types_booster_dn_here,row)
    }
  } else{
    vax_types_booster_dn_here = vax_types_dn_here %>% mutate(prop = 0)
  }
  
  for (this_vax_type in vax_types_here){
    this_vax_slice = workshop_slice %>%
      mutate(vaccine_type = this_vax_type,
             doses_delivered_this_date = case_when(
               dose %in% c(1,2) ~ doses_delivered_this_date * vax_types_dn_here$prop[vax_types_dn_here$vaccine_type == this_vax_type],
               TRUE ~ doses_delivered_this_date * vax_types_booster_dn_here$prop[vax_types_booster_dn_here$vaccine_type == this_vax_type]))
    workshop_type = rbind(workshop_type,this_vax_slice)
  }
}
workshop_type$doses_delivered_this_date[is.na(workshop_type$doses_delivered_this_date)] <- 0
if (round(sum(workshop_type$doses_delivered_this_date)) != round(sum(workshop$doses_delivered_this_date))){stop('doses lost dose!')}

#correct to government reported total vaccine doses! (it appears WHO calculated based on all vaccines in PNG being double-dose)
workshop_type = workshop_type %>% 
  filter(!(dose == 2 & vaccine_type == "Johnson & Johnson")) 
workshop_type$dose[workshop_type$dose == 3 & workshop_type$vaccine_type == "Johnson & Johnson"] = 2

# setting_vaccine
# workshop_type %>% group_by(vaccine_type,dose) %>% summarise(total = sum(doses_delivered_this_date))
check = workshop_type %>% 
  group_by(vaccine_type) %>% 
  summarise(est = sum(doses_delivered_this_date)) %>%
  left_join(setting_vaccine, by = 'vaccine_type') %>%
  filter(abs(doses-est)/doses>0.05)
if (nrow(check)>0){stop('Vaccine type distribution not quite there!')}

workshop = workshop_type; rm(workshop_type)
#_______________________________________________________________________________



### SPLIT BETWEEN AGE AND RISK GROUPS
prioritisation = data.frame()
for (this_dose in unique(setting_vaccine_dn$dose)){
  older_prop = setting_vaccine_dn %>%
    filter(dose == this_dose) %>%
    pivot_wider(names_from = age_floor,
                names_prefix = "age_floor_",
                values_from = delivered) %>%
    mutate(prop = age_floor_45/age_floor_18)
  older_prop = older_prop$prop
  #Note: interestingly lower uptake in older age groups in PNG (although very low proportions anyway so health care workers may have skewed)
  
  prioritisation_this_dose = pop_risk_group_dn %>%
    filter(age_group %in% c("18 to 29",  "30 to 44",  "45 to 59",  "60 to 69",  "70 to 100")) %>%
    mutate(prop_age = pop/sum(pop),
           prioritised_age = case_when(age_group %in% c("18 to 29",  "30 to 44") ~ 1-older_prop,
                                       TRUE ~ older_prop),
           prop = prop_age * prioritised_age,
           prop = prop/sum(prop)) %>%
    select(risk_group,age_group,prop) 
  
  if(round(sum(prioritisation_this_dose$prop),digits=1) != 1){stop('issue with age/risk split!')}
  
  if(this_dose == 1){prioritisation_this_dose$schedule = "primary"}
  if(this_dose == 3){prioritisation_this_dose$schedule = "booster"}
  
  prioritisation = rbind(prioritisation,prioritisation_this_dose)

}

workshop = workshop %>%
  mutate(schedule = case_when(
    dose > 2 ~ "booster",
    dose == 2 & vaccine_type == "Johnson & Johnson" ~ "booster",
    TRUE ~ "primary"
  )) %>%
  left_join(prioritisation, by = 'schedule') %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date*prop)
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
    FROM_vaccine_type = vaccine_type,
    FROM_dose = dose - 1
  ) %>%
  left_join(pop_risk_group_dn, by = c("age_group", "risk_group")) %>%
  group_by(risk_group, age_group, vaccine_type, dose) %>%
  mutate(coverage_this_date = case_when(pop > 0 ~ cumsum(doses_delivered_this_date) /pop,
                                        TRUE ~ 0)) %>%
  select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date,age_group,risk_group,FROM_vaccine_type,FROM_dose)
#_______________________________________________________________________________

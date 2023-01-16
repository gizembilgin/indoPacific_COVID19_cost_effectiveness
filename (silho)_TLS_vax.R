#This program is our best guess as to the distribution of doses delivered in Timor-Leste by age/risk/vaccine type based on the Oxford tracker, and UNICEF Market Dashboard
#NB: no publicly avaliable gov reports since May 2022
this_setting = setting = "TLS"


### SUPPLY
# Note: UNICEF’s COVID-19 Market Dashboard has supply equal to only half of number of doses delivered; same with WHO dashboard has doses administered ~ two times doses received
setting_vaccine_dn <- read.csv("1_inputs/vaccination/vaccine_setting_dn.csv",header=TRUE)%>%
  filter(setting == this_setting)
setting_vaccine_dn$last_update = as.Date(setting_vaccine_dn$last_update,format = '%d/%m/%Y')
setting_vaccine_dn <- setting_vaccine_dn %>% 
  select(last_update,age_floor,vaccine_type,dose,delivered)

#check ggplot
ggplot(setting_vaccine_dn) + geom_point(aes(x=last_update,y=delivered, color = as.factor(dose),shape=as.factor(age_floor))) +
  facet_grid(vaccine_type ~ .) 
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

workshop_num = strategy %>%
   pivot_longer(cols = c('V1_Vaccine_Prioritisation_summary','V2_Vaccine_Availability_summary'),
               names_to = "measure",
               values_to = "value")
workshop_charac = strategy %>%
   pivot_longer(cols = c('V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary','V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary'),
               names_to = "measure",
               values_to = "value")
ggplot() + 
  geom_point(data=workshop_num,aes(x=date,y=value))+ 
  facet_grid(measure ~ .) 
ggplot() + 
  geom_point(data=workshop_charac,aes(x=date,y=value))+ 
  facet_grid(measure ~ .) 
#_______________________________________________________________________________



### SPLIT BETWEEN VACCINE TYPES
workshop = vaccination_history_DOSE_NUMBER 

#dates new vaccine types became available
vax_type_introduction = supply %>%
  group_by(vaccine_type) %>%
  summarise(date = min(delivery_month)+round(365/12/2)) %>% #added component to make it the middle of the delivery month
  arrange(date)
if (min(vax_type_introduction$date) > min(vaccination_history_DOSE_NUMBER$date)){
  vax_type_introduction$date[vax_type_introduction$date == min(vax_type_introduction$date)] = min(vaccination_history_DOSE_NUMBER$date)
}

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
  vax_types_dn_here = supply %>% 
    filter(vaccine_type %in% vax_types_here) %>%
    group_by(vaccine_type) %>%
    summarise(doses = sum(doses))
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

  #booster doses in TLS are 98.8% Pfizer
  if (3 %in% unique(workshop_slice$dose[workshop_slice$doses_delivered_this_date>0])){
    vax_types_booster_dn_here = data.frame(vaccine_type = "Pfizer", prop = 1)
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

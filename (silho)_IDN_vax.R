# This program is our best guess as to the distribution of doses delivered in Indonesia by age/risk/vaccine type 
#


this_setting = setting = "IDN"

workshop = vaccination_history_DOSE_NUMBER %>%
  mutate(dose = as.numeric(dose)) %>%
  filter(doses_delivered_this_date>0) %>%
  select(date,dose,doses_delivered_this_date)

setting_vaccine_dn <- read.csv("1_inputs/vaccination/vaccine_setting_dn.csv",header=TRUE)%>%
  filter(setting == this_setting)
setting_vaccine_dn$last_update = as.Date(setting_vaccine_dn$last_update,format = '%d/%m/%Y')



### INCLUDE FOURTH DOSE #_______________________________________________________
#Based on the official IDN government dashboard (https://vaksin.kemkes.go.id/#/vaccines) the OWID data does not include second booster doses
#NB: denominator of this percentage is eligible population (HCW and elderly 60+)
total_dose4 = setting_vaccine_dn %>%
  filter(dose == 4) %>%
  filter(comments != "coverage_percentage")
total_dose4 = total_dose4 %>%
  filter(last_update == max(total_dose4$last_update))
total_dose4 = sum(total_dose4$delivered)

second_booster_dose = setting_vaccine_dn %>%
  filter(dose == 4) %>%
  select(dose,age_floor,delivered,last_update,comments)  %>%
  filter(comments == "coverage_percentage") %>%
  mutate(internal_prop = delivered/max(second_booster_dose$delivered),
         delivered = internal_prop * total_dose4)

#step out day by day - smooth doses delivered over period since last report
prev_date = second_booster_dose$last_update[1]
prev_value = second_booster_dose$delivered[1]
second_booster_dose_expanded = data.frame(date = prev_date, delivered = 0)

for (step in 1:nrow(second_booster_dose)) {
  this_date = second_booster_dose$last_update[step]
  this_value = second_booster_dose$delivered[step]
  
  if (this_date > prev_date + 1) {
    #  if not simply the next day
    fill = data.frame(date = seq((prev_date + 1), (this_date - 1), by = "days")) %>%
      mutate(
        days_since = as.numeric(date - prev_date),
        delivered = prev_value + days_since * (this_value - prev_value) /
          as.numeric(this_date - prev_date)
      ) %>%
      select(-days_since)
    
    second_booster_dose_expanded = rbind(second_booster_dose_expanded, fill)
  }
  
  prev_date = this_date
  prev_value = this_value
} # end of day by day expansion

second_booster_dose_expanded = second_booster_dose_expanded %>%
  mutate(dose = 4) %>%
  rename(doses_delivered_this_date = delivered) %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date - lag(doses_delivered_this_date,default = 0))
if (round(sum(second_booster_dose_expanded$doses_delivered_this_date)/total_dose4,digits = 2) != 1){stop("dose 4 doesnt align")}

workshop = rbind(workshop,second_booster_dose_expanded)
rm(second_booster_dose_expanded,fill,this_date,this_value,prev_date,prev_value,second_booster_dose,total_dose4)

# to_plot = workshop %>%
#   group_by(dose) %>%
#   arrange(date) %>%
#   mutate(cumulative = cumsum(doses_delivered_this_date))
# ggplot(to_plot) + geom_point(aes(x=date,y=cumulative))+ facet_grid(dose ~ .)
#_______________________________________________________________________________



### INCLUDE AGE DISTRIBUTION #__________________________________________________
#(A/C) age dn of dose 1/2 over time
#ASSUMPTION: dn of doses among age groups in Jakarata is representative of the dn of doses among age groups in wider IDN
this_dn = setting_vaccine_dn %>%
  filter(dose <3) %>%
  filter(comments == "coverage_percentage")
#find pop size for age_groups 6-11,12-17,18-59,60+


#(B/C) age dn of dose 3 (end time point)
this_dn = setting_vaccine_dn %>%
  filter(dose == 3) %>%
  filter(comments != "coverage_percentage")
this_dn = this_dn %>% filter(last_update == max(this_dn$last_update))



#(C/C) age dn of dose 4 (end time point)
#ASSUMPTION: HCW are aged 18-59
this_dn = setting_vaccine_dn %>%
  filter(dose == 4) %>%
  filter(comments != "coverage_percentage")
this_dn = this_dn %>% filter(last_update == max(this_dn$last_update))

#CHECK: coverage this date by dose and age

#Convert from 6-11,12-17,18-59 and 60+ to model age groups





### Inspect: Oxford COVID-19 Government Response Tracker
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

strategy = strategy %>%
  mutate(age_floor_risk = case_when(
    V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary == "16-19 yrs" ~ 18,
    V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary == "35-39 yrs" ~ 35,
    V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary == "60-64 yrs" ~ 60,
    V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary == "80+ yrs" ~ 80,
    V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary == "5-15 yrs" ~ 6,
  ),
  age_floor_general_public = case_when(
    V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary == "16-19 yrs" ~ 18,
    V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary == "35-39 yrs" ~ 35,
    V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary == "60-64 yrs" ~ 60,
    V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary == "80+ yrs" ~ 80,
    V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary == "5-15 yrs" ~ 6,
  ))
workshop_charac = strategy %>%
   pivot_longer(cols = c('age_floor_general_public','age_floor_risk'),
               names_to = "measure",
               values_to = "value")
ggplot() +
  geom_point(data=workshop_charac,aes(x=date,y=value))+
  facet_grid(measure ~ .)
#_______________________________________________________________________________



### LOAD UNICEF’s COVID-19 Market Dashboard
supply <- read.csv("1_inputs/vaccine_market_UNICEF.csv",header=TRUE) 
supply$delivery_month = as.Date(supply$delivery_month, "%d/%m/%Y")

#seven vaccines will blow out the number of model classes, let's condense down to those where there is sufficient doses to cover >2.5% of Indonesia's population
supply$vaccine_type[supply$vaccine_type == "Covishield"] = "AstraZeneca" #The Oxford–AstraZeneca COVID‑19 vaccine, sold under the brand names Covishield and Vaxzevria
supply$vaccine_type[supply$vaccine_type == "Sinopharm"] = "Sinovac"      #Both viral_inactivated
supply$vaccine_type[supply$vaccine_type == "Covavax"] = "Sinovac"        #Both viral_inactivated
supply$vaccine_type[supply$vaccine_type == "Janssen"] = "AstraZeneca"    #Both viral_vector

supply = supply %>%
  group_by(setting,delivery_month,vaccine_type) %>%
  summarise(doses = sum(doses), .groups = "keep") %>%
  filter(setting == this_setting) %>%
  group_by(vaccine_type) %>%
  mutate(cumulative_doses = cumsum(doses))

ggplot(supply) + geom_line(aes(x=delivery_month,y=cumulative_doses))+
  facet_grid(vaccine_type ~ .)
#primarily Pfizer doses in the last few months

supply %>% group_by(vaccine_type) %>% summarise(total = sum(doses)) %>% arrange(total)
#_______________________________________________________________________________
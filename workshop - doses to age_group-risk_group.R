
setting_list            = c("IDN"      ,"FJI" ,"PHL"        ,"PNG"                 ,"SLB"              ,"TLS")
setting_long_list       = c("Indonesia","Fiji","Philippines","Papua New Guinea"    ,"Solomon Islands"  ,"Timor-Leste")

#metadata https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md#vaccination-policies
workshop <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_vaccines_full.csv")
workshop = workshop %>% filter(CountryCode %in% setting_list)
workshop$Date = as.Date(as.character(workshop$Date),"%Y%m%d")  
workshop = workshop %>% 
  select(-contains("V4")) %>% #we aren't interested in mandates
  select(-contains("V3")) %>% # note: all six settings are V3_Vaccine_Financial_Support_summary ==5 -> all categories fully funded by the government
  select(CountryName,
         CountryCode,
         Date,
         contains("yrs"),
         contains("summary"),
         contains("pregnant"),
         contains("notes")
         )

colnames(workshop) = gsub(" ","_", colnames(workshop))
colnames(workshop) = gsub("-","_", colnames(workshop))
colnames(workshop) = gsub("[()+/]","", colnames(workshop))

###check if pregnant eligble
check_pregnant = workshop %>%
  select(CountryName,
                CountryCode,
                Date,
                contains("pregnant"))
ggplot() + 
  geom_point(data=check_pregnant,aes(x=Date,y=V2_Pregnant_people))+ 
  facet_grid(CountryName ~ .) 
rm(check_pregnant)


###check notes
check_notes = workshop %>%
  select(CountryName,
         CountryCode,
         Date,
         contains("notes"))
rm(check_notes)

###check summary
check_summary = workshop %>%
  select(CountryName,
         CountryCode,
         Date,
         contains("summary"))

ggplot() + 
  geom_point(data=check_summary,aes(x=Date,y=V1_Vaccine_Prioritisation_summary))+ 
  facet_grid(CountryName ~ .) 
# 0 - no plan
# 1 – a prioritised plan is in place
# 2 – universal/general eligibility; no prioritisation between groups

ggplot() + 
  geom_point(data=check_summary,aes(x=Date,y=V2_Vaccine_Availability_summary))+ 
  facet_grid(CountryName ~ .) 
# 0 – no categories are receiving vaccines
# 1 – vaccines are available to some categories
# 2 – vaccines are available to anyone over the age of 16 yrs
# 3 – vaccines are available to anyone over the age of 16 yrs PLUS one or both of 5-15 yrs and 0-4 yrs

ggplot() + 
  geom_point(data=check_summary,aes(x=Date,y=V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary))+ 
  facet_grid(CountryName ~ .) 
# 0 – no categories are receiving vaccines
# numerical range – Lowest age range for ‘General’ category

ggplot() + 
  geom_point(data=check_summary,aes(x=Date,y=V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary))+ 
  facet_grid(CountryName ~ .) 
# 0 – no categories are receiving vaccines
# numerical range – Lowest age range from either ‘General’ or ‘At-risk’ categories

rm(check_summary)

##might not need other columns!


workshop_num = check_summary %>%
  filter(CountryCode == this_setting) %>%
  pivot_longer(cols = c('V1_Vaccine_Prioritisation_summary','V2_Vaccine_Availability_summary'),
               names_to = "measure",
               values_to = "value")
workshop_charac = check_summary %>%
  filter(CountryCode == this_setting) %>%
  pivot_longer(cols = c('V2B_Vaccine_age_eligibilityavailability_age_floor_general_population_summary','V2C_Vaccine_age_eligibilityavailability_age_floor_at_risk_summary'),
               names_to = "measure",
               values_to = "value")
ggplot() + 
  geom_point(data=workshop_num,aes(x=Date,y=value))+ 
  facet_grid(measure ~ .) 
ggplot() + 
  geom_point(data=workshop_charac,aes(x=Date,y=value))+ 
  facet_grid(measure ~ .) 


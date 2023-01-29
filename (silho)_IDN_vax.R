# This program is our best guess as to the distribution of doses delivered in Indonesia by age/risk/vaccine type 


this_setting = setting = "IDN"

vaccination_history_WORKSHOP = vaccination_history_DOSE_NUMBER %>%
  mutate(dose = as.numeric(dose)) %>%
  filter(doses_delivered_this_date>0) %>%
  select(date,dose,doses_delivered_this_date)

setting_vaccine_dn <- read.csv("1_inputs/vaccination/vaccine_setting_dn.csv",header=TRUE)%>%
  filter(setting == this_setting)
setting_vaccine_dn$last_update = as.Date(setting_vaccine_dn$last_update,format = '%d/%m/%Y')



### INCLUDE FOURTH DOSE #_______________________________________________________
#Based on the official IDN government dashboard (https://vaksin.kemkes.go.id/#/vaccines) the OWID data does not include second booster doses
#NB: this data is presented as a % coverage for which the denominator is the eligible population (HCW and elderly 60+)
total_dose4 = setting_vaccine_dn %>%
  filter(dose == 4 & comments != "coverage_percentage")
total_dose4 = total_dose4 %>%
  filter(last_update == max(total_dose4$last_update))
total_dose4 = sum(total_dose4$delivered) #absolute number

second_booster_dose = setting_vaccine_dn %>%
  filter(dose == 4 & comments == "coverage_percentage") %>%
  select(dose,age_floor,delivered,last_update,comments)  
second_booster_dose = second_booster_dose %>%
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

vaccination_history_WORKSHOP = rbind(vaccination_history_WORKSHOP,second_booster_dose_expanded)
rm(second_booster_dose_expanded,fill,this_date,this_value,prev_date,prev_value,second_booster_dose,total_dose4)

# to_plot = vaccination_history_WORKSHOP %>%
#   group_by(dose) %>%
#   arrange(date) %>%
#   mutate(cumulative = cumsum(doses_delivered_this_date))
# ggplot(to_plot) + geom_point(aes(x=date,y=cumulative))+ facet_grid(dose ~ .)
# ggplot(to_plot[to_plot$dose == 4,]) + geom_point(aes(x=date,y=cumulative))
#_______________________________________________________________________________



### INCLUDE AGE DISTRIBUTION #__________________________________________________
#find pop size for age_groups 6-11,12-17,18-59,60+
workshop_num <- c(0,5,11,17,59,110)
workshop_labels <- c(0,6,12,18,60)
pop_dn <- pop_setting_orig %>%
  mutate(age_floor = cut(age,breaks = workshop_num, include.lowest = T, labels = workshop_labels)) %>%
  group_by(age_floor) %>%
  summarise(population = sum(population)) %>%
  mutate(age_floor = as.numeric(levels(age_floor)))
pop_dn_denominator = setting_vaccine_dn %>%
  filter(dose <3 & !(str_detect(update_source,"https://corona.jakarta")))%>%
  pivot_wider(names_from = comments,
              values_from = delivered) %>%
  mutate(population = total/coverage_percentage*100) %>%
  group_by(age_floor) %>%
  summarise(population = mean(population))
rm(workshop_num,workshop_labels)


#(A/C) age dn of dose 1/2 over time
# Note: we are using the age distribution of the roll out in Jakarta to infer the age distribution of the roll out across IDN
# We are stretching the age distribution of doses delivered by % of cumulative doses, not date, since the roll out in Jakarta and IDN were of different speeds
this_dn = setting_vaccine_dn %>%
  filter(dose <3 & str_detect(update_source,"https://corona.jakarta"))
zero_rows = crossing(last_update = min(vaccination_history_DOSE_NUMBER$date) - 1,
                       dose = c(1,2),
                       delivered = 0,
                       age_floor = unique(this_dn$age_floor))
this_dn = bind_rows(zero_rows,this_dn) %>%
  group_by(age_floor,dose) %>%
  mutate(delivered_here = delivered - lag(delivered))

# correct where government has jumped back on estimate
#ggplot(this_dn) + geom_point(aes(x=last_update,y=delivered,color=as.factor(dose))) + facet_grid(age_floor ~ .)
issue = this_dn %>% filter(delivered_here < 0 )
while(nrow(issue)>0){
  this_age = issue$age_floor[1]
  this_cutoff = issue$last_update[1]
  correction = this_dn %>%
    filter(age_floor == this_age & last_update < this_cutoff) %>%
    mutate(delivered = delivered + issue$delivered_here[1])
  uncorrected_rows = this_dn %>%
    filter(!(age_floor == this_age & last_update < this_cutoff))
  
  this_dn = rbind(correction,uncorrected_rows)%>%
    group_by(age_floor,dose) %>%
    mutate(delivered_here = delivered - lag(delivered))
  issue = this_dn %>% filter(delivered_here < 0 )
}
rm(zero_rows,this_age,this_cutoff,correction,uncorrected_rows,issue)
this_dn$delivered[this_dn$delivered<0]<-0
#ggplot(this_dn) + geom_point(aes(x=last_update,y=delivered,color=as.factor(dose))) + facet_grid(age_floor ~ .)

# create internal distribution
this_dn = this_dn %>%
  group_by(age_floor,dose) %>%
  mutate(delivered = delivered/max(delivered))
#ggplot(this_dn) + geom_point(aes(x=last_update,y=delivered,color=as.factor(dose))) + facet_grid(age_floor ~ .)

#adjust to national levels
expected = setting_vaccine_dn %>%
  filter(dose <3 & !(str_detect(update_source,"https://corona.jakarta")) & comments == "coverage_percentage")%>%
  rename(expected = delivered) %>%
  select(age_floor,dose,expected)
this_dn = this_dn %>%
  left_join(expected, by = c('age_floor','dose')) %>%
  mutate(delivered = delivered*expected) %>%
  group_by(age_floor,dose) %>%
  mutate(delivered_here = delivered - lag(delivered))
save_plot = ggplot(this_dn) + geom_point(aes(x=last_update,y=delivered,color=as.factor(dose))) + facet_grid(age_floor ~ .)

#derive rough doses delivered -> proportion of doses by age group
this_dn = this_dn %>%
  select(last_update,dose,age_floor,delivered,delivered_here) %>%
  left_join(pop_dn_denominator, by = 'age_floor') %>%
  mutate(delivered_here = (delivered_here/100)*population) %>%
  filter(is.na(delivered_here) == FALSE) %>%
  group_by(last_update,dose) %>%
  mutate(prop = delivered_here/sum(delivered_here)) 
#ggplot(this_dn) + geom_point(aes(x=last_update,y=prop,color=as.factor(age_floor))) + facet_grid(dose ~ .)
#ggplot(this_dn) + geom_point(aes(x=last_update,y=prop,color=as.factor(dose))) + facet_grid(age_floor ~ .)

#check
# check = this_dn %>%
#   group_by(age_floor,dose) %>%
#   mutate(cumsum = cumsum(delivered_here)) %>%
#   mutate(cov_this_date = cumsum/population)
# ggplot(check) + geom_point(aes(x=last_update,y=cov_this_date,color=as.factor(dose))) + facet_grid(age_floor ~ .)
# save_plot

#cumulative sum
workshop = this_dn %>%
  ungroup() %>% group_by(last_update,dose) %>%
  arrange(desc(last_update)) %>%
  summarise(cumulative_this_update = sum(delivered_here), .groups = "keep") %>%
  group_by(dose) %>%
  mutate(cumulative_this_update = cumsum(cumulative_this_update))
this_dn = this_dn %>%
  left_join(workshop, by = c('last_update','dose'))

#adjust to closest date of real known
expected = setting_vaccine_dn %>%
  filter(dose <3 & !(str_detect(update_source,"https://corona.jakarta")) & comments != "coverage_percentage") %>%
  group_by(dose) %>%
  summarise(expected = sum(delivered))
current = workshop %>% 
  filter(last_update == max(workshop$last_update)) %>%
  left_join(expected, by = 'dose') %>%
  mutate(ratio = expected/cumulative_this_update)
ratio = current %>% select(dose,ratio)
this_dn = this_dn %>%
  left_join(ratio, by = "dose") %>%
  mutate(cumulative_this_update = cumulative_this_update * ratio)
  
#apply distribution of dose 1/2
workshop_d12 = vaccination_history_WORKSHOP %>%
  filter(dose < 3) %>%
  group_by(dose) %>%
  mutate(cumulative = cumsum(doses_delivered_this_date)) %>%
  mutate(cumulative_this_update = -99) #dummy value to be replaced

for (this_dose in c(1,2)){
  for (this_update in 1:length(unique(this_dn$cumulative_this_update[this_dn$dose == this_dose]))){ #find latest update
    this_cumulative_update = unique(this_dn$cumulative_this_update[this_dn$dose == this_dose])[this_update]
    workshop_d12$cumulative_this_update[workshop_d12$dose == this_dose & workshop_d12$cumulative <= this_cumulative_update & workshop_d12$cumulative_this_update == -99] = this_cumulative_update #replace only if dummy value
  } 
}
rm(this_cumulative_update)

expected = sum(workshop_d12$doses_delivered_this_date)
workshop_d12 = workshop_d12 %>%
  left_join(this_dn, by = c("dose", "cumulative_this_update"))%>%
  mutate(doses_delivered_this_date = doses_delivered_this_date * prop) %>%
  select(date,dose,age_floor,doses_delivered_this_date) 
if (round(expected) != round(sum(workshop_d12$doses_delivered_this_date))){stop('dose 1/2 lost!')}


#CHECK - compare us of UN vs IDN government denominator
# to_plot = pop_dn %>%
#   filter(age_floor>0) %>%
#   select(age_floor,population) %>%
#   rename(UN_pop_est = population) %>%
#   left_join(pop_dn_denominator, by = 'age_floor') %>%
#   rename(government_pop_est = population) %>%
#   mutate(age_group = case_when(
#     age_floor == 6 ~ "6 to 11",
#     age_floor == 12 ~ "12 to 17",
#     age_floor == 18 ~ "18 to 59",
#     age_floor == 60 ~ "60 to 110"
#   )) %>%
#   pivot_longer(cols = c('government_pop_est','UN_pop_est'),
#                names_to = 'source',
#                values_to = 'population')
# to_plot$age_group <- factor(to_plot$age_group,levels = c("6 to 11","12 to 17","18 to 59","60 to 110"))
# ggplot(to_plot) + geom_col(aes(x=age_group,y=population, fill = source), position = "dodge")
# 
# gov_denom = workshop_d12 %>%
#   group_by(dose,age_floor) %>%
#   arrange(date) %>%
#   mutate(cumulative = cumsum(doses_delivered_this_date))%>%
#   left_join(pop_dn_denominator, by = "age_floor") %>%
#   mutate(cov = cumulative/population)
# gov_denom = gov_denom %>% filter(date == max(gov_denom$date)) %>% select(dose,age_floor,cov) %>% mutate(cov = cov * 100, pop_dn = 'gov estimate') %>% arrange(dose,age_floor)
# 
# UN_denom = workshop_d12 %>%
#   group_by(dose,age_floor) %>%
#   arrange(date) %>%
#   mutate(cumulative = cumsum(doses_delivered_this_date))%>%
#   left_join(pop_dn, by = "age_floor") %>%
#   mutate(cov = cumulative/population)
# UN_denom = UN_denom %>% filter(date == max(UN_denom$date)) %>% select(dose,age_floor,cov) %>% mutate(cov = cov * 100, pop_dn = 'UN estimate') %>% arrange(dose,age_floor)
# 
# to_plot = rbind(gov_denom,UN_denom) %>%
#    mutate(age_group = case_when(
#     age_floor == 6 ~ "6 to 11",
#     age_floor == 12 ~ "12 to 17",
#     age_floor == 18 ~ "18 to 59",
#     age_floor == 60 ~ "60 to 110"
#   )) 
# to_plot$age_group <- factor(to_plot$age_group,levels = c("6 to 11","12 to 17","18 to 59","60 to 110"))
# ggplot(to_plot) + geom_col(aes(x=age_group,y=cov, fill = pop_dn), position = "dodge") + ylab("coverage (%)")
#DECISION: When reconstructing the vaccination history of Indonesia we prioritised the reported absolute number of doses delivered over the reported percentage coverage of age groups. 
#National percentage vaccine coverage differed most notably in the 60 + age group. The denominator used in official reporting did not align with UN population estimates, and was lower 
#in total than the 2020 Indonesian census. The first dose percentage coverage reported for Jakarta was greater than 100% in multiple age groups.
rm(this_dn,workshop,save_plot,expected)
#________________________________________


#(B/C) age dn of booster doses (end time point)
#NB: only available data is the snapshot at the current time point
#ASSUMPTION: that HCW are aged 18-59 (dose 4 eligible)
#import latest data
dose_3 = setting_vaccine_dn %>%
  filter(dose == 3 & comments != "coverage_percentage")
dose_3 = dose_3 %>% filter(last_update == max(dose_3$last_update))
dose_4 = setting_vaccine_dn %>%
  filter(dose == 4& comments != "coverage_percentage")
dose_4 = dose_4 %>% filter(last_update == max(dose_4$last_update))

#calculate proportion
this_dn = rbind(dose_3,dose_4) %>%
  group_by(dose) %>%
  mutate(prop = delivered/sum(delivered)) %>%
  select(dose,age_floor,prop)

#apply proportion
workshop_d34 = vaccination_history_WORKSHOP %>% 
  filter(dose %in% c(3,4)) %>%
  left_join(this_dn, by = 'dose') %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date * prop) %>%
  select(-prop)
#________________________________________


#(C/C)Convert from 6-11,12-17,18-59 and 60+ to model age groups
workshop_num <- c(0,5,11,17,59,110)
workshop_labels <- c(0,6,12,18,60)
this_dn <- pop_setting_orig %>%
  mutate(age_floor = cut(age,breaks = workshop_num, include.lowest = T, labels = workshop_labels),
         agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  group_by(age_floor) %>%
  mutate(model_group_percent = population/sum(population)) %>%
  group_by(agegroup_MODEL,age_floor) %>%
  summarise(model_group_percent = sum(model_group_percent), .groups = "keep") %>%
  mutate(age_floor = as.numeric(levels(age_floor)[as.numeric(age_floor)]))

expected = sum(vaccination_history_WORKSHOP$doses_delivered_this_date)
vaccination_history_WORKSHOP = rbind(workshop_d12,workshop_d34) %>%
  left_join(this_dn, by = c("age_floor"))%>%
  mutate(doses_delivered_this_date = doses_delivered_this_date * model_group_percent) %>%
  select(date,dose,agegroup_MODEL,doses_delivered_this_date) %>%
  rename(age_group = agegroup_MODEL)
if (round(expected) != round(sum(vaccination_history_WORKSHOP$doses_delivered_this_date))){stop('dose 1/2 lost!')}
#_______________________________________________________________________________



### INCLUDE VACCINE TYPE USING UNICEF’s COVID-19 Market Dashboard #______________________________________________________
supply <- read.csv("1_inputs/vaccine_market_UNICEF.csv",header=TRUE) 
supply$delivery_month = as.Date(supply$delivery_month, "%d/%m/%Y")

#seven vaccines will blow out the number of model classes, let's condense down to those where there is sufficient doses to cover >2.5% of Indonesia's population
supply$vaccine_type[supply$vaccine_type == "Covishield"] = "AstraZeneca" #The Oxford–AstraZeneca COVID‑19 vaccine, sold under the brand names Covishield and Vaxzevria
supply$vaccine_type[supply$vaccine_type == "Sinopharm"] = "Sinovac"      #Both viral_inactivated
supply$vaccine_type[supply$vaccine_type == "Covavax"] = "Sinovac"        #Both viral_inactivated
supply$vaccine_type[supply$vaccine_type == "Janssen"] = "AstraZeneca"    #Both viral_vector
# supply %>% group_by(vaccine_type) %>% summarise(total = sum(doses)) %>% arrange(total)

supply = supply %>%
  filter(setting == this_setting) %>%
  group_by(setting,delivery_month,vaccine_type) %>%
  summarise(doses = sum(doses), .groups = "keep") %>%
  group_by(vaccine_type) %>%
  mutate(cumulative_doses = cumsum(doses))
# ggplot(supply) + geom_point(aes(x=delivery_month,y=doses))+ facet_grid(vaccine_type ~ .)
# ggplot(supply) + geom_line(aes(x=delivery_month,y=cumulative_doses))+ facet_grid(vaccine_type ~ .)
# NB: primarily Pfizer doses in the last few months

primary_type_dn = supply %>%
  filter(delivery_month < min(vaccination_history_WORKSHOP$date[vaccination_history_WORKSHOP$dose == 3])) %>%
  group_by(vaccine_type) %>%
  summarise(doses = sum(doses)) %>%
  ungroup() %>%
  mutate(prop = doses/sum(doses))
primary_type_dn = crossing(dose = c(1,2),
                           primary_type_dn)
booster_type_dn = supply %>%
  filter(delivery_month >= min(vaccination_history_WORKSHOP$date[vaccination_history_WORKSHOP$dose == 3])) %>%
  group_by(vaccine_type) %>%
  summarise(doses = sum(doses)) %>%
  ungroup() %>%
  mutate(prop = doses/sum(doses))
booster_type_dn = crossing(dose = c(3,4),
                           booster_type_dn)
this_dn = rbind(primary_type_dn,booster_type_dn) %>% select(-doses)

workshop = vaccination_history_WORKSHOP %>%
  left_join(this_dn, by = "dose") %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date * prop) %>%
  select(-prop)

# workshop = data.frame()
# list_dates = unique(supply$delivery_month)
# prev_date = as.Date('1900-01-01')
# for (this_interval in 1:length(unique(supply$delivery_month))){
#   
#   cutoff = list_dates[this_interval] + 30
#   slice = vaccination_history_WORKSHOP %>%
#     filter(date>= prev_date & date <cutoff)
#   
#   if(nrow(slice)>0){
#     this_supply = supply %>%
#       filter(delivery_month<=cutoff) %>%
#       group_by(vaccine_type) %>%
#       summarise(doses = sum(doses))
#     
#     if(nrow(workshop)>0){
#       previously_delivered =  workshop %>%
#         group_by(vaccine_type) %>%
#         summarise(delivered = sum(doses_delivered_this_date))
#       
#       this_supply = this_supply %>% left_join(previously_delivered, by = "vaccine_type") 
#       this_supply$delivered[is.na(this_supply$delivered)] <- 0
#       this_supply = this_supply %>% mutate(doses = doses-delivered)
#     }
#     if(this_interval == length(unique(supply$delivery_month))){
#       slice = vaccination_history_WORKSHOP %>%
#         filter(date>= prev_date)
#       this_supply = supply %>%
#         filter(delivery_month==list_dates[this_interval])
#     }    
#     
#     for(this_vax in unique(this_supply$vaccine_type)){
#       slice_component = slice %>%
#         mutate(vaccine_type = this_vax,
#                doses_delivered_this_date = doses_delivered_this_date*
#                  (this_supply$doses[this_supply$vaccine_type == this_vax])/sum(this_supply$doses))
#       workshop = rbind(workshop,slice_component)
#     }
#   }
#   prev_date = cutoff
# }
vaccination_history_WORKSHOP = workshop 


#CHECKED dose 2 not delivered before dose 1
d=2
workshop_prev_dose = vaccination_history_WORKSHOP %>%
  filter(dose == d - 1) %>% ungroup() %>%
  select(date,vaccine_type,age_group,doses_delivered_this_date)%>%
  group_by(vaccine_type,age_group) %>%
  mutate(prev_avaliable = cumsum(doses_delivered_this_date))
workshop_next_dose = vaccination_history_WORKSHOP %>%
  filter(dose == d) %>% ungroup() %>%
  select(date,vaccine_type,age_group,doses_delivered_this_date)%>%
  group_by(vaccine_type,age_group) %>%
  mutate(next_delivered = cumsum(doses_delivered_this_date),
         date = date-vaxCovDelay$delay[vaxCovDelay$dose == (d-1)])  %>%
  select(-doses_delivered_this_date)
check = workshop_prev_dose %>%
  left_join(workshop_next_dose, by = c("date", "vaccine_type", "age_group")) %>%
  filter(prev_avaliable < next_delivered) %>%
  mutate(diff = next_delivered - prev_avaliable)
#push back one week
while(nrow(check)>0){
  this_date = check$date[1]
  this_vax = check$vaccine_type[1]
  this_age_group = check$age_group[1]
  this_diff = check$diff[1]
  vaccination_history_WORKSHOP$doses_delivered_this_date[vaccination_history_WORKSHOP$dose == d-1 &
                                                           vaccination_history_WORKSHOP$date == this_date &
                                                           vaccination_history_WORKSHOP$vaccine_type == this_vax &
                                                           vaccination_history_WORKSHOP$age_group == this_age_group] =
    vaccination_history_WORKSHOP$doses_delivered_this_date[vaccination_history_WORKSHOP$dose == d-1 &
                                                             vaccination_history_WORKSHOP$date == this_date &
                                                             vaccination_history_WORKSHOP$vaccine_type == this_vax &
                                                             vaccination_history_WORKSHOP$age_group == this_age_group] + this_diff
  trigger = 0
  search = 7
  while(trigger == 0){
    window = sum(vaccination_history_WORKSHOP$doses_delivered_this_date[vaccination_history_WORKSHOP$dose == d-1 &
                                                                 vaccination_history_WORKSHOP$date > this_date &
                                                                 vaccination_history_WORKSHOP$date <= this_date + search &
                                                               vaccination_history_WORKSHOP$vaccine_type == this_vax &
                                                                 vaccination_history_WORKSHOP$age_group == this_age_group])
    if (window >= this_diff){
      vaccination_history_WORKSHOP$doses_delivered_this_date[vaccination_history_WORKSHOP$dose == d-1 &
                                                               vaccination_history_WORKSHOP$date > this_date &
                                                               vaccination_history_WORKSHOP$date <= this_date + search &
                                                               vaccination_history_WORKSHOP$vaccine_type == this_vax &
                                                               vaccination_history_WORKSHOP$age_group == this_age_group] = (window-this_diff)/search
      trigger = 1
    }
    search = search + 7
  }
  
  workshop_prev_dose = vaccination_history_WORKSHOP %>%
    filter(dose == d - 1) %>% ungroup() %>%
    select(date,vaccine_type,age_group,doses_delivered_this_date)%>%
    group_by(vaccine_type,age_group) %>%
    mutate(prev_avaliable = cumsum(doses_delivered_this_date))
  workshop_next_dose = vaccination_history_WORKSHOP %>%
    filter(dose == d) %>% ungroup() %>%
    select(date,vaccine_type,age_group,doses_delivered_this_date)%>%
    group_by(vaccine_type,age_group) %>%
    mutate(next_delivered = cumsum(doses_delivered_this_date),
           date = date-vaxCovDelay$delay[vaxCovDelay$dose == (d-1)])  %>%
    select(-doses_delivered_this_date)
  check = workshop_prev_dose %>%
    left_join(workshop_next_dose, by = c("date", "vaccine_type", "age_group")) %>%
    filter(prev_avaliable < next_delivered) %>%
    mutate(diff = next_delivered - prev_avaliable)
}



#Split booster doses across vaccine types
workshop = vaccination_history_WORKSHOP %>% filter(dose<3)
for (this_booster in unique(vaccination_history_WORKSHOP$dose[vaccination_history_WORKSHOP$dose>2])){
  workshop_booster_pool =  vaccination_history_WORKSHOP %>% 
    filter(dose == this_booster - 1) %>%
    group_by(age_group,vaccine_type) %>%
    summarise(total = sum(doses_delivered_this_date), .groups = "keep") %>%
    group_by(age_group) %>%
    mutate(prop = total/sum(total)) %>%
    select(-total) %>%
    rename(FROM_vaccine_type = vaccine_type)
  
  workshop_booster = vaccination_history_WORKSHOP %>% 
    filter(dose == this_booster) %>%
    left_join(workshop_booster_pool,by=c("age_group")) %>%
    mutate(doses_delivered_this_date = doses_delivered_this_date*prop)
  
  workshop = rbind(workshop,workshop_booster); rm(workshop_booster,workshop_booster_pool)
}
vaccination_history_WORKSHOP = workshop
#_______________________________________________________________________________


### APPLY RISK GROUPS
this_dn = pop_risk_group_dn %>%
  group_by(age_group) %>%
  mutate(prop = pop/sum(pop))
vaccination_history_WORKSHOP = vaccination_history_WORKSHOP %>%
  select(-prop) %>%
  left_join(this_dn, by = 'age_group') %>%
  mutate(doses_delivered_this_date = doses_delivered_this_date * prop)
#_______________________________________________________________________________


### CREATE FINAL DATASET
vaccination_history_TRUE = vaccination_history_WORKSHOP %>%
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
  group_by(risk_group, age_group, vaccine_type, dose) %>%
  mutate(coverage_this_date = case_when(pop > 0 ~ cumsum(doses_delivered_this_date) /pop,
                                        TRUE ~ 0)) %>%
  select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date,age_group,risk_group,FROM_vaccine_type,FROM_dose)
vaccination_history_TRUE$FROM_vaccine_type[is.na(vaccination_history_TRUE$FROM_vaccine_type)] <- vaccination_history_TRUE$vaccine_type[is.na(vaccination_history_TRUE$FROM_vaccine_type)]
#_______________________________________________________________________________

#rm(vaccination_history_DOSE_NUMBER,vaccination_history_WORKSHOP,workshop,expected,this_dn)




#view = vaccination_history_TRUE %>% group_by(vaccine_type,dose,FROM_vaccine_type,age_group) %>% summarise(sum = sum(doses_delivered_this_date))
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
#   filter(round(prev_avaliable) < round(next_delivered))

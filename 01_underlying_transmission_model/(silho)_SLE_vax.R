#Take John Hopkins reporting of vaccination coverage, a collation of data from WHO, CDC and Our World in Data
workshop <- readr::read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv")
workshop = workshop[workshop$'Country_Region' == setting_long,]

### CORRECT where doses admin < people with at least one dose
workshop_correct = workshop[workshop$Doses_admin<workshop$People_at_least_one_dose,]
while(nrow(workshop_correct)>0){
  for (row in 1:nrow(workshop_correct)){
    this_date = workshop_correct$Date[row]
    #bring People_at_least_one_dose back to latest date that made sense
    workshop$People_at_least_one_dose[workshop$Date == this_date] = workshop$People_at_least_one_dose[workshop$Date == (this_date -1)]
  }
  workshop_correct = workshop[workshop$Doses_admin<workshop$People_at_least_one_dose,]
}

### separate doses delivered into dose one and dose two
### ASSUMPTION: no boosters delivered in SLE (only 0.1% booster coverage at 23/09/22)
vaccination_history = workshop %>%
  rename(date = Date, 
         dose_one = People_at_least_one_dose) %>%
  mutate(dose_two = Doses_admin - dose_one ) %>%
  select(date,dose_one,dose_two) %>%
  pivot_longer(
    cols='dose_one':'dose_two',
    names_to='dose_charac',
    values_to = 'num'
  ) %>%
  mutate(dose = case_when(
    dose_charac == 'dose_one' ~ 1,
    dose_charac == 'dose_two' ~ 2
  )) %>%
  select(-dose_charac)


### CORRECT where doses today < doses yesterday
workshop_correct = vaccination_history %>% 
  group_by(dose) %>%
  mutate(doses_delivered_this_date = num - lag(num)) %>%
  filter(doses_delivered_this_date < 0)
while(nrow(workshop_correct)>0){
  for (row in 1:nrow(workshop_correct)){
    this_dose = workshop_correct$dose[row]
    this_date = workshop_correct$date[row]
    
    vaccination_history$num[vaccination_history$date == this_date & vaccination_history$dose == this_dose ] = 
      vaccination_history$num[vaccination_history$date == (this_date-1) & vaccination_history$dose == this_dose ]
  }
  
  workshop_correct = vaccination_history %>% 
    group_by(dose) %>%
    mutate(doses_delivered_this_date = num - lag(num)) %>%
    filter(doses_delivered_this_date < 0)
}


### CORRECT where dose 2 delivered before dose 1 available
workshop_dose1 = vaccination_history %>% filter(dose == 1) %>%
  rename(cum_doses = num) 
workshop_dose2 = vaccination_history %>% filter(dose == 2) %>%
  rename(cum_doses = num) 

timing_check = workshop_dose1 %>%
  mutate(dose = 2,
         date = date+vaxCovDelay$delay[vaxCovDelay$dose == 1])  %>%
  rename(dose1_avaliable = cum_doses)

timing_check = workshop_dose2 %>%
  left_join(timing_check, by = c("date", "dose")) %>%
  filter(dose1_avaliable < cum_doses)

for (row in 1:nrow(timing_check)){
  this_date = timing_check$date[row]-vaxCovDelay$delay[vaxCovDelay$dose == 1]
  vaccination_history$num[vaccination_history$dose == 1 & vaccination_history$date == this_date] =
    vaccination_history$num[vaccination_history$dose == 1 & vaccination_history$date == this_date] + timing_check$cum_doses[row]
}


### Load data on vaccine supply in from African CDC Dashboard https://africacdc.org/covid-19-vaccination/ (last update 23/09/2022)
this_setting = setting
setting_vaccine <- read.csv("01_inputs/vaccination/vaccine_setting_history.csv",header=TRUE)%>%
  filter(setting == this_setting)
setting_vaccine$last_update = as.Date(setting_vaccine$last_update,format = '%d/%m/%Y')
setting_vaccine <- setting_vaccine %>% filter( last_update == max(setting_vaccine$last_update))

if ("Johnson & Johnson" %in% unique(setting_vaccine$vaccine_type)){ #J&J the only single-dose vaccine
  setting_vaccine <- setting_vaccine %>%
    mutate(
      dose_one = case_when(
        vaccine_type == "Johnson & Johnson" ~ 2*doses/(sum(setting_vaccine$doses)+setting_vaccine$doses[setting_vaccine$vaccine_type == "Johnson & Johnson"]),
        vaccine_type != "Johnson & Johnson" ~ doses/(sum(setting_vaccine$doses)+setting_vaccine$doses[setting_vaccine$vaccine_type == "Johnson & Johnson"])
      ),
      dose_two = case_when(
        vaccine_type == "Johnson & Johnson" ~ 0,
        vaccine_type != "Johnson & Johnson" ~ doses/(sum(setting_vaccine$doses)- setting_vaccine$doses[setting_vaccine$vaccine_type == "Johnson & Johnson"])
      ))
  
} else {
  setting_vaccine <- setting_vaccine %>%
    mutate(dose_one=doses/sum(setting_vaccine$doses),
           dose_two=doses/sum(setting_vaccine$doses))
}

setting_vaccine_2 <- setting_vaccine %>%
  select(vaccine_type,dose_two,dose_one) %>%
  pivot_longer(
    cols='dose_one':'dose_two',
    names_to='dose_charac',
    values_to = 'prop'
  ) %>%
  mutate(dose = case_when(
    dose_charac == 'dose_one' ~ 1,
    dose_charac == 'dose_two' ~ 2
  )) %>%
  select(-dose_charac)


### Separate vaccine coverage into vaccine type
#PART ONE: reserve dose 1 for individuals who will later receive a second dose of a double-dose vaccines
workshop_dose1 = vaccination_history %>% filter(dose == 1) %>% rename(cum_doses = num) 
workshop_dose2 = vaccination_history %>% filter(dose == 2) %>% rename(cum_doses = num) 

timing_check = workshop_dose2 %>%
  mutate(dose = 1,
         date = date-vaxCovDelay$delay[vaxCovDelay$dose == 1])  %>%
  rename(dose2_required= cum_doses)
timing_check = workshop_dose1 %>% 
  left_join(timing_check, by = c("date", "dose"))

#correct where no dose 2 21 days after to compare to
if (nrow(timing_check[is.na(timing_check$dose2_required),]) == vaxCovDelay$delay[vaxCovDelay$dose == 1]){
  timing_check$dose2_required[is.na(timing_check$dose2_required)] = max(timing_check$dose2_required,na.rm=TRUE)
} else{
  warning('issue with NA in timing_check')
}

setting_vaccine_part_one = setting_vaccine_2 %>% filter(dose == 2 & prop > 0) %>% mutate(dose = 1)

part_one = timing_check %>%
  rename(num=dose2_required) %>%
  select(-cum_doses) %>% 
  left_join(setting_vaccine_part_one, by = 'dose')  %>%
  select(date, vaccine_type, dose, num, prop)

#PART TWO: dose 2 for double-dose vaccines (companion doses to part one)
part_two = vaccination_history %>% 
  filter(dose == 2) %>% 
  left_join(setting_vaccine_2, by = 'dose')  %>%
  select(date, vaccine_type, dose, num, prop)

#PART THREE: dose 1 for individuals who will NOT receive a second dose
#CAUTION - hard coded as drawn from African CDC Dashboard (last update 28/09/2022)
partial_cov = 34.1
full_cov = 25.6

full_cov_part_two = 100*unique(part_two$num[part_two$date == max(part_two$date)])/sum(pop)
full_cov_part_three = full_cov - full_cov_part_two
partial_cov_part_three = partial_cov - full_cov
single_dose_vaccine_ratio = full_cov_part_three/(full_cov_part_three+partial_cov_part_three)
if (single_dose_vaccine_ratio>1 | single_dose_vaccine_ratio<0){stop('single_dose_vaccine_ratio ill configured')}



setting_vaccine_part_three_pt1 = setting_vaccine_2 %>% filter(prop > 0 & dose == 2)  %>% mutate(dose = 1, prop = prop * (1-single_dose_vaccine_ratio) )
setting_vaccine_part_three_pt2 = setting_vaccine_2 %>% filter(prop == 0 & dose == 2) %>% mutate(dose = 1, prop = single_dose_vaccine_ratio)
setting_vaccine_part_three = rbind(setting_vaccine_part_three_pt1,setting_vaccine_part_three_pt2)
if(round(sum(setting_vaccine_part_three$prop),digits=4) != 1){stop('setting_vaccine_part_three total > 100%')}


part_three = timing_check %>%
  mutate(num = cum_doses - dose2_required,
         num_inital = num) 
#CORRECT - no negative doses!
while(nrow(na.omit(part_three[part_three$num > lead(part_three$num),]))>0){
  part_three = part_three %>%
    mutate(num = case_when(
      num > lead(num,na.rm=TRUE) ~ lead(num),
      TRUE ~ num
    ))
}

part_three = part_three  %>%
  select(-cum_doses,-dose2_required) %>% 
  left_join(setting_vaccine_part_three, by = 'dose')  %>%
  select(date, vaccine_type, dose, num, prop)

#BRING PART 1-3 TOGETHER  
vaccination_history_2 = rbind(part_one,part_two,part_three)



vaccination_history_2 = vaccination_history_2 %>%
  mutate(coverage_this_date_num = num*prop) %>% 
  group_by(date,vaccine_type,dose) %>%
  summarise(coverage_this_date_num = sum(coverage_this_date_num), .groups='keep') %>%
  mutate(coverage_this_date = 100 * coverage_this_date_num / sum(pop)) %>%
  group_by(dose,vaccine_type) %>%
  arrange(date) %>%
  mutate(doses_delivered_this_date = coverage_this_date_num - lag(coverage_this_date_num))

if(sum(vaccination_history_2$doses_delivered_this_date,na.rm=TRUE) != sum(vaccination_history$num[vaccination_history$date == max(vaccination_history$date)])){
  stop('doses dont align between vaccination_history and vaccination_history_2')
}

vaccination_history_3 <- vaccination_history_2 %>%
  mutate(    vaccine_mode = case_when(
    vaccine_type == 'Pfizer' ~ 'mRNA',
    vaccine_type == 'Moderna' ~ 'mRNA',
    vaccine_type == 'AstraZeneca' ~ 'viral_vector',
    vaccine_type == 'Sinopharm' ~ 'viral_inactivated',
    vaccine_type == 'Sinovac' ~ 'viral_inactivated',
    vaccine_type == 'Johnson & Johnson' ~ 'viral_vector'
  )) 
vaccination_history_3 <- na.omit(vaccination_history_3) # nrows = 1365-5 = 1360

vaccination_history_POP <- vaccination_history_3 %>%
  select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date) %>%
  arrange(date,vaccine_type,dose)
if (nrow(vaccination_history_POP[vaccination_history_POP$doses_delivered_this_date<0,])>0){stop('negative doses delivered!')}

### CHECK PARITAL/FULL SCHEDULE COVERAGE ALIGNS
# vaccination_history_POP %>%
#   filter(date == max(vaccination_history_POP$date)) %>%
#   group_by(dose) %>%
#   summarise(sum=sum(coverage_this_date))
# #35% aligns with 34% partial coverage
# 
# vaccination_history_POP %>%
#   filter(date == max(vaccination_history_POP$date)) %>%
#   mutate(vaccination_status = case_when(
#     vaccine_type == 'Johnson & Johnson' ~ 'full',
#     dose == 2 ~ 'full',
#     TRUE ~ 'not complete'
#   )) %>%
#   group_by(vaccination_status) %>%
#   summarise(sum=sum(coverage_this_date))
# #26% aligns with 26% full coverage

### CHECK SUPPLY > DELIVERED
total_doses = vaccination_history_POP %>%
  group_by(vaccine_type) %>%
  summarise(delivered_doses=sum(doses_delivered_this_date)) %>%
  mutate(prop_delivered_doses = delivered_doses/sum(delivered_doses))
supply = setting_vaccine %>% select(vaccine_type,doses) %>%
  rename(supply_doses = doses) %>%
  mutate(prop_supply_doses = supply_doses/sum(supply_doses))

check = supply %>% left_join(total_doses,by='vaccine_type')
if (nrow(check[check$supply_doses < check$delivered_doses,])){warning('supply<delivered doses')}

if (fitting == "off"){
  vaccination_history_POP = vaccination_history_POP %>% filter(date <= date_start)
}
##_____________________________________________________________________________


##(ii/iii) Split daily doses by age and risk_____________________________________
#LIMITATION - only one risk group at a time!
vaccination_history_TRUE = data.frame() 

if (risk_group_toggle == "off"){
  #ASSUMPTION uniform distribution in ages 18+
  age_split =  pop_setting  %>% 
    mutate(adult_pop = case_when(
      age_group %in% c('0 to 4','5 to 9','10 to 17') ~ 0,
      TRUE ~ pop)) %>%
    mutate(split = adult_pop/sum(adult_pop)) %>%
    select(-adult_pop)
  
  for (j in 1:num_age_groups){
    workshop = vaccination_history_POP %>% 
      ungroup() %>%
      mutate(age_group = age_group_labels[j],
             doses_delivered_this_date = doses_delivered_this_date*age_split$split[j])
    vaccination_history_TRUE = rbind(vaccination_history_TRUE,workshop)
  }
  vaccination_history_TRUE$risk_group = "general_public"
  
} else if (risk_group_toggle == "on"){
  if (is.na(risk_group_lower_cov_ratio)){
    
    age_risk_split =  pop_risk_group_dn %>%
      mutate(adult_pop = case_when(
        age_group %in% c('0 to 4','5 to 9','10 to 17') ~ 0,
        TRUE ~ pop)) %>%
      mutate(split = adult_pop/sum(adult_pop)) %>%
      select(-adult_pop)
    
    if (is.na(risk_group_prioritisation_to_date) == FALSE){
      
      workshop = age_risk_split
      age_list = unique(workshop$age_group)[!(unique(workshop$age_group) %in% workshop$age_group[workshop$split == 0])]
      
      for (i in 1:length(age_list)){
        age_risk_split$split[ age_risk_split$risk_group == risk_group_name &  age_risk_split$age_group == age_list[i]] =  
          sum(workshop$split[ age_risk_split$age_group == age_list[i]]) * risk_group_prioritisation_to_date 
        
        age_risk_split$split[ age_risk_split$risk_group == 'general_public' &  age_risk_split$age_group == age_list[i]] =  
          sum(workshop$split[ age_risk_split$age_group == age_list[i]]) * (1-risk_group_prioritisation_to_date) 
      }
    }
    
    if (sum(age_risk_split$split) != 1){stop('(1) simulate setting line 290: dn of doses >1')}
    
    for (r in 1:num_risk_groups){
      for (j in 1:num_age_groups){
        workshop = vaccination_history_POP %>% mutate(
          age_group = age_group_labels[j],
          risk_group = risk_group_labels[r],
          doses_delivered_this_date = doses_delivered_this_date*age_risk_split$split[age_risk_split$age_group == age_group_labels[j] & age_risk_split$risk_group == risk_group_labels[r]])
        
        vaccination_history_TRUE = rbind(vaccination_history_TRUE,workshop)
      }
    }
    if (sum(vaccination_history_POP$doses_delivered_this_date) != sum(vaccination_history_TRUE$doses_delivered_this_date)){
      stop('vaccine doses in vaccination_history_POP != total vaccine doses in vaccination_history_TRUE')
    }
    
  } else{
    
    #calculate if lower initial coverage in risk group due to vaccine hesitancy
    workshop_ratio = sum(pop_high_risk$pop[!pop_high_risk$age_group %in% c('0 to 4','5 to 9','10 to 17')])/sum(pop_general_public$pop[!pop_general_public$age_group %in% c('0 to 4','5 to 9','10 to 17')])
    apply_cov_ratio = 1+risk_group_lower_cov_ratio*workshop_ratio
    apply_cov_ratio = (1/apply_cov_ratio)
    
    age_risk_split =  pop_risk_group_dn %>%
      mutate(adult_pop = case_when(
        age_group %in% c('0 to 4','5 to 9','10 to 17') ~ 0,
        TRUE ~ pop)) %>%
      group_by(risk_group) %>%
      mutate(split = adult_pop/sum(adult_pop)) %>%
      select(-adult_pop)
    
    if (sum(age_risk_split$split) != 2){stop('(1) simulate setting line ~350: dn of doses >100%')}
    
    for (r in 1:num_risk_groups){
      for (j in 1:num_age_groups){
        workshop = vaccination_history_POP %>% mutate(
          age_group = age_group_labels[j],
          risk_group = risk_group_labels[r],
          doses_delivered_this_date = case_when(
            risk_group_labels[r] == risk_group_name ~ doses_delivered_this_date*(1-apply_cov_ratio)*age_risk_split$split[age_risk_split$age_group == age_group_labels[j] & age_risk_split$risk_group == risk_group_labels[r]],
            TRUE ~ doses_delivered_this_date*apply_cov_ratio*age_risk_split$split[age_risk_split$age_group == age_group_labels[j] & age_risk_split$risk_group == risk_group_labels[r]]))
        
        vaccination_history_TRUE = rbind(vaccination_history_TRUE,workshop)
      }
    }
    if (sum(vaccination_history_POP$doses_delivered_this_date) != sum(vaccination_history_TRUE$doses_delivered_this_date)){stop('(1) simulate setting line 371')}
  }
} 

#Let's recalculate coverage_this_date here
vaccination_history_TRUE = vaccination_history_TRUE %>% 
  left_join(pop_risk_group_dn, by = c("age_group", "risk_group")) %>%
  group_by(risk_group,age_group,vaccine_type,dose) %>%
  mutate(coverage_this_date = case_when(
    pop > 0 ~ cumsum(doses_delivered_this_date)/pop,
    TRUE ~ 0)) %>%
  select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date,age_group,risk_group)

#CHECK
workshop_dose1 = vaccination_history_TRUE %>% filter(dose == 1) %>% 
  mutate(cum_doses = cumsum(doses_delivered_this_date)) %>%
  select(-doses_delivered_this_date,-coverage_this_date)
workshop_dose2 = vaccination_history_TRUE %>% filter(dose == 2) %>% 
  mutate(cum_doses = cumsum(doses_delivered_this_date)) %>%
  select(-doses_delivered_this_date,-coverage_this_date)

timing_check = workshop_dose1 %>%
  mutate(dose = 2,
         date = date+vaxCovDelay$delay[vaxCovDelay$dose == 1])  %>%
  rename(dose1_avaliable = cum_doses)

timing_check = workshop_dose2 %>%
  left_join(timing_check) %>%
  filter(cum_doses>0) %>%
  filter(dose1_avaliable < cum_doses)

if(length(unique(timing_check$date))>0){stop('Vaccine doses 2 delivered before vaccine doses 1')}
##_____________________________________________________________________________


##(iii/iii) Plot _______________________________________________________________
#COMEBACK: doses delivered very jagged, should we do seven day rolling average?
# vaccination_history <- vaccination_history %>%
#   mutate(coverage_this_date = 100 * num / sum(pop))
# 
# ggplot() +
#   geom_point(data=vaccination_history,aes(x=date,y=coverage_this_date,color=as.factor(dose)),na.rm=TRUE) +
#   labs(title=setting_long) +
#   xlab("") +
#   scale_x_date(date_breaks="1 month", date_labels="%b") +
#   ylab("vaccine coverage (%)") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(color = 'black'))



rm(workshop, timing_check, workshop_dose1, workshop_dose2, vaccination_history_3, vaccination_history_2, vaccination_history, setting_vaccine, setting_vaccine_2)

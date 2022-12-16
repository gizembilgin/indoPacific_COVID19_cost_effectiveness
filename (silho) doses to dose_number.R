require(readr)
require(ggplot2)
require(gridExtra)
require(ggpubr)
require(tidyverse)


### This program loads raw data collated by Our World in Data (OWID) for our individual setting,
### and distributes doses delivered between dose numbers

###DECISION: we are going to use OWID database instead of John Hopkins (used in previous paper) because of the detail provided
#option1 <- readr::read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv")
#option2 <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

#We will use the OWID smoothed statistics as a comparison. 
#We can not us them in the model since smoothed people vaccinated > total doses delivered at some points; and other discontinuity issues. 
comparison = readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv") %>%
  filter(iso_code %in% setting) %>%
  select(iso_code,date,daily_vaccinations,daily_people_vaccinated)
#daily_vaccinations: new doses administered per day (7-day smoothed)
#daily_people_vaccinated: daily number of people receiving a first COVID-19 vaccine dose (7-day smoothed)


this_setting = setting
  
setting_names = data.frame(setting = c("IDN","FJI","PHL","PNG","SLB","TLS"),
                           setting_long= c("Indonesia","Fiji","Philippines","Papua New Guinea","Solomon Islands","Timor-Leste"),
                           setting_input_text = c("Indonesia","Fiji","Philippines","Papua%20New%20Guinea","Solomon%20Islands","Timor")) %>%
  filter(setting == this_setting)


### Step One: load raw country profile
raw = readr::read_csv(paste("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/",setting_names$setting_input_text,".csv",sep=""))
#_____________________________________


### Step Two: pivot each raw column long
raw_long = raw %>%
  pivot_longer(cols = c('total_vaccinations','people_vaccinated','people_fully_vaccinated','total_boosters'),
               names_to = "measure",
               values_to = "value")
#_____________________________________


### Step Three: remove NA and add 0 for booster start dates
workshop = data.frame(na.omit(raw_long))

if(this_setting == "FJI"){
  booster_start_date = data.frame(location = "Fiji", date = as.Date('2022-01-06'), vaccine = NA, source_url = "https://www.health.gov.fj/booster-doses/",
                                  measure = "total_boosters", value = 0)
  workshop = rbind(workshop,booster_start_date)
  
  #add in second booster dose as well
  #note: total_vaccinations - people_vaccinated - people_fully_vaccinated - total_boosters aligns perfectly with second booster doses delivered reported  in Pacific Data
  total_boosters2 = raw %>% 
    mutate(value = total_vaccinations - people_vaccinated - people_fully_vaccinated - total_boosters,
           measure = "total_boosters2") %>%
    filter(value>0) %>%
    select(-total_vaccinations,-people_vaccinated, -people_fully_vaccinated, -total_boosters)
  booster_start_date = data.frame(location = "Fiji", date = as.Date('2022-05-26'), vaccine = NA, source_url = "https://www.health.gov.fj/second-booster-dose/",
                                  measure = "total_boosters2", value = 0)
  workshop = rbind(workshop,booster_start_date,total_boosters2)
  
} else if (this_setting == "IDN"){
  booster_start_date = data.frame(location = "Indonesia", date = as.Date('2022-01-11'), vaccine = NA, source_url = "https://sehatnegeriku.kemkes.go.id/baca/rilis-media/20220111/2139141/vaksinasi-booster-gratis-dimulai-12-januari-2022/",
                                  measure = "total_boosters", value = 0)
  workshop = rbind(workshop,booster_start_date)
  
} else if (this_setting == "PHL"){
  booster_start_date = data.frame(location = "Philippines", date = as.Date('2021-11-16'), vaccine = NA, source_url = "https://www.rappler.com/nation/health-workers-may-get-booster-shots-against-covid-19-starting-november-17/",
                                  measure = "total_boosters", value = 0)
  workshop = rbind(workshop,booster_start_date)
    
} else if (this_setting %in% c("PNG","TLS","SLB")){
  #can't find booster dose start date online, therefore need a best guess based on roll out rate
  find_booster_start = workshop %>% 
    filter(measure == "total_boosters") %>%
    mutate(rollout = (value - lag(value))/as.numeric(date-lag(date)))
  est_date = min(find_booster_start$date) -  min(find_booster_start$value)/mean(find_booster_start$rollout[find_booster_start$date < min(find_booster_start$date) + 60],na.rm=TRUE)
  rm(find_booster_start)
  
  #COMEBACK - hard coded as I can't get the date format to work! :(
  if (this_setting == "PNG"){
    booster_start_date = data.frame(location = "PNG", date = as.Date("2022-01-26"), vaccine = NA, source_url = NA, measure = "total_boosters", value = 0)
  } else if (this_setting == "SLB"){
    booster_start_date = data.frame(location = "Solomon Islands", date = as.Date("2022-03-16"), vaccine = NA, source_url = NA, measure = "total_boosters", value = 0)
  } else if (this_setting == "TLS"){
    booster_start_date = data.frame(location = "Timor", date = as.Date("2022-03-07"), vaccine = NA, source_url = NA, measure = "total_boosters", value = 0)
    #also add rows for total_vax and people_vax
    total_vax = data.frame(location = "Timor", date = min(raw$date)-1, vaccine = NA, source_url = NA, measure = "total_vaccinations", value = 0)
    people_vax = data.frame(location = "Timor", date = min(raw$date)-1, vaccine = NA, source_url = NA, measure = "people_vaccinated", value = 0)
    workshop = rbind(workshop,total_vax,people_vax); rm(total_vax,people_vax)
  } 
  
  workshop = rbind(workshop,booster_start_date)
} 

#limit to last zero entry
if (length(unique(workshop$measure)) != length(unique(workshop$measure[workshop$value == 0]))){stop('not all measures have a zero date!')}
max_date_zero = workshop %>% 
  filter(as.numeric(value) == 0) %>%
  group_by(measure) %>%
  summarise(max_zero_date = max(date))
workshop= workshop %>% 
  left_join(max_date_zero, by = 'measure') %>%
  filter(date >= max_zero_date) %>%
  select(-max_zero_date)
#_____________________________________


### Step Four: step out day by day - smooth doses delivered over period since last report
workshop_expanded = data.frame()
workshop = workshop %>% group_by(measure) %>% arrange(measure,date) %>% select(date,measure,value)
  
prev_measure = workshop$measure[1]
prev_date = workshop$date[1]
prev_value = workshop$value[1]
  
for (step in 1:nrow(workshop)) {
  this_measure = workshop$measure[step]
  this_date = workshop$date[step]
  this_value = workshop$value[step]
  
  if (this_measure == prev_measure) {# if same measure, and
    if (this_date > prev_date + 1) {#  if not simply the next day
      fill = data.frame(date = seq((prev_date + 1), (this_date - 1), by = "days"),
                        measure = this_measure) %>%
        mutate(
          days_since = as.numeric(date - prev_date),
          value = prev_value + days_since * (this_value - prev_value) /
            as.numeric(this_date - prev_date)
        ) %>%
        select(-days_since)
      
      workshop_expanded = rbind(workshop_expanded, fill)
    }
  }
  
  workshop_expanded = rbind(workshop_expanded, workshop[step, ])
  
  prev_measure = this_measure
  prev_date = this_date
  prev_value = this_value
} # end of day by day expansion
#_____________________________________

  
### Step Five: pivot back wider
workshop = workshop_expanded %>%
  pivot_wider(names_from = measure,
              values_from = value)
workshop[is.na(workshop)] <- 0

if(nrow(workshop[workshop$people_fully_vaccinated > workshop$people_vaccinated,])){stop("people_fully_vaccinated >people_vaccinated")}
if(nrow(workshop[workshop$total_boosters > workshop$people_fully_vaccinated,])){stop("total_boosters >people_fully_vaccinated")}

if ("total_boosters2" %in% names(workshop)){
  workshop = workshop %>%
    mutate(total_vaccinations = case_when(
      (people_vaccinated + people_fully_vaccinated + total_boosters + total_boosters2) < total_vaccinations ~ (people_vaccinated + people_fully_vaccinated + total_boosters + total_boosters2),
      TRUE ~ total_vaccinations))
  check = workshop %>%
    filter(round(total_vaccinations) > round(people_vaccinated + people_fully_vaccinated + total_boosters + total_boosters2)) %>%
    mutate(diff = total_vaccinations - (people_vaccinated + people_fully_vaccinated + total_boosters + total_boosters2),
           diff_percentage = diff/total_vaccinations)
  if (nrow(check)>0){stop("total_vaccinations is illconfigured")}
} else{
  workshop = workshop %>%
    mutate(total_vaccinations = case_when(
      (people_vaccinated + people_fully_vaccinated + total_boosters) < total_vaccinations ~ (people_vaccinated + people_fully_vaccinated + total_boosters),
      TRUE ~ total_vaccinations))
  check = workshop %>%
    filter(round(total_vaccinations) > round(people_vaccinated + people_fully_vaccinated + total_boosters)) %>%
    mutate(diff = total_vaccinations - (people_vaccinated + people_fully_vaccinated + total_boosters),
           diff_percentage = diff/total_vaccinations)
  if (nrow(check)>0){stop("total_vaccinations is illconfigured")}
}
#_____________________________________


### Step Six: split into doses  
#CORRECT in individual nations
#total vax seemed to be configured to asssume all 2-dose vax, including for PNG and IDN when not true
# if (length(grep("Johnson", raw$vaccine))>0){
#   workshop = workshop %>%
#     mutate(dose3 = round(total_boosters,digits=4),
#            dose2 = round(people_fully_vaccinated - ((people_vaccinated + people_fully_vaccinated + total_boosters) - total_vaccinations),digits=4), #difference in total vax and other three metrics is dose 1 full
#            dose1 = round(people_vaccinated,digits=4),
#            dose1_JJ = round(((people_vaccinated + people_fully_vaccinated + total_boosters) - total_vaccinations),digits=4),
#     )
# } else{
workshop = workshop %>%
  rename(dose1 = people_vaccinated,
         dose2 = people_fully_vaccinated,
         dose3 = total_boosters) %>%
  select(-total_vaccinations)
if ("total_boosters2" %in% names(workshop)) {
  workshop = workshop %>%
    rename(dose4 = total_boosters2)
}
# }
#_____________________________________

  
### Step Seven: split into doses_delivered_per_day
workshop = workshop %>%
  pivot_longer(
    cols = c(2:ncol(workshop)),
    names_to = "dose",
    names_prefix = "dose*",
    values_to = "cumulative_doses"
  ) %>%
  group_by(dose) %>%
  arrange(date) %>%
  mutate(doses_delivered_this_date = cumulative_doses - lag(cumulative_doses)) %>%
  mutate(iso_code = this_setting) %>%
  select(-cumulative_doses)
workshop[is.na(workshop)] <- 0
if (nrow(workshop[round(workshop$doses_delivered_this_date) < 0, ]) > 0) {stop('negative doses_delivered_this_date!')}
#ggplot(workshop) + geom_line(aes(x = date, y = doses_delivered_this_date, color = as.factor(dose)))

# compare to smoothed provided
# check_1 = workshop %>%
#   filter(dose == 1) %>%
#   left_join(comparison, by = c('iso_code', 'date')) %>%
#   mutate(diff = doses_delivered_this_date - daily_people_vaccinated)
# check_2 = workshop %>%
#   group_by(iso_code, date) %>%
#   summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>%
#   left_join(comparison, by = c('iso_code', 'date')) %>%
#   mutate(diff = doses_delivered_this_date - daily_vaccinations)
# 
# sum(check_1$diff, na.rm = TRUE)
# sum(check_2$diff, na.rm = TRUE)
# sum(workshop$doses_delivered_this_date[workshop$dose == 3])
# #<0.5% of pop
 

vaccination_history_DOSE_NUMBER = workshop
rm(comparison,workshop,check,workshop_expanded,max_date_zero)


### FORCE previous dose to be delivered before next dose
#NB: not an issue for FJI, but an issue for IDN
check_conserved = round(sum(vaccination_history_DOSE_NUMBER$doses_delivered_this_date)) #save total number of doses as comparison

for (d in 2:max(vaccination_history_DOSE_NUMBER$dose)){
  workshop_prev_dose = vaccination_history_DOSE_NUMBER %>% 
    filter(dose == d - 1) %>%
    group_by(iso_code) %>%
    mutate(prev_avaliable = cumsum(doses_delivered_this_date))
  
  workshop_next_dose = vaccination_history_DOSE_NUMBER %>% 
    filter(dose == d) %>%
    group_by(iso_code) %>%
    mutate(next_delivered = cumsum(doses_delivered_this_date)) 
  
  timing_check = workshop_next_dose %>%
    mutate(date = date-vaxCovDelay$delay[vaxCovDelay$dose == (d-1)])  %>%
    select(-dose,-doses_delivered_this_date)
  timing_check = workshop_prev_dose %>%
    left_join(timing_check, by = c("date","iso_code")) %>%
    filter(prev_avaliable < next_delivered)
  
  while(nrow(timing_check)>0){
    ammend_date = timing_check$date[1]
    ammend_amount = timing_check$next_delivered[1] - timing_check$prev_avaliable[1] 
    
    if (ammend_date - 1 < min(vaccination_history_DOSE_NUMBER$date)){#if date to fix before vaccine history starts
      row = data.frame(date = ammend_date - 1,
                       dose = d - 1,
                       doses_delivered_this_date = ammend_amount,
                       iso_code = this_setting)
      vaccination_history_DOSE_NUMBER = rbind(row,vaccination_history_DOSE_NUMBER)
    } else{
      vaccination_history_DOSE_NUMBER$doses_delivered_this_date[vaccination_history_DOSE_NUMBER$dose == (d-1) &
                                                                  vaccination_history_DOSE_NUMBER$date == ammend_date - 1] =  
        vaccination_history_DOSE_NUMBER$doses_delivered_this_date[vaccination_history_DOSE_NUMBER$dose == (d-1) & vaccination_history_DOSE_NUMBER$date == ammend_date - 1] + ammend_amount
    }
    
    trial = 1
    while(trial > 0){#remove this amount from first sensible interval
      window = (trial-1) * 7 + 1
      if (sum(vaccination_history_DOSE_NUMBER$doses_delivered_this_date[vaccination_history_DOSE_NUMBER$dose == (d-1) & 
                                                                        vaccination_history_DOSE_NUMBER$date >= ammend_date &
                                                                        vaccination_history_DOSE_NUMBER$date < ammend_date + window])>ammend_amount){
        vaccination_history_DOSE_NUMBER$doses_delivered_this_date[vaccination_history_DOSE_NUMBER$dose == (d-1) & vaccination_history_DOSE_NUMBER$date >= ammend_date  &
                                                                    vaccination_history_DOSE_NUMBER$date < ammend_date + window] =  
          (sum(vaccination_history_DOSE_NUMBER$doses_delivered_this_date[vaccination_history_DOSE_NUMBER$dose == (d-1) & vaccination_history_DOSE_NUMBER$date >= ammend_date  &
                                                                           vaccination_history_DOSE_NUMBER$date < ammend_date + window]) - ammend_amount)/window
        
        trial = 0 #end loop if delivered
      } else{
        trial = trial + 1
      }
    }
    rm(trial,window,ammend_date,ammend_amount)
    if (round(check_conserved) != round(sum(vaccination_history_DOSE_NUMBER$doses_delivered_this_date))){stop('doses lost!')}
    
    workshop_prev_dose = vaccination_history_DOSE_NUMBER %>% 
      filter(dose == d - 1) %>%
      group_by(iso_code) %>%
      mutate(prev_avaliable = cumsum(doses_delivered_this_date))
    
    workshop_next_dose = vaccination_history_DOSE_NUMBER %>% 
      filter(dose == d) %>%
      group_by(iso_code) %>%
      mutate(next_delivered = cumsum(doses_delivered_this_date)) 
    
    timing_check = workshop_next_dose %>%
      mutate(date = date-vaxCovDelay$delay[vaxCovDelay$dose == (d-1)])  %>%
      select(-dose,-doses_delivered_this_date)
    
    timing_check = workshop_prev_dose %>%
      left_join(timing_check, by = c("date","iso_code")) %>%
      filter(prev_avaliable < next_delivered)
  } #ends when timing_check has no row
}
rm(check_conserved,timing_check,workshop_prev_dose,workshop_next_dose,raw,raw_long,setting_names,booster_start_date)


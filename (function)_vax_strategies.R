
###### Coding vaccine prioritisation strategies

#options(scipen = 100)

#####(1/3) Toggles #############################################################

vax_age_strategy = "oldest"
#options: "oldest", "youngest","50_down","uniform", OTHER?

vax_dose_strategy = 2
#options: 1, 2

vax_strategy_vaccine_type = "AstraZeneca" 
#options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"  
if (vax_strategy_vaccine_type == "Johnson & Johnson"){vax_dose_strategy == 1}

vax_strategy_vaccine_interval = 7*3 #(days) interval between first and second dose

vax_strategy_num_doses = as.integer(10000000)
#COMEBACK - should have % option

vax_strategy_max_expected_cov = 0.8 #between 0-1 % of total age group expected to be vaccinated

vax_strategy_start_date = as.Date('2022-01-20')

vax_strategy_roll_out_speed = 50000 #doses delivered per day
#vax_strategy_roll_out_speed = vax_strategy_num_doses/(6*30) #all doses delivered within 6 months

#_______________________________________________________________________________



#####(2/3) Enact strategy ######################################################
###(A/C) Calculate the eligible population
eligible_pop = data.frame(pop_setting)
colnames(eligible_pop) = c('age_group','eligible_individuals')

#make long by dose
workshop = data.frame()
for (i in 1:num_vax_doses){
  this_dose = as.data.frame(eligible_pop) %>% mutate(dose = i)
  workshop = rbind(workshop,this_dose)
}
eligible_pop= workshop

#remove already vaccinated individuals
existing_coverage = data.frame(eligible_pop$age_group,eligible_pop$dose,rep(0,num_age_groups*num_vax_doses))
colnames(existing_coverage) = c('age_group','dose','cov_to_date')

for (t in 1:num_vax_types){
  for (d in 1:num_vax_doses){
    #need to sum across vaccine_coverage (as this is vaccination_history_FINAL split across age groups)
    existing_coverage$cov_to_date[existing_coverage$dose == d] =
      existing_coverage$cov_to_date[existing_coverage$dose == d] + vaccine_coverage_end_history[(J*(t+(d-1)*T) - J+1):(J*(t+(d-1)*T))]
  }
}
## CHECK - aligns!
# workshop<- eligible_pop %>% left_join(existing_coverage) %>%
#   mutate(eligible_individuals = round(eligible_individuals *(1-cov_to_date))) %>%
#   select(age_group,dose,eligible_individuals)
# 1-aggregate(workshop$eligible_individuals, by=list(workshop$dose), FUN=sum)$x/sum(pop)
# workshop <- vaccination_history_FINAL[vaccination_history_FINAL$date == as.Date('2022-02-22'),]
# aggregate(workshop$coverage_this_date, by=list(workshop$dose), FUN=sum)

#now remove vaccinated, and vaccine hesistant
unreachable = 1-vax_strategy_max_expected_cov

eligible_pop <- eligible_pop %>% left_join(existing_coverage) %>%
  mutate(eligible_individuals = round(eligible_individuals *(1-(cov_to_date+unreachable)))) %>%
  select(age_group,dose,eligible_individuals)

eligible_pop

#NOTE: some with dose 2 > dose 1
#Assume covered by existing vaccine supply and/or never likely to get second dose?
workshop_JJ =  vaccination_history_FINAL$coverage_this_date[vaccination_history_FINAL$date == max(vaccination_history_FINAL$date) & vaccination_history_FINAL$vaccine_type == "Johnson & Johnson"]
diff_dose_one_two  = sum(eligible_pop$eligible_individuals[eligible_pop$dose == 2] - eligible_pop$eligible_individuals[eligible_pop$dose == 1])
  
diff_dose_one_two - workshop_JJ/100* sum(pop)
100*diff_dose_one_two/sum(pop) - workshop_JJ
#<HERE>


###(B/C) Place priority # on age group by strategy
#lots of {} strategy
#want table with columns: age_group, priority
#options: "oldest", "youngest","dose 1", OTHER?
if (vax_age_strategy == "oldest"){
  eligible_pop <- eligible_pop %>%
    mutate(priority= case_when(
      age_group == '60-100' ~ 1,
      age_group == '50-59' ~ 2,
      age_group == '40-49' ~ 3,
      age_group == '30-39' ~ 4,
      age_group == '20-29' ~ 5,
      age_group == '5-19' ~ 6,
      age_group == '0-4' ~ 99 # will restrict priority loop to <10
    ))
} else if (vax_age_strategy == "youngest"){
  eligible_pop <- eligible_pop %>%
    mutate(priority= case_when(
      age_group == '60-100' ~ 6,
      age_group == '50-59' ~ 5,
      age_group == '40-49' ~ 4,
      age_group == '30-39' ~ 3,
      age_group == '20-29' ~ 2,
      age_group == '5-19' ~ 1,
      age_group == '0-4' ~ 99
    ))
} else if (vax_age_strategy == "50_down"){
  eligible_pop <- eligible_pop %>%
    mutate(priority= case_when(
      age_group == '60-100' ~ 6,
      age_group == '50-59' ~ 1,
      age_group == '40-49' ~ 2,
      age_group == '30-39' ~ 3,
      age_group == '20-29' ~ 4,
      age_group == '5-19' ~ 5,
      age_group == '0-4' ~ 99
    ))
} else if (vax_age_strategy == "uniform"){
  eligible_pop <- eligible_pop %>%
    mutate(priority= case_when(
      age_group == '60-100' ~ 1,
      age_group == '50-59' ~ 1,
      age_group == '40-49' ~ 1,
      age_group == '30-39' ~ 1,
      age_group == '20-29' ~ 1,
      age_group == '5-19' ~ 1,
      age_group == '0-4' ~ 99
    ))
} else if (vax_age_strategy == "uniform_transmit"){
  eligible_pop <- eligible_pop %>%
    mutate(priority= case_when(
      age_group == '60-100' ~ 3,
      age_group == '50-59' ~ 2,
      age_group == '40-49' ~ 2,
      age_group == '30-39' ~ 1,
      age_group == '20-29' ~ 1,
      age_group == '5-19' ~ 1,
      age_group == '0-4' ~ 99
    ))
}



###(C/C) Distribute doses by priority
#separate by 1/2 dose strategy
#want table with columns: age_group, priority, dose 1, dose 2
doses_to_deliver = vax_strategy_num_doses
priority_num = 1
eligible_pop <- eligible_pop %>% mutate(doses_delivered = 0)

n=length(unique(eligible_pop$priority))
highest_priority = sort(unique(eligible_pop$priority),partial=n-1)[n-1] #highest valid priority

while (doses_to_deliver>0 & priority_num <= (highest_priority)){
  priority_group = eligible_pop[eligible_pop$priority == priority_num,]
  
  if (length(unique(priority_group$age_group)) == 1){
  
    workshop = doses_to_deliver/vax_dose_strategy - priority_group$eligible_individuals[priority_group$dose == vax_dose_strategy]
      
    if (workshop < 0){
      priority_group$doses_delivered[priority_group$dose == vax_dose_strategy & priority_group$priority == priority_num] = doses_to_deliver/vax_dose_strategy
      if (vax_dose_strategy == 2){priority_group$doses_delivered[priority_group$dose == 1] = doses_to_deliver/vax_dose_strategy }
      doses_to_deliver = 0
    
    } else if (workshop>0){
      priority_group$doses_delivered[priority_group$dose == vax_dose_strategy] =  priority_group$eligible_individuals[priority_group$dose == vax_dose_strategy]
      doses_to_deliver = doses_to_deliver - priority_group$eligible_individuals[priority_group$dose == vax_dose_strategy]
      
      if (vax_dose_strategy == 2){
        workshop2 = doses_to_deliver- priority_group$eligible_individuals[priority_group$dose == 1]
        if(workshop2 < 0){
          priority_group$doses_delivered[priority_group$dose == 1] = doses_to_deliver
          doses_to_deliver = 0
        } else{
          priority_group$doses_delivered[priority_group$dose == 1] = priority_group$eligible_individuals[priority_group$dose == 1]
          doses_to_deliver = doses_to_deliver - priority_group$eligible_individuals[priority_group$dose == 1]
        }
      }
    }
  
  } else {
    workshop = doses_to_deliver/vax_dose_strategy - sum(priority_group$eligible_individuals[priority_group$dose == vax_dose_strategy])
    
    if (workshop < 0){
      prop_to_deliver = doses_to_deliver/vax_dose_strategy / sum(priority_group$eligible_individuals[priority_group$dose == vax_dose_strategy])
      
      priority_group$doses_delivered[priority_group$dose == vax_dose_strategy] = prop_to_deliver * priority_group$eligible_individuals[priority_group$dose == vax_dose_strategy]
      if (vax_dose_strategy == 2){priority_group$doses_delivered[priority_group$dose == 1] = prop_to_deliver * priority_group$eligible_individuals[priority_group$dose == 1] }
      doses_to_deliver = 0
      
    } else if (workshop>0){
      priority_group$doses_delivered[priority_group$dose == vax_dose_strategy] =  priority_group$eligible_individuals[priority_group$dose == vax_dose_strategy]
      doses_to_deliver = doses_to_deliver - sum(priority_group$eligible_individuals[priority_group$dose == vax_dose_strategy])
      
      if (vax_dose_strategy == 2){
        workshop2 = doses_to_deliver- sum(priority_group$eligible_individuals[priority_group$dose == 1])
        if(workshop2 < 0){
          prop_to_deliver = doses_to_deliver / sum(priority_group$eligible_individuals[priority_group$dose == 1])
          priority_group$doses_delivered[priority_group$dose == 1] = doses_to_deliver
          doses_to_deliver = 0
        } else{
          priority_group$doses_delivered[priority_group$dose == 1] = priority_group$eligible_individuals[priority_group$dose == 1]
          doses_to_deliver = doses_to_deliver - sum(priority_group$eligible_individuals[priority_group$dose == 1])
        }
      }
    }
  }
  
  eligible_pop$doses_delivered[eligible_pop$priority == priority_num] = priority_group$doses_delivered
  
  priority_num = priority_num + 1  
}

#_______________________________________________________________________________



#####(3/3) Distribute between days #############################################
# we are going to use:
# (1) vax_strategy_roll_out_speed - max doses delivered per day
# (2) vax_strategy_start_date - first day of doses delivered


VA =  eligible_pop %>% 
  select(age_group,dose,doses_delivered,priority) %>%
  mutate(doses_left = doses_delivered)

vax_strategy_delivery_timeframe = vax_strategy_num_doses/vax_strategy_roll_out_speed #(days)
vax_delivery_outline = data.frame(as.numeric(),as.numeric(),as.character(),as.numeric())
colnames(vax_delivery_outline) = c('day','dose','age_group','doses_delivered')

priority_num = 1
priority_age = as.character(unique(VA$age_group[VA$priority == priority_num]))

daily_avaliable_doses = data.frame(day=1:vax_strategy_delivery_timeframe,
                                   avaliable = vax_strategy_roll_out_speed)
#COMEBACK = need to correct last day so don't overshoot avaliable doses

#for (day in 1:vax_strategy_delivery_timeframe){
for (day in 1){  
  
  avaliable = daily_avaliable_doses$avaliable[daily_avaliable_doses$day == day]
  
  while(avaliable>0){
    
    if(sum(VA$doses_left[VA$priority == priority_num])>0){ 
    #i.e., while we still have doses to deliver in this priority group
    #if(VA$doses_left[VA$priority == priority_num & VA$dose == 1]>0){ 
      
      #Are there some stragglers in this group with dose 1 but not dose 2 coverage?
      #COMEBACK - don't accidentally give extra dose to J&J
      diff_doses = (VA$doses_left[VA$priority == priority_num & VA$dose == 2]-VA$doses_left[VA$priority == priority_num & VA$dose == 1])
      
      #COMEBACK - covering 1st without 2nd first!, should we cover all open age groups, not just priority?
      if(vax_dose_strategy >1 & diff_doses >0){
        vax_delivery_outline = rbind(vax_delivery_outline,
                                     cbind(day = day,
                                           dose = 2,
                                           age_group = priority_age,
                                           doses_delivered = min(diff_doses,avaliable)))
        VA$doses_left[VA$priority == priority_num & VA$dose == 2] =
          VA$doses_left[VA$priority == priority_num & VA$dose == 2] -  min(diff_doses,avaliable)
        
        avaliable = avaliable -  min(diff_doses,avaliable)
      }
      
      ### need two nestled if loops? if doses_left >= max_avaliable, or >=avaliable; if < <
      if(VA$doses_left[VA$priority == priority_num & VA$dose == 1] >= max_avaliable/vax_dose_strategy &
         avaliable >= max_avaliable/vax_dose_strategy){
        #if number to deliver as complete 'dose' strategy > available doses
        #NB: check for dose 1 even when two dose strategy as dose 2 > dose 1 and result in -ve dose one!
        
        vax_delivery_outline = rbind(vax_delivery_outline,
                                     cbind(day = day,
                                           dose = 1,
                                           age_group = priority_age,
                                           doses_delivered = max_avaliable/vax_dose_strategy))
        VA$doses_left[VA$priority == priority_num & VA$dose == 1] =
          VA$doses_left[VA$priority == priority_num & VA$dose == 1] - max_avaliable/vax_dose_strategy
        
        if (vax_dose_strategy == 2){
          vax_delivery_outline = rbind(vax_delivery_outline,
                                       cbind(day = day + vax_strategy_vaccine_interval,
                                             dose = 2,
                                             age_group = priority_age,
                                             doses_delivered = max_avaliable/vax_dose_strategy))
          VA$doses_left[VA$priority == priority_num & VA$dose == 2] =
            VA$doses_left[VA$priority == priority_num & VA$dose == 2] - max_avaliable/vax_dose_strategy
          
          daily_avaliable_doses$avaliable[daily_avaliable_doses$day == day+vax_strategy_vaccine_interval] =
            daily_avaliable_doses$avaliable[daily_avaliable_doses$day == day+vax_strategy_vaccine_interval] - avaliable/vax_dose_strategy
        }
        avaliable = 0
      }
      
      if(VA$doses_left[VA$priority == priority_num & VA$dose == 1] < avaliable/vax_dose_strategy | 
         avaliable < max_avaliable/vax_dose_strategy){
        #if number to deliver in 'complete' schedule < available doses
        
        dose_to_deliver = min(avaliable/vax_dose_strategy,VA$doses_left[VA$priority == priority_num & VA$dose == 1])
        #is this correct??
        
        vax_delivery_outline = rbind(vax_delivery_outline,
                                     cbind(day = day ,
                                           dose = 1,
                                           age_group = priority_age,
                                           doses_delivered = VA$doses_left[VA$priority == priority_num & VA$dose == 1]))
        
        VA$doses_left[VA$priority == priority_num & VA$dose == 1] = 0
        
        avaliable = avaliable -  VA$doses_left[VA$priority == priority_num & VA$dose == 1]
                                    
        if (vax_dose_strategy == 2){
          vax_delivery_outline = rbind(vax_delivery_outline,
                                       cbind(day = day + vax_strategy_vaccine_interval,
                                             dose = 2,
                                             age_group = priority_age,
                                             doses_delivered = VA$doses_left[VA$priority == priority_num & VA$dose == 1]))
                                             #same doses delivered to second dose as to first!
                                             
          VA$doses_left[VA$priority == priority_num & VA$dose == 2] = 0
          
          daily_avaliable_doses$avaliable[daily_avaliable_doses$day == day+vax_strategy_vaccine_interval] =
            daily_avaliable_doses$avaliable[daily_avaliable_doses$day == day+vax_strategy_vaccine_interval] - VA$doses_left[VA$priority == priority_num & VA$dose == 1] 
        }
      }
      
    #} else if(sum(VA$doses_left[VA$priority == priority_num])==0){
    } else if (VA$doses_left[VA$priority == priority_num & VA$dose == 1] ==0){
        priority_num = priority_num+1
        priority_age = as.character(unique(VA$age_group[VA$priority == priority_num]))
    }
  }
  
}

vax_delivery_outline$day = as.numeric(vax_delivery_outline$day)















#_______________________________________________________________________________


###### Coding vaccine prioritisation strategies
#options(scipen = 100) #removes scientific notation

vax_strategy <- function(vax_strategy_start_date,
                         vax_strategy_num_doses,
                         vax_strategy_roll_out_speed,   # doses delivered per day
                         vax_delivery_group = 'universal', #options = 'universal','at_risk','general_public'
                         vax_age_strategy,              # options: "oldest", "youngest","50_down","uniform", OTHER?
                         vax_dose_strategy,             # options: 1,2
                         vax_strategy_vaccine_type,     # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"  
                         vax_strategy_vaccine_interval, # (days) interval between first and second dose
                         vax_strategy_max_expected_cov,  # value between 0-1 (equivalent to %) of age group willing to be vaccinated
                         restriction_date = NA,
                         vax_end_hist   = vaccine_coverage_end_history
                         ){

  
##### Set-up ###################################################################
### TOGGLES - left these hard coded for development
# vax_age_strategy = "uniform"
# vax_dose_strategy = 2
# vax_strategy_vaccine_type = "Pfizer"
# vax_strategy_vaccine_interval = 7*3
# vax_strategy_num_doses = as.integer(6000000)
# vax_strategy_max_expected_cov = 0.8
# vax_strategy_start_date = as.Date('2022-04-20')
# vax_strategy_roll_out_speed = 50000
# #vax_strategy_roll_out_speed = vax_strategy_num_doses/(6*30) #all doses delivered within 6 months
  # vax_strategy_start_date        = vax_strategy_toggles$vax_strategy_start_date
  # vax_strategy_num_doses         = vax_strategy_toggles$vax_strategy_num_doses
  # vax_strategy_roll_out_speed    = vax_strategy_toggles$vax_strategy_roll_out_speed
  # vax_age_strategy               = vax_strategy_toggles$vax_age_strategy
  # vax_dose_strategy              = vax_strategy_toggles$vax_dose_strategy
  # vax_strategy_vaccine_type      = vax_strategy_toggles$vax_strategy_vaccine_type
  # vax_strategy_vaccine_interval  = vax_strategy_toggles$vax_strategy_vaccine_interval
  # vax_strategy_max_expected_cov  = vax_strategy_toggles$vax_strategy_max_expected_cov
  
  
### WARNINGS 
if (vax_strategy_start_date <= max(vaccination_history_TRUE$date)){ 
  stop ('Your hypothetical vaccine campaign start date needs to be in the future!')
}
# if (vax_strategy_start_date <= date_start){ 
#   stop ('Your hypothetical vaccine campaign start date needs to be after the start date (unless you want to do more coding)')
# }
if (!(vax_strategy_vaccine_type %in% c("Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"))){
  stop('pick a valid vaccine type, or check your spelling!')
}
if (vax_delivery_group != "universal" & num_risk_groups == 1){
  warning("You need a risk group to have a risk strategy! We have overwritten vax_delivery_group = 'universal'")
  vax_delivery_group = 'universal'
}
  

### IMPORTS
prioritisation_csv <- read.csv("1_inputs/prioritisation.csv",header=TRUE)

if (vax_delivery_group %in% c('universal','general_public')){ this_risk_group = 'general_public'
} else if (vax_delivery_group == 'at_risk'){ this_risk_group = risk_group_name}


### IS THIS A BOOSTER?
booster_dose = "N"
if (vax_dose_strategy == 3){booster_dose = "Y"}
if (vax_dose_strategy == 2 & vax_strategy_vaccine_type == "Johnson & Johnson"){booster_dose = "Y"}
#_______________________________________________________________________________



#####(1/?) Calculate the eligible population ###################################
if (vax_delivery_group == 'universal'){
  eligible_pop = data.frame(pop_setting)
  colnames(eligible_pop) = c('age_group','eligible_individuals')
} else if (vax_delivery_group == 'at_risk'){
  eligible_pop = pop_risk_group_dn[pop_risk_group_dn == risk_group_name,] %>% select(age_group,pop)
  colnames(eligible_pop) = c('age_group','eligible_individuals')
} else if (vax_delivery_group == 'general_public'){
  eligible_pop = pop_risk_group_dn[pop_risk_group_dn == 'general_public',] %>% select(age_group,pop)
  colnames(eligible_pop) = c('age_group','eligible_individuals')
}


#make long by dose
workshop = data.frame()
for (i in 1:num_vax_doses){
  this_dose = as.data.frame(eligible_pop) %>% mutate(dose = i)
  workshop = rbind(workshop,this_dose)
}
eligible_pop= workshop

#remove already vaccinated individuals
existing_coverage = crossing(dose = c(1:num_vax_doses),
                             age_group = age_group_labels,
                             cov_to_date = 0)

for (d in 1:num_vax_doses){
  for (i in 1:num_age_groups){
    #need to sum across vaccine_coverage (as this is vaccination_history_POP split across age groups)
    existing_coverage$cov_to_date[existing_coverage$dose == d & 
                                    existing_coverage$age_group == age_group_labels[i]] = 
      sum(vax_end_hist$coverage_this_date[vax_end_hist$risk_group == this_risk_group &
                                                                 vax_end_hist$dose == d & 
                                                                 vax_end_hist$age_group == age_group_labels[i]])
  }
} 

#COMEBACK RISK - existing coverage is in % age group, may need to make risk specific


## CHECK - aligns!
# workshop<- eligible_pop %>% left_join(existing_coverage) %>%
#   mutate(eligible_individuals = round(eligible_individuals *(1-cov_to_date))) %>%
#   select(age_group,dose,eligible_individuals)
# 1-aggregate(workshop$eligible_individuals, by=list(workshop$dose), FUN=sum)$x/sum(pop)
# workshop <- vaccination_history_POP[vaccination_history_POP$date == as.Date('2022-02-22'),]
# aggregate(workshop$coverage_this_date, by=list(workshop$dose), FUN=sum)

#now remove vaccinated, and vaccine hesistant
unreachable = 1-vax_strategy_max_expected_cov
#COMEBACK RISK - vaccine hesitancy may differ in the at risk group

eligible_pop <- eligible_pop %>% left_join(existing_coverage) %>%
  mutate(eligible_individuals = round(eligible_individuals *(1-(cov_to_date+unreachable)))) %>%
  select(age_group,dose,eligible_individuals)

#NOTE: some with dose 2 > dose 1
#Assume covered by existing vaccine supply and/or never likely to get second dose?
# if ("Johnson & Johnson" %in% vax_type_list ){
#   workshop_JJ =  vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & 
#                                                                 vaccination_history_POP$vaccine_type == "Johnson & Johnson"]
# } else {workshop_JJ=0 }
# diff_dose_one_two  = sum(eligible_pop$eligible_individuals[eligible_pop$dose == 2] - eligible_pop$eligible_individuals[eligible_pop$dose == 1])
#   
# diff_dose_one_two - workshop_JJ/100* sum(pop)
# 100*diff_dose_one_two/sum(pop) - workshop_JJ
# COMEBACK / ASSUMPTIOn: people with dose 1 will either be addressed by existing schema, or never followed up

eligible_pop$eligible_individuals[eligible_pop$dose == 2] = eligible_pop$eligible_individuals[eligible_pop$dose == 1] 
#_______________________________________________________________________________



#####(2/?) Place priority # on age group by strategy ############################
if (vax_age_strategy %in% unique(prioritisation_csv$strategy)) {
  elected_strategy = prioritisation_csv[prioritisation_csv$strategy == vax_age_strategy,c('age_group','priority')]
  eligible_pop <- eligible_pop %>% left_join(elected_strategy)
} else if (vax_age_strategy == "manual_overwrite"){
  eligible_pop <- eligible_pop %>%
    mutate(priority= case_when(
      age_group == '70 to 100' ~ 6,
      age_group == '60 to 69' ~ 5,
      age_group == '45 to 59' ~ 4,
      age_group == '30 to 44' ~ 3,
      age_group == '18 to 29' ~ 2,
      age_group == '5 to 17' ~ 1,
      age_group == '0 to 4' ~ 99
    ))
}
#Note: 99 used as a 'never' indicator
#_______________________________________________________________________________



#####(3/?)  Distribute doses by priority #######################################
doses_to_deliver = vax_strategy_num_doses
priority_num = 1
eligible_pop <- eligible_pop %>% mutate(doses_delivered = 0)

n=length(unique(eligible_pop$priority))
highest_priority = sort(unique(eligible_pop$priority),partial=n-1)[n-1] #highest valid priority

while (doses_to_deliver>0 & priority_num <= (highest_priority)){
  priority_group = eligible_pop[eligible_pop$priority == priority_num,]
  
  workshop = doses_to_deliver/vax_dose_strategy - sum(priority_group$eligible_individuals[priority_group$dose == 1]) #check enough for all priority groups
    
  if (workshop < 0){
    prop_to_deliver = doses_to_deliver/vax_dose_strategy / sum(priority_group$eligible_individuals[priority_group$dose == 1])
    
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
  
  eligible_pop$doses_delivered[eligible_pop$priority == priority_num] = priority_group$doses_delivered
  
  priority_num = priority_num + 1  
}

#_______________________________________________________________________________



#####(4/?) Distribute between days #############################################
# we should use:
# (1) vax_strategy_num_doses - doses to deliver
# (2) vax_strategy_roll_out_speed - max doses delivered per day
# (3) vax_strategy_start_date - first day of doses delivered

VA =  eligible_pop %>% mutate(doses_left = doses_delivered)
priority_num = 1
priority_group  = as.character(unique(VA$age_group[VA$priority == priority_num]))

if (vax_dose_strategy == 1){
  ceiling = min(sum(eligible_pop$doses_delivered),vax_strategy_num_doses) #max delivery (either limited by eligible individuals, or available doses!)
  timeframe = ceiling/vax_strategy_roll_out_speed
  daily_per_dose = vax_strategy_roll_out_speed
} else if (vax_dose_strategy == 2){
  #COMEBACK - do we need a ceiling here too?
  if(vax_strategy_num_doses/(vax_strategy_roll_out_speed*2)<vax_strategy_vaccine_interval){
    timeframe = vax_strategy_num_doses/(vax_strategy_roll_out_speed*2)
    daily_per_dose = vax_strategy_roll_out_speed
  } else{
    timeframe = vax_strategy_num_doses/(vax_strategy_roll_out_speed)
    daily_per_dose = vax_strategy_roll_out_speed/2
  }
}
timeframe = round(timeframe)
if (is.na(restriction_date) == FALSE){
  timeframe = as.numeric(restriction_date - vax_strategy_start_date) + 1
}

length_track = timeframe
if (vax_dose_strategy == 2){length_track=length_track+vax_strategy_vaccine_interval}

vax_delivery_outline <- crossing(day = c(1:length_track),
                                 dose = c(1:num_vax_doses),
                                 age_group = age_group_labels,
                                 doses_delivered = c(0))

#for (day in 1:229){
for (day in 1:timeframe){
  avaliable = daily_per_dose
  #ensuring that we don't overshoot available doses
  if (day == timeframe){avaliable = vax_strategy_num_doses/vax_dose_strategy-(timeframe-1)*daily_per_dose}
  if (avaliable > sum(VA$doses_left)){avaliable = sum(VA$doses_left)}
  
  while(avaliable>0 & priority_num <= highest_priority){
    
    if(sum(VA$doses_left[VA$priority == priority_num])>0){ 
      #i.e., while we still have doses to deliver in this priority group
      
      if(0 %in% VA$doses_left[VA$priority == priority_num & VA$dose == 1]){
        age_complete = VA$age_group[VA$doses_left == 0 & VA$priority == priority_num & VA$dose == 1]
        VA$priority[VA$age_group %in% age_complete] = VA$priority[VA$age_group %in% age_complete] + 100
        
        priority_group = as.character(unique(VA$age_group[VA$priority == priority_num]))
      } #FIX - when one age group in the priority group runs out first
      
      #ISSUE HERE
      workshop_doses = min(sum(VA$doses_left[VA$priority == priority_num & VA$dose == 1]),
                           daily_per_dose)
      #either deliver max capacity or number left in this group, whichever is fewer
      
      leftover=0
      
      for (i in length(priority_group):1){
        workshop_age = priority_group[i]
        workshop_prop = sum(VA$doses_left[VA$age_group == workshop_age])/sum(VA$doses_left[VA$priority == priority_num])
        workshop_calc = workshop_doses * workshop_prop + leftover
        
        if (workshop_calc > VA$doses_left[VA$age_group == workshop_age & VA$dose == 1]){
          leftover = abs(workshop_calc - VA$doses_left[VA$age_group == workshop_age & VA$dose == 1])
          workshop_calc = VA$doses_left[VA$age_group == workshop_age & VA$dose == 1]
          
        } else{
          leftover = 0
        }
        vax_delivery_outline$doses_delivered[vax_delivery_outline$day == day &
                                               vax_delivery_outline$dose == 1 &
                                               vax_delivery_outline$age_group == workshop_age] = workshop_calc
        VA$doses_left[VA$age_group == workshop_age & VA$dose == 1] = VA$doses_left[VA$age_group == workshop_age & VA$dose == 1] - workshop_calc
        
        if (vax_dose_strategy == 2){
          vax_delivery_outline$doses_delivered[vax_delivery_outline$day == day + vax_strategy_vaccine_interval &
                                                 vax_delivery_outline$dose == 2 &
                                                 vax_delivery_outline$age_group == workshop_age] = workshop_calc
          VA$doses_left[VA$age_group == workshop_age & VA$dose == 2] = VA$doses_left[VA$age_group == workshop_age & VA$dose == 2] - workshop_calc
        }
        
      }
      
      avaliable = avaliable - workshop_doses
      
      
    } else if (sum(VA$doses_left[VA$priority == priority_num])==0){
      priority_num = priority_num+1
      priority_group = as.character(unique(VA$age_group[VA$priority == priority_num]))
    } else{
      stop('negative doses left, reconsider!')
    }
  } #<end while loop>
  
}

### formating vax_delivery_outline to align with vaccination_history_TRUE
vax_delivery_outline$date = vax_strategy_start_date + (vax_delivery_outline$day-1)
vax_delivery_outline$vaccine_type = vax_strategy_vaccine_type
if (vax_strategy_vaccine_type %in% c("Moderna","Pfizer")){
  vax_delivery_outline$vaccine_mode = 'mRNA'
} else if (vax_strategy_vaccine_type %in% c("AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac")){
  vax_delivery_outline$vaccine_mode = 'viral'
}

vax_delivery_outline$coverage_this_date = NA #shouldn't be used anyway
names(vax_delivery_outline)[names(vax_delivery_outline) == 'doses_delivered'] <-'doses_delivered_this_date'

vax_delivery_outline = vax_delivery_outline %>% 
  select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date,age_group)
#CHECK - aggregate(vax_delivery_outline$doses_delivered_this_date,by=list(vax_delivery_outline$age_group),FUN=sum) - aligns with eligible_pop

### adding to end of vaccination_history_TRUE
#do we need zero rows?
if(vax_delivery_group != 'universal'){
  return(vax_delivery_outline)
} #HERE return as vaccination_history if 'universal', but as extra lines + timeframe if at risk

vax_delivery_outline$risk_group = this_risk_group

vaccination_history_MODF = rbind(vaccination_history_TRUE,vax_delivery_outline)

return(vaccination_history_MODF)

}


# vax_delivery_outline =
#   vax_strategy(vax_strategy_start_date                  = as.Date('2022-04-20'),
#                vax_strategy_num_doses         = as.integer(1000000),
#                vax_strategy_roll_out_speed    = 50000 ,               # doses delivered per day
#                vax_age_strategy               = "oldest",            # options: "oldest", "youngest","50_down","uniform", OTHER?
#                vax_dose_strategy              = 2,                    # options: 1,2
#                vax_strategy_vaccine_type      = "Pfizer" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
#                vax_strategy_vaccine_interval  = 7*3 ,                 # (days) interval between first and second dose
#                vax_strategy_max_expected_cov  = 0.8                   # value between 0-1 (equivalent to %) of age group willing to be vaccinated
#   )


# ### CHECK 
#aggregate(vax_delivery_outline$doses_delivered_this_date, by=list(category=vax_delivery_outline$dose,vax_delivery_outline$age_group), FUN=sum)









#_______________________________________________________________________________

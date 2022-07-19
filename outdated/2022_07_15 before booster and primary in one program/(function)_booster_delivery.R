### This function delivers booster doses to individuals who have ALREADY recieved their primary schedule
#options(warn = 0) # = 2 when debugging

booster_delivery <- function(vax_strategy_start_date,
                         vax_strategy_roll_out_speed,      # doses delivered per day
                         vax_strategy_num_doses,           # dose left to deliver
                         vax_delivery_group = 'universal', # options = 'universal','at_risk','general_public'
                         vax_age_strategy,                 # options: "oldest", "youngest","50_down","uniform", OTHER?
                         booster_dose_type,
                         booster_dose_number,
                         booster_dose_interval,
                         vax_end_hist   = vaccine_coverage_end_history
){
  
  
  ##### Set-up ###################################################################
  ### TOGGLES - left these hard coded for development
  # vax_strategy_start_date        = vax_strategy_toggles$vax_strategy_start_date
  # vax_strategy_roll_out_speed    = vax_strategy_toggles$vax_strategy_roll_out_speed
  # vax_age_strategy               = vax_strategy_toggles$vax_age_strategy
  # vax_strategy_max_expected_cov  = vax_strategy_toggles$vax_strategy_max_expected_cov
  # vax_delivery_group = 'at_risk'
  # booster_dose_type = "Johnson & Johnson"
  # booster_dose_number = 2
  # booster_dose_interval = 30*3
  
  ### WARNINGS 
  if (vax_strategy_start_date <= max(vaccination_history_TRUE$date)){ 
    stop ('Your hypothetical vaccine campaign start date needs to be in the future!')
  }
  if (!(booster_dose_type %in% c("Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"))){
    stop('pick a valid vaccine type, or check your spelling!')
  }
  if (booster_dose_number == 1){stop('A first dose is not a booster dose! Ensure booster_dose_number > 1')}

  ### IMPORTS
  prioritisation_csv <- read.csv("1_inputs/prioritisation.csv",header=TRUE)
  
  if (vax_delivery_group %in% c('universal','general_public')){ this_risk_group = 'general_public'
  } else if (vax_delivery_group == 'at_risk'){ this_risk_group = risk_group_name}
  
  booster_dose_interval = round(booster_dose_interval)
  #_______________________________________________________________________________
  
  
  
  #####(1/?) Calculate the eligible population ###################################
  if (vax_delivery_group == 'universal'){ eligible_pop = data.frame(pop_setting)
  } else if (vax_delivery_group == 'at_risk'){ eligible_pop = pop_risk_group_dn[pop_risk_group_dn == risk_group_name,] %>% select(age_group,pop)
  } else if (vax_delivery_group == 'general_public'){ eligible_pop = pop_risk_group_dn[pop_risk_group_dn == 'general_public',] %>% select(age_group,pop)}
  colnames(eligible_pop) = c('age_group','eligible_individuals') 
  
  #select already vaccinated individuals
  #ASSUMPTION / COMEBACK - including those with only 1 primary dose
  existing_coverage = crossing(dose = unique(vaccination_history_TRUE$dose),
                               vaccine_type = unique(vaccination_history_TRUE$vaccine_type),
                               age_group = age_group_labels,
                               cov_to_date = 0)
  
  for (t in 1:length(unique(vaccination_history_TRUE$vaccine_type))){
    for (d in 1:length(unique(vaccination_history_TRUE$dose))){
      for (i in 1:num_age_groups){
        #need to sum across vaccine_coverage (as this is vaccination_history_POP split across age groups)
        existing_coverage$cov_to_date[existing_coverage$dose == d & 
                                        existing_coverage$age_group == age_group_labels[i] &
                                        existing_coverage$vaccine_type == unique(vaccination_history_TRUE$vaccine_type)[t]] = 
          vax_end_hist$coverage_this_date[vax_end_hist$risk_group == this_risk_group &
                                                vax_end_hist$dose == d & 
                                                vax_end_hist$age_group == age_group_labels[i]&
                                                vax_end_hist$vaccine_type == unique(vaccination_history_TRUE$vaccine_type)[t]]
      }
    } 
  }

  eligible_pop <- eligible_pop %>% left_join(existing_coverage) %>%
    mutate(eligible_individuals = round(eligible_individuals *cov_to_date)) %>%
    select(age_group,dose,vaccine_type,eligible_individuals)
  
  eligible_pop$eligible_individuals[eligible_pop$dose == 1] = eligible_pop$eligible_individuals[eligible_pop$dose == 1] - eligible_pop$eligible_individuals[eligible_pop$dose == 2]
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
  
  ceiling = min(sum(eligible_pop$doses_delivered),vax_strategy_num_doses)
  timeframe = ceiling/vax_strategy_roll_out_speed
  daily_per_dose = vax_strategy_roll_out_speed

  timeframe = ceiling(timeframe)

  booster_delivery_outline <- crossing(day = c(1:timeframe),
                                   dose = unique(eligible_pop$dose), 
                                   vaccine_type = unique(eligible_pop$vaccine_type),
                                   age_group = age_group_labels,
                                   doses_delivered = c(0))
  
  for (day in 1:timeframe){
    #for (day in 1:14){
    #for (day in 1:(timeframe-1)){
    avaliable = daily_per_dose
    daily_per_dose_here = daily_per_dose

    #ensuring that we don't overshoot available doses
    if (day == timeframe){
      workshop_leftover = sum(booster_delivery_outline$doses_delivered)
      avaliable = min(vax_strategy_num_doses-workshop_leftover,daily_per_dose)
      #CHECK
      if(workshop_leftover != (timeframe-1)*daily_per_dose){stop('ERROR line 185 of booster delivery function')}
    }
    if (avaliable > sum(VA$doses_left)){avaliable = sum(VA$doses_left)}
    
    while(avaliable>0 & priority_num <= highest_priority){
      
      if(sum(VA$doses_left[VA$priority == priority_num])>0){ 
        #i.e., while we still have doses to deliver in this priority group
        
        if(0 %in% VA$doses_left[VA$priority == priority_num & VA$dose == 1]){
          age_complete = VA$age_group[VA$doses_left == 0 & VA$priority == priority_num & VA$dose == 1]
          VA$priority[VA$age_group %in% age_complete] = VA$priority[VA$age_group %in% age_complete] + 100
          
          priority_group = as.character(unique(VA$age_group[VA$priority == priority_num]))
        } #FIX - when one age group in the priority group runs out first
        
        workshop_doses = min(sum(VA$doses_left[VA$priority == priority_num]),
                             daily_per_dose_here)
        #either deliver max capacity or number left in this group, whichever is fewer
        
        leftover=0
        VA_pt = VA #snapshot
        
        for (i in length(priority_group):1){
          for (d in unique(VA$dose)){
            for (t in 1:length(unique(VA$vaccine_type))){
              
              workshop_age = priority_group[i]
              workshop_type = unique(VA$vaccine_type)[t]
              
              workshop_prop = VA_pt$doses_left[VA_pt$age_group == workshop_age & VA_pt$dose == d & VA_pt$vaccine_type == workshop_type]/
                sum(VA_pt$doses_left[VA_pt$priority == priority_num])
              workshop_calc = workshop_doses * workshop_prop + leftover
              
              if (workshop_calc > VA$doses_left[VA$age_group == workshop_age & VA$dose == d & VA$vaccine_type == workshop_type]){
                leftover = abs(workshop_calc - VA$doses_left[VA$age_group == workshop_age & VA$dose == d & VA$vaccine_type == workshop_type])
                workshop_calc = VA$doses_left[VA$age_group == workshop_age & VA$dose == d & VA$vaccine_type == workshop_type]
                
              } else{
                leftover = 0
              }
              booster_delivery_outline$doses_delivered[booster_delivery_outline$day == day &
                                                         booster_delivery_outline$dose == d &
                                                         booster_delivery_outline$vaccine_type == workshop_type &
                                                         booster_delivery_outline$age_group == workshop_age] = workshop_calc
              VA$doses_left[VA$age_group == workshop_age & VA$dose == d & VA$vaccine_type == workshop_type] = 
                VA$doses_left[VA$age_group == workshop_age & VA$dose == d & VA$vaccine_type == workshop_type] - workshop_calc

            }
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
  
  ### formating booster_delivery_outline to align with vaccination_history_TRUE
  booster_delivery_outline$date = vax_strategy_start_date + (booster_delivery_outline$day-1)
  
  booster_delivery_outline = booster_delivery_outline %>%
    rename(FROM_vaccine_type = vaccine_type,
           FROM_dose = dose) %>%
    mutate(vaccine_type = booster_dose_type,
           dose = 8)
  
  if (booster_dose_type %in% c("Moderna","Pfizer")){
    booster_delivery_outline$vaccine_mode = 'mRNA'
  } else if (booster_dose_type %in% c("AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac")){
    booster_delivery_outline$vaccine_mode = 'viral'
  }
  
  booster_delivery_outline$coverage_this_date = NA #shouldn't be used anyway
  names(booster_delivery_outline)[names(booster_delivery_outline) == 'doses_delivered'] <-'doses_delivered_this_date'
  
  booster_delivery_outline = booster_delivery_outline %>% 
    select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date,age_group,FROM_dose,FROM_vaccine_type)
  
  #CHECK 
  workshop = aggregate(booster_delivery_outline$doses_delivered_this_date,by=list(booster_delivery_outline$age_group),FUN=sum)
  colnames(workshop) = c('age_group','doses')
  
  if (round(sum(workshop$doses)) != round(sum(eligible_pop$doses_delivered))) { #if not all doses delivered
    if (is.na(restriction_date) == TRUE){ #if no restriction date -> error
      stop('error line 350 of vax strategies function')
    } else{ #else if restriction date
      if (round(as.numeric(restriction_date - vax_strategy_start_date +1) * vax_strategy_roll_out_speed) == round(sum(workshop$doses))){
        #if restriction date causing non delivery of doses
      } else{  
        stop('error line 359 of vax strategies function')
      }
    } 
  }
  
  ggplot(booster_delivery_outline) + geom_point(aes(x=date,y=doses_delivered_this_date,color=as.factor(age_group),shape=as.factor(FROM_vaccine_type)))
  
  ### adding to end of vaccination_history_TRUE
  #do we need zero rows?
  if(vax_delivery_group != 'universal'){
    return(booster_delivery_outline)
  } #HERE return as vaccination_history if 'universal', but as extra lines + timeframe if at risk
  
  booster_delivery_outline$risk_group = this_risk_group
  
  vaccination_history_MODF = rbind(vaccination_history_TRUE,booster_delivery_outline)
  
  return(vaccination_history_MODF)
  
}


# booster_delivery_outline =
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
#aggregate(booster_delivery_outline$doses_delivered_this_date, by=list(category=booster_delivery_outline$dose,booster_delivery_outline$age_group), FUN=sum)









#_______________________________________________________________________________

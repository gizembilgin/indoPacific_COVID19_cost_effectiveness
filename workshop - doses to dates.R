#####(4/?) Distribute between days #############################################
# we should use:
# (1) vax_strategy_num_doses - doses to deliver
# (2) vax_strategy_roll_out_speed - max doses delivered per day
# (3) vax_strategy_start_date - first day of doses delivered

VA =  eligible_pop %>% mutate(doses_left = doses_delivered)
priority_num = 1
priority_group  = as.character(unique(VA$age_group[VA$priority == priority_num]))

if (vax_dose_strategy == 1){
  timeframe = vax_strategy_num_doses/vax_strategy_roll_out_speed
  daily_per_dose = vax_strategy_roll_out_speed
} else if (vax_dose_strategy == 2){
  if(vax_strategy_num_doses/(vax_strategy_roll_out_speed*2)<vax_strategy_vaccine_interval){
    timeframe = vax_strategy_num_doses/(vax_strategy_roll_out_speed*2)
    daily_per_dose = vax_strategy_roll_out_speed
  } else{
    timeframe = vax_strategy_num_doses/(vax_strategy_roll_out_speed)
    daily_per_dose = vax_strategy_roll_out_speed/2
  }
}

length_track = timeframe
if (vax_dose_strategy == 2){length_track=length_track+vax_strategy_vaccine_interval}

vax_delivery_outline <- crossing(day = c(1:length_track),
                               dose = c(1:num_vax_doses),
                               age_group = age_group_labels,
                               doses_delivered = c(0))
vax_delivery_outline <- vax_delivery_outline %>% mutate(age_group = gsub('-',' to ',age_group))

for (day in 1:timeframe){

  avaliable = daily_per_dose
  #ensuring that last day doesn't overshoot available doses
  if (day == timeframe){
    avaliable = vax_strategy_num_doses/vax_dose_strategy-(timeframe-1)*daily_per_dose
  }
  
  while(avaliable>0 & priority_num <= highest_priority){
    
    if(sum(VA$doses_left[VA$priority == priority_num])>0){ 
      #i.e., while we still have doses to deliver in this priority group
      
      #ISSUE HERE
      workshop_doses = min(sum(VA$doses_left[VA$priority == priority_num & VA$dose == 1]),
                     daily_per_dose)
      #either deliver max capacity or number left in this group, whichever is fewer
      
      for (i in 1:length(priority_group)){
        workshop_age = priority_group[i]
        workshop_prop = sum(VA$doses_left[VA$age_group == workshop_age])/sum(VA$doses_left[VA$priority == priority_num])
        
        vax_delivery_outline$doses_delivered[vax_delivery_outline$day == day &
                                             vax_delivery_outline$dose == 1 &
                                             vax_delivery_outline$age_group == workshop_age] = workshop_doses * workshop_prop
        VA$doses_left[VA$age_group == workshop_age & VA$dose == 1] = VA$doses_left[VA$age_group == workshop_age & VA$dose == 1] - workshop_doses * workshop_prop
        
        if (vax_dose_strategy == 2){
          vax_delivery_outline$doses_delivered[vax_delivery_outline$day == day + vax_strategy_vaccine_interval &
                                                 vax_delivery_outline$dose == 2 &
                                                 vax_delivery_outline$age_group == workshop_age] = workshop_doses * workshop_prop
          VA$doses_left[VA$age_group == workshop_age & VA$dose == 2] = VA$doses_left[VA$age_group == workshop_age & VA$dose == 2] - workshop_doses * workshop_prop
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



### CHECK 
aggregate(vax_delivery_outline$doses_delivered, by=list(category=vax_delivery_outline$dose,vax_delivery_outline$age_group), FUN=sum)

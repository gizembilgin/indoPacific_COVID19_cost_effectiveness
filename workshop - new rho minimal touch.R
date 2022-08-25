
  
  #create data set of historical cases by days in recovery class
  if (nrow(incidence_log)>0){
    workshop = incidence_log %>% select(date,daily_cases)
    workshop = rbind(workshop,hist_cases)
    
  } else{
    workshop = hist_cases
  }
  #______________________
  
  #calculate proportion of individuals in recovery class by days since recovery
  workshop = workshop %>%
    mutate(date = date + round(AverageSymptomaticPeriod)) %>%
    filter(date <= date_now & date > (date_now - 1/omega)) %>%
    mutate(days = round(as.numeric(date_now - date)))
  workshop = workshop %>% mutate(prop_window = daily_cases/sum(workshop$daily_cases))   
  if (round(sum(workshop$prop_window),digits=5) != 1){stop('error in rho_time_step')}
  
  ggplot(workshop) + geom_line(aes(date,prop_window)) 
  #______________________
  
  if (length(variant_change_date[variant_change_date<date_now])>0){
    latest_variant_introduction = max(variant_change_date[variant_change_date<date_now])
    latest_variant_shift = synthetic_strain_shift %>% 
      select(days,percentage) %>%
      mutate(date = days + latest_variant_introduction)
    
    #rho = how protected are people today from the circulating strains today?
    #    = %new circulating today * protection to new today + %old circulating today * protection to old today
    
    if (which(variant_change_date == latest_variant_introduction) == 1){
      protection_to_old = workshop %>% 
        left_join(rho_acq[rho_acq$type == 'new_to_new',], by = "days") %>% #COMEBACK and remove when rho dn by days fit
        mutate(interim = protection * prop_window)
      protection_to_old = sum(protection_to_old$interim,na.rm=TRUE)
      
      
    } else if (which(variant_change_date == latest_variant_introduction) > 1){
      prev_num = which(variant_change_date == latest_variant_introduction) - 1
      prev_variant_introduction = variant_change_date[prev_num]
      
      prev_variant_shift = synthetic_strain_shift %>% 
        select(days,percentage) %>%
        mutate(date = days + prev_variant_introduction)
      
      if (which(variant_change_date == latest_variant_introduction) == 2){
        protection_to_oldest = workshop %>% 
          mutate(month = ceiling(days / 30)) %>%
          left_join(rho_acq[rho_acq$type == 'new_to_new',], by = "days") %>% #COMEBACK and remove when rho dn by days fit
          mutate(interim = protection * prop_window)
        protection_to_oldest = sum(protection_to_oldest$interim,na.rm=TRUE)
        
      } else if (which(variant_change_date == latest_variant_introduction) > 2){
        
        prev2_num = which(variant_change_date == latest_variant_introduction) - 2
        prev2_variant_introduction = variant_change_date[prev2_num]
        
        prev2_variant_shift = synthetic_strain_shift %>% 
          select(days,percentage) %>%
          mutate(date = days + prev2_variant_introduction)
        
        protection_to_oldest2 = workshop %>% 
          mutate(month = ceiling(days / 30)) %>%
          left_join(rho_acq[rho_acq$type == 'new_to_new',], by = "days") %>% #COMEBACK and remove when rho dn by days fit
          mutate(interim = protection * prop_window)
        protection_to_oldest2 = sum(protection_to_oldest2$interim,na.rm=TRUE)
        
        protection_to_recentOld2 = workshop %>% 
          left_join(rho_dn_wide, by = "days") %>% #COMEBACK and remove when rho dn by days fit
          left_join(prev2_variant_shift,by="date") %>%
          mutate(interim = case_when(
            date < prev2_variant_introduction ~ prev_to_new * prop_window,
            date >= prev2_variant_introduction & date <= max(prev2_variant_shift$date)~(prev_to_new*(1-percentage)+new_to_new*percentage)*prop_window,
            date > max(prev2_variant_shift$date) ~ new_to_new*prop_window
          ))
        protection_to_recentOld2 = sum(protection_to_recentOld2$interim,na.rm=TRUE)
        
        percentage_old = prev2_variant_shift$percentage[prev2_variant_shift$date == max(prev2_variant_shift$date)]
        protection_to_oldest =  protection_to_recentOld2 * percentage_old + (1-percentage_old) * protection_to_oldest2
        
      }
      
      protection_to_recentOld = workshop %>% 
        mutate(month = ceiling(days / 30)) %>%
        left_join(rho_dn_wide, by = "days") %>% #COMEBACK and remove when rho dn by days fit
        left_join(prev_variant_shift,by="date") %>%
        mutate(interim = case_when(
          date < prev_variant_introduction ~ prev_to_new * prop_window,
          date >= prev_variant_introduction & date <= max(prev_variant_shift$date)~(prev_to_new*(1-percentage)+new_to_new*percentage)*prop_window,
          date > max(prev_variant_shift$date) ~ new_to_new*prop_window
        ))
      protection_to_recentOld = sum(protection_to_recentOld$interim,na.rm=TRUE)
      
      percentage_old = prev_variant_shift$percentage[prev_variant_shift$date == max(prev_variant_shift$date)]
      protection_to_old =  protection_to_recentOld * percentage_old + (1-percentage_old) * protection_to_oldest
      
    }
    
    protection_to_new = workshop %>% 
      mutate(month = ceiling(days / 30)) %>%
      left_join(rho_dn_wide, by = "days") %>% #COMEBACK and remove when rho dn by days fit
      left_join(latest_variant_shift,by="date") %>%
      mutate(interim = case_when(
        date < latest_variant_introduction ~ prev_to_new * prop_window,
        date >= latest_variant_introduction & date <= max(latest_variant_shift$date)~(prev_to_new*(1-percentage)+new_to_new*percentage)*prop_window,
        date > max(latest_variant_shift$date) ~ new_to_new*prop_window
      ))
    protection_to_new = sum(protection_to_new$interim,na.rm=TRUE)
    
    if (date_now %in% latest_variant_shift$date){
      percentage_new = latest_variant_shift$percentage[latest_variant_shift$date == date_now]
    } else{
      percentage_new = latest_variant_shift$percentage[latest_variant_shift$date == max(latest_variant_shift$date)]
    }
    
    
    function_result = percentage_new * protection_to_new + (1-percentage_new) * protection_to_old
    
    
    
    
  } else{
    workshop = workshop %>% 
      mutate(month = ceiling(days / 30)) %>%
      left_join(rho_acq[rho_acq$type == 'new_to_new',], by = "days") %>% #COMEBACK and remove when rho dn by days fit
      mutate(interim = protection * prop_window)
    
    function_result = sum(workshop$interim,na.rm=TRUE) #COMEBACK - remove na.rm
  }
  
  
  
  
 function_result

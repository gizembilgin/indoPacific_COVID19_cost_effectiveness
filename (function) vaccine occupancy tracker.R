
# Distributon of time since vaccination per dose for use in vaccine effectiveness calculation
# This will branch in two places:
# BRANCH ONE = {removing primary dose i + 1 from primary dose i | removing booster dose from primary dose D}
# BRANCH TWO = {JHU data | hypothetical vaccines strategy data}

vaccine_occupancy_tracker = data.frame()

if (date_now == date_start){JHU_lower_doses = data.frame()}

# BRANCH ONE {removing primary dose i + 1 from primary dose i} ##################################################################
for (d in D:1){
  if (d == D){
    #nothing can take away from the highest dose
    workshop = vaccination_history_FINAL %>% 
      filter(dose == d &
               doses_delivered_this_date > 0 &
               date < date_now - vaxCovDelay$delay[vaxCovDelay$dose == d])
    vaccine_occupancy_tracker = bind_rows(vaccine_occupancy_tracker, workshop)
    
  } else{

    #people 'graduate' out of doses lower than D, and should be removed before VE calculation
    #we have to split between known exact intervals (our artificial strategy) and real doses to date (JHU data, delayed uploading)
    cutoff = max(vaccination_history_TRUE$date)
    
    
    ### BRANCH TWO {JHU data} ####################################################################################################
    if (date_now == date_start){ #run once per model run to save time
      workshop = vaccination_history_FINAL %>% 
        filter(dose == d &
                 date < date_now - vaxCovDelay$delay[vaxCovDelay$dose == d] & # doses delivered and 'active' by this date
                 date <= cutoff)                                              # doses from JHU
      
      suppressor = vaccination_history_FINAL %>% 
        filter(dose == d+1 &
                 #doses_delivered_this_date > 0 &
                 date < date_now - vaxCovDelay$delay[vaxCovDelay$dose == d + 1] &
                 date <= cutoff) %>%
        mutate(dose = dose - 1) %>%
        select(date,vaccine_type,dose,doses_delivered_this_date,age_group,risk_group) %>%
        rename(suppress = doses_delivered_this_date)
      
      #quick plot
      # workshop_p = workshop %>% group_by(date) %>% summarise(doses_delivered = sum(doses_delivered_this_date))
      # suppressor_p = suppressor %>% group_by(date) %>% summarise(doses_delivered = sum(suppress))
      # workshop_plot = ggplot() + geom_line(data = workshop_p, aes(x=date,y=doses_delivered))
      # suppressor_plot = ggplot() + geom_line(data = suppressor_p, aes(x=date,y=doses_delivered))
      # grid.arrange(workshop_plot, suppressor_plot, nrow=2)
      #almost exact overlap due to REPORTING date not actual date of dose
      #ASSUMPTION - both doses administered at once
      
      workshop = workshop %>% left_join(suppressor)
      if (! sum(suppressor$suppress) == sum(workshop$suppress,na.rm=TRUE)){stop('left join for suppression not working')}
      workshop = workshop %>% mutate(doses_delivered_this_date = doses_delivered_this_date - suppress) %>%
        select(-suppress)
      
      #quick plot
      # workshop_p = workshop %>% group_by(date) %>% summarise(doses_delivered = sum(doses_delivered_this_date))
      # suppressor_p = suppressor %>% group_by(date) %>% summarise(doses_delivered = sum(suppress))
      # workshop_plot = ggplot() + geom_line(data = workshop_p, aes(x=date,y=doses_delivered))
      # suppressor_plot = ggplot() + geom_line(data = suppressor_p, aes(x=date,y=doses_delivered))
      # grid.arrange(workshop_plot, suppressor_plot, nrow=2)
      
      correct = workshop[workshop$doses_delivered_this_date < 0,]
      while (nrow(correct)>0){
        save_point = sum(workshop$doses_delivered_this_date)
        correct = workshop[workshop$doses_delivered_this_date < 0,]
        for (i in 1:nrow(correct)){
          search_beam = workshop %>%
            filter(vaccine_type == correct$vaccine_type[i] &
                     dose == correct$dose[i] &
                     age_group == correct$age_group[i] &
                     risk_group == correct$risk_group[i] &
                     date < correct$date[i] &
                     doses_delivered_this_date > 0)
          located_date = max(search_beam$date)
          
          workshop$doses_delivered_this_date[workshop$vaccine_type == correct$vaccine_type[i] &
                                               workshop$dose == correct$dose[i] &
                                               workshop$age_group == correct$age_group[i] &
                                               workshop$risk_group == correct$risk_group[i] &
                                               workshop$date == located_date] =  
            workshop$doses_delivered_this_date[workshop$vaccine_type == correct$vaccine_type[i] &
                                                 workshop$dose == correct$dose[i] &
                                                 workshop$age_group == correct$age_group[i] &
                                                 workshop$risk_group == correct$risk_group[i] &
                                                 workshop$date == located_date] + correct$doses_delivered_this_date[i]
          
          workshop$doses_delivered_this_date[workshop$vaccine_type == correct$vaccine_type[i] &
                                               workshop$dose == correct$dose[i] &
                                               workshop$age_group == correct$age_group[i] &
                                               workshop$risk_group == correct$risk_group[i] &
                                               workshop$date == correct$date[i]] = 0  
          
          
          this_date = correct$date[i]
        }
        if (! sum(workshop$doses_delivered_this_date) == save_point){stop('you have lost some doses')}
        correct = workshop[workshop$doses_delivered_this_date < 0,]
      }
      
      #quick plot
      # workshop_p = workshop %>% group_by(date) %>% summarise(doses_delivered = sum(doses_delivered_this_date))
      # suppressor_p = suppressor %>% group_by(date) %>% summarise(doses_delivered = sum(suppress))
      # workshop_plot = ggplot() + geom_line(data = workshop_p, aes(x=date,y=doses_delivered))
      # suppressor_plot = ggplot() + geom_line(data = suppressor_p, aes(x=date,y=doses_delivered))
      # grid.arrange(workshop_plot, suppressor_plot, nrow=2)
      
      JHU_lower_doses = rbind(JHU_lower_doses,workshop)
    } 
    #____________________________________________________________________________________________________________________________
    
    
    # BRANCH TWO {hypothetical vaccines strategy data} ##########################################################################
    if (vax_strategy_toggle == "on"){
      #LIMITATON - can't have hypoth vax of multi types or intervals
      workshop = vaccination_history_FINAL %>% 
        filter(dose == d &
                 date < date_now - vaxCovDelay$delay[vaxCovDelay$dose == d] & # doses delivered and 'active' by this date
                 date > cutoff &                                              # hypothetical doses
                 vaccine_type == vax_strategy_toggles$vax_strategy_vaccine_type
        )

      suppressor = vaccination_history_FINAL %>% 
        filter(dose == d+1 &
                 #doses_delivered_this_date > 0 &
                 date < date_now - vaxCovDelay$delay[vaxCovDelay$dose == d + 1] &
                 date > cutoff &
                 vaccine_type == vax_strategy_toggles$vax_strategy_vaccine_type) %>%
        mutate(dose = dose - 1,
               date = date - round(vax_strategy_toggles$vax_strategy_vaccine_interval[d])) %>%
        select(date,vaccine_type,dose,doses_delivered_this_date,age_group,risk_group) %>%
        rename(suppress = doses_delivered_this_date)
      
      #quick plot
      # workshop_p = workshop %>% group_by(date) %>% summarise(doses_delivered = sum(doses_delivered_this_date)) %>% ungroup()
      # suppressor_p = suppressor %>% group_by(date) %>% summarise(doses_delivered = sum(suppress))
      # workshop_plot = ggplot() + geom_line(data = workshop_p, aes(x=date,y=doses_delivered))
      # suppressor_plot = ggplot() + geom_line(data = suppressor_p, aes(x=date,y=doses_delivered))
      # grid.arrange(workshop_plot, suppressor_plot, nrow=2)
      
      workshop = workshop %>% left_join(suppressor)
      workshop$suppress[is.na(workshop$suppress)] = 0
      if (! sum(suppressor$suppress) == sum(workshop$suppress,na.rm=TRUE)){stop('left join for suppression not working')}
      workshop = workshop %>% mutate(doses_delivered_this_date = doses_delivered_this_date - suppress) %>%
          select(-suppress)
      
      #quick plot
      # workshop_p = workshop %>% group_by(date) %>% summarise(doses_delivered = sum(doses_delivered_this_date))
      # suppressor_p = suppressor %>% group_by(date) %>% summarise(doses_delivered = sum(suppress))
      # workshop_p$date = as.Date(workshop_p$date)
      # suppressor_p$date = as.Date(suppressor_p$date)
      # workshop_plot = ggplot() + geom_line(data = workshop_p, aes(x=date,y=doses_delivered))
      # suppressor_plot = ggplot() + geom_line(data = suppressor_p, aes(x=date,y=doses_delivered))
      # grid.arrange(workshop_plot, suppressor_plot, nrow=2)
      
      vaccine_occupancy_tracker = bind_rows(vaccine_occupancy_tracker, workshop)
        #____________________________________________________________________________________________________________________________
    }
  }
}
vaccine_occupancy_tracker = rbind(vaccine_occupancy_tracker,JHU_lower_doses)
#____________________________________________________________________________________________________________________________________



# BRANCH ONE {removing booster dose from primary dose D}
### modification for booster poaching applies to JHU data ONLY
if (nrow(vaccination_history_FINAL[vaccination_history_FINAL$dose == 8,])>0){
  this_vax_history = vaccination_history_FINAL[vaccination_history_FINAL$dose == 8,]
  booster_type = unique(this_vax_history$vaccine_type)
  if (booster_type == 'Johnson & Johnson'){
    booster_dose_number = 2
  } else{
    booster_dose_number = 3
  }
  
## include poached booster doses
  workshop = vaccination_history_FINAL %>%
    filter(dose == 8 &
           date < date_now - vaxCovDelay$delay[vaxCovDelay$dose == booster_dose_number]) %>%
    mutate(dose = booster_dose_number)
  
  if (sum(workshop$doses_delivered_this_date)>0){
    vaccine_occupancy_tracker = bind_rows(vaccine_occupancy_tracker, workshop)
    
    ## suppress origin of poached booster doses
    #sum all d+1 / sum all d and suppress by this proportion
    #note - this will have to be recursive within vaccine_occupancy_tracker
    #  workshop = vaccination_history_FINAL[vaccination_history_FINAL$dose == 8,]
    
    suppressed_total = 0
    for (r in 1:num_risk_groups){
      for (i in 1:J){
        for (t in 1:length(unique(workshop$FROM_vaccine_type))){
          for (d in 1:D){
            suppressed = workshop %>% filter(risk_group == risk_group_labels[r] &
                                               age_group == age_group_labels[i] &
                                               FROM_vaccine_type == unique(workshop$FROM_vaccine_type)[t] &
                                               FROM_dose == d)
            suppressed = sum(suppressed$doses_delivered_this_date)
            suppressed_total = suppressed_total + suppressed
            
            population = vaccine_occupancy_tracker %>% filter(risk_group == risk_group_labels[r] &
                                                                age_group == age_group_labels[i] &
                                                                vaccine_type == unique(workshop$FROM_vaccine_type)[t] &
                                                                dose == d)
            population = sum(population$doses_delivered_this_date)
            
            global_suppression = 1 - suppressed/population
            
            if (is.na(global_suppression) == FALSE){
              vaccine_occupancy_tracker$doses_delivered_this_date[vaccine_occupancy_tracker$risk_group == risk_group_labels[r] &
                                                                    vaccine_occupancy_tracker$age_group == age_group_labels[i] &
                                                                    vaccine_occupancy_tracker$vaccine_type == unique(workshop$FROM_vaccine_type)[t] &
                                                                    vaccine_occupancy_tracker$dose == d] = global_suppression * vaccine_occupancy_tracker$doses_delivered_this_date[vaccine_occupancy_tracker$risk_group == risk_group_labels[r] &
                                                                                                                                                                                      vaccine_occupancy_tracker$age_group == age_group_labels[i] &
                                                                                                                                                                                      vaccine_occupancy_tracker$vaccine_type == unique(workshop$FROM_vaccine_type)[t] &
                                                                                                                                                                                      vaccine_occupancy_tracker$dose == d]
            }
          }
        }
      }
    }
  }
  #suppressor by FROM_vaccine_type, FROM_dose, age_group, risk_group
  
}
#____________________________________________________________________________________________________________________________________



###CHECKS - same number of people
dose1_pop = vaccination_history_FINAL %>% filter( dose == 1 & date < date_now - vaxCovDelay$delay[vaxCovDelay$dose == 1])
dose1_pop = sum(dose1_pop$doses_delivered_this_date)
if (! round(sum(vaccine_occupancy_tracker$doses_delivered_this_date)) == round(dose1_pop)){
  stop('Vaccinated population does not align with occupancy tracker')
}




### check if structure fits VE function or if need to sum across vax_type, dose, agegroup, riskgroup, date (propbably should do this anyway for cleanliness)


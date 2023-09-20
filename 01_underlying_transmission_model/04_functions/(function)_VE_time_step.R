### This function calculates the population-level vaccine effectiveness at any given time by vaccine_type, dose and age_group

VE_time_step <- function(strain_now,date_now,outcome,
                         VE_waning_LOCAL = VE_waning_distribution,#in case not copied into local environment of the function
                         vaccination_history_LOCAL = vaccination_history_FINAL,
                         SA_toggles_local = sensitivity_analysis_toggles){ 
  
  #(1) load VE_distribution
  VE_distribution <- VE_waning_LOCAL[VE_waning_LOCAL$outcome == outcome &
                                              VE_waning_LOCAL$strain == strain_now,]
  if (strain_now == 'WT'){
    VE_distribution <- VE_waning_LOCAL[VE_waning_LOCAL$outcome == outcome &
                                                VE_waning_LOCAL$strain == 'delta',] 
  }
  if ('VE_older_adults' %in% names(SA_toggles_local)){
    if ('age_group' %in% colnames(VE_distribution)){
      if (length(unique(VE_distribution$age_group)) == 1){
        VE_distribution = VE_distribution %>% ungroup() %>% select(-age_group)
      }
    }
  }
  
  
  #(2) doses delivered to this date
  vax_to_this_date <- vaccination_history_LOCAL[vaccination_history_LOCAL$date <= date_now,] 
  
  #re code additional booster doses to correct dose number
  if (nrow(vaccination_history_LOCAL[vaccination_history_LOCAL$dose == 8,])>0){
    
    booster_type = unique(vaccination_history_LOCAL$vaccine_type[vaccination_history_LOCAL$dose == 8])

    if (booster_type == 'Johnson & Johnson'){booster_dose_number = 2
    } else{booster_dose_number = 3}
    
    vax_to_this_date = vax_to_this_date %>% mutate(
    dose = case_when(
      dose == 9 ~ booster_dose_number + 1,
      dose == 8 ~ booster_dose_number,
      TRUE ~ dose
      ))
  }
  
  vax_to_this_date <- vax_to_this_date %>% 
    select(risk_group,vaccine_type,dose,date,age_group,doses_delivered_this_date) %>%
    rename(doses = doses_delivered_this_date)
  
  total_doses_up_to_this_date <- vax_to_this_date %>%
    group_by(risk_group,vaccine_type,dose,age_group) %>%
    summarise(total_delivered = sum(doses),.groups = "keep")
    
  vax_to_this_date <- vax_to_this_date %>%
    left_join(total_doses_up_to_this_date, by = c("risk_group", "vaccine_type", "dose", "age_group")) %>%
    mutate(prop = case_when(
      total_delivered >0 ~ doses/total_delivered,
      total_delivered == 0 ~ 0),
      days = as.numeric(date_now - date ))
  
  #<interlude> to add together all days >365 to 365
  meddling <- vax_to_this_date[vax_to_this_date$days > 364,]
  if(length(unique(meddling$days))>1){
    meddling <- meddling %>%
      group_by(risk_group,vaccine_type,dose,age_group) %>%
      summarise(prop = sum(prop),.groups = "keep") %>% 
      mutate(days=365)
    
    vax_to_this_date <- rbind(vax_to_this_date[vax_to_this_date$days<365,c(colnames(meddling))],
                          meddling)
  }

  #(3) Bring VE d'n and AIR history together
  if ('age_group' %in% colnames(VE_distribution)){
    workshop <- vax_to_this_date %>%
      left_join(VE_distribution, by = c("vaccine_type", "dose", "days",'age_group')) %>%
      select(risk_group,vaccine_type,dose,days,age_group,VE_days,prop) %>%
      mutate(VE_weighted = VE_days*prop)
  } else{
    workshop <- vax_to_this_date %>%
      left_join(VE_distribution, by = c("vaccine_type", "dose", "days")) %>%
      select(risk_group,vaccine_type,dose,days,age_group,VE_days,prop) %>%
      mutate(VE_weighted = VE_days*prop)
  }
  
  
  #(4) Aggregate to estimate population VE for doses
  workshop <- workshop %>%
    group_by(risk_group,dose,vaccine_type,age_group) %>%
    summarise(VE = sum(VE_weighted),.groups = "keep")
  
  if(nrow(workshop[round(workshop$VE,digits=2)>1,])){stop('VE > 1!')}
  
  #<interim> add none covered vaccines
  if (exists("vax_type_list") == FALSE){ vax_type_list = unique(vaccination_history_LOCAL$vaccine_type)}
  if (exists("age_group_labels") == FALSE){ age_group_labels = unique(vaccination_history_LOCAL$age_group)}
  if (exists("num_vax_doses") == FALSE){ num_vax_doses = length(unique(vaccination_history_LOCAL$dose[vaccination_history_LOCAL$dose != 8]))}
  if (exists("risk_group_labels") == FALSE){ risk_group_labels = unique(vaccination_history_LOCAL$risk_group)}
  
  for (t in 1:length(vax_type_list)){
    for (i in 1:length(age_group_labels)){
      for (d in 1:num_vax_doses){
        for (r in 1:length(risk_group_labels)){
          this_vax = vax_type_list[t]
          if (!( this_vax %in% unique(workshop$vaccine_type[workshop$risk_group == risk_group_labels[r] & workshop$dose == d & workshop$age_group == age_group_labels[i]]))){
            workshop2 = crossing(risk_group = risk_group_labels[r],
                                 dose = d,
                                 vaccine_type = this_vax,
                                 age_group = age_group_labels[i],
                                 VE = 0) 
            workshop = rbind(workshop,workshop2)
          } 
        }
      }
    }
  }
  
  
  workshop[is.na(workshop)] <-0 
  
  VE_tidy = workshop
  
  return(VE_tidy)
  
}
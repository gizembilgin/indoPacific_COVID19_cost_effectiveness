##NB: this calculates the population-level VE value at a given time for doses 1-2
## it is still NOT stochastic
## previously - not within an age-group as in ACT model, but within a dose and vaccine_type compartment
## ADAPTING 08/03 to be age-specific (necessary for hypoth vaccine campaigns)
## NOte: for speed, should we only go to age-specific once the hypoth campaign starts?


VE_time_step <- function(strain_now,date_now,outcome){
  
  #(1) VE_distribution
  VE_distribution <- VE_waning_distribution[VE_waning_distribution$outcome == outcome &
                                              VE_waning_distribution$strain == strain_now,] 
  if (strain_now == 'WT'){
    VE_distribution <- VE_waning_distribution[VE_waning_distribution$outcome == outcome &
                                                VE_waning_distribution$strain == 'delta',] 
  }
  
  #(2) doses delivered to this date
  occupancy = "off"
  if (occupancy == "on"){   
    if (date_now <= max_occupany_run_date){
      source(paste(getwd(),"/(function) vaccine occupancy tracker.R",sep=""))
      if (nrow(vaccine_occupancy_tracker) == 0){
        vaccine_occupancy_tracker = vaccination_history_FINAL %>% 
          filter(date == min(vaccination_history_FINAL$date)) %>%
          mutate(doses_delivered_this_date = 0)
      }
    }
    
    vax_to_this_date <- vaccine_occupancy_tracker
    
  } else{
    vax_to_this_date <- vaccination_history_FINAL[vaccination_history_FINAL$date <= date_now,] %>% mutate(
      dose = case_when(
        dose == 8 ~ booster_dose_number,
        TRUE ~ dose
      )
    )
  }
  
  vax_to_this_date <- vax_to_this_date %>% # rearrange AIR dataset
    select(risk_group,vaccine_type,dose,date,age_group,doses_delivered_this_date) %>%
    rename(doses = doses_delivered_this_date)
  
  total_doses_up_to_this_date <- aggregate(vax_to_this_date$doses, 
                           by=list(Category=vax_to_this_date$risk_group, vax_to_this_date$vaccine_type, vax_to_this_date$dose,vax_to_this_date$age_group)
                           , FUN=sum) 
  colnames(total_doses_up_to_this_date) <- c('risk_group','vaccine_type','dose','age_group','total_delivered')
  
  vax_to_this_date <- vax_to_this_date %>%
    left_join(total_doses_up_to_this_date, by = c("risk_group", "vaccine_type", "dose", "age_group")) %>%
    mutate(prop = case_when(
      total_delivered >0 ~ doses/total_delivered,
      total_delivered == 0 ~ 0
      ),
      days = as.numeric(date_now - date ))
  
  #<interlude> to add together all days >365 to 365
  meddling <- vax_to_this_date[vax_to_this_date$days > 364,]
  if(length(unique(meddling$days))>1){
    meddling <- aggregate(meddling$prop, 
                        by=list(Category=meddling$risk_group, meddling$vaccine_type, meddling$dose,meddling$age_group)
                        , FUN=sum)
    colnames(meddling)  = c('risk_group','vaccine_type','dose','age_group','prop')
    meddling = meddling %>% mutate(days=365)
    
    vax_to_this_date <- rbind(vax_to_this_date[vax_to_this_date$days<365,c(colnames(meddling))],
                          meddling)
  }

  #(3) Bring VE d'n and AIR history together
  workshop <- vax_to_this_date %>%
    left_join(VE_distribution, by = c("vaccine_type", "dose", "days")) %>%
    select(risk_group,vaccine_type,dose,days,age_group,VE_days,prop) %>%
    mutate(VE_weighted = VE_days*prop)
  
  
  #(4) Aggregate to estimate population VE for doses
  workshop <- aggregate(workshop$VE_weighted, 
                        by=list(Category=workshop$risk_group,workshop$dose,workshop$vaccine_type,workshop$age_group)
                        , FUN=sum)
  colnames(workshop) <- c('risk_group','dose','vaccine_type','age_group','VE')
  
  #<interim> add none covered vaccines
  
  for (i in 1:J){
    for (t in 1:num_vax_types){
      for (d in 1:D){
        for (r in 1:num_risk_groups){
          this_vax = vax_type_list[t]
          if (!( this_vax %in% unique(workshop$vaccine_type[workshop$risk_group == risk_group_labels[r] & workshop$dose == d & workshop$age_group == age_group_labels[i]]))){
            workshop2 = crossing(risk_group = risk_group_labels[r],
                                 dose = d,
                                 vaccine_type = this_vax,
                                 age_group = age_group_labels[i],
                                 VE =0) 
            workshop = rbind(workshop,workshop2)
          } 
        }
      }
    }
  }

  workshop[is.na(workshop)] <-0 #J&J second dose correction
  
  VE_tidy = workshop
  
  if (! VE_at_risk_suppress == 1){
    VE_tidy = VE_tidy %>%
      mutate(VE = case_when(
        risk_group == risk_group_name ~ VE * VE_at_risk_suppress,
        TRUE ~ VE
      ))
  }
  
  return(VE_tidy)
  
}











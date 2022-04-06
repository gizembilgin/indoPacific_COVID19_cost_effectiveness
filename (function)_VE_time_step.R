##NB: this calculates the population-level VE value at a given time for doses 1-2
## it is still NOT stochastic
## previously - not within an age-group as in ACT model, but within a dose and vaccine_type compartment
## ADAPTING 08/03 to be age-specific (necessary for hypoth vaccine campaigns)
## NOte: for speed, should we only go to age-specific once the hypoth campaign starts?


VE_time_step <- function(strain_now,date_now,outcome){
  
  #(1) VE_distribution
  VE_distribution <- VE_waning_distribution[VE_waning_distribution$outcome == outcome &
                                              VE_waning_distribution$strain == strain_now,] 
  
  #(2) doses delivered to this date
  vax_to_this_date <- vaccination_history_FINAL[vaccination_history_FINAL$date <= date_now,]
  vax_to_this_date <- vax_to_this_date %>% # rearrange AIR dataset
    select(vaccine_type,dose,date,age_group,doses_delivered_this_date)
  colnames(vax_to_this_date) <- c('vaccine_type','dose','date','age_group','doses')
  
  total_doses_up_to_this_date <- aggregate(vax_to_this_date$doses, 
                           by=list(Category=vax_to_this_date$vaccine_type, vax_to_this_date$dose,vax_to_this_date$age_group)
                           , FUN=sum) 
  colnames(total_doses_up_to_this_date) <- c('vaccine_type','dose','age_group','total_delivered')
  
  vax_to_this_date <- vax_to_this_date %>%
    left_join(total_doses_up_to_this_date) %>%
    mutate(prop = case_when(
      total_delivered >0 ~ doses/total_delivered,
      total_delivered == 0 ~ 0
      ),
      days = as.numeric(date_now - date )) 
  
  #<interlude> to add together all days >365 to 365
  meddling <- vax_to_this_date[vax_to_this_date$days > 364,]
  if(length(unique(meddling$days))>1){
    meddling <- aggregate(meddling$prop, 
                        by=list(Category=meddling$vaccine_type, meddling$dose,meddling$age_group)
                        , FUN=sum)
    colnames(meddling)  = c('vaccine_type','dose','age_group','prop')
    meddling = meddling %>% mutate(days=365)
    
    vax_to_this_date <- rbind(vax_to_this_date[vax_to_this_date$days<365,c(colnames(meddling))],
                          meddling)
  }
  
  #(3) Bring VE d'n and AIR history together
  workshop <- vax_to_this_date %>%
    left_join(VE_distribution) %>%
    select(vaccine_type,dose,days,age_group,VE_days,prop) %>%
    mutate(VE_weighted = VE_days*prop)
  
  #(4) Aggregate to estimate population VE for doses
  workshop <- aggregate(workshop$VE_weighted, 
                        by=list(Category=workshop$dose,workshop$vaccine_type,workshop$age_group)
                        , FUN=sum)
  colnames(workshop) <- c('dose','vaccine_type','age_group','VE')
  
  #<interim> add none covered vaccines
  for (i in 1:num_vax_types){
    this_vax = vax_type_list[i]
    if (!( this_vax %in% unique(workshop$vaccine_type))){
      workshop2 = crossing(dose = c(1:num_vax_doses),
                           vaccine_type = this_vax,
                           age_group = age_group_labels,
                           VE =0) 
      workshop = rbind(workshop,workshop2)
    } 
    #slightly manual work around!
    if(this_vax == "Johnson & Johnson" & nrow(workshop[workshop$vaccine_type =="Johnson & Johnson",])==num_age_groups){
      workshop2 = crossing(dose = 2,
                           vaccine_type = "Johnson & Johnson",
                           age_group = age_group_labels,
                           VE =0) 
      workshop = rbind(workshop,workshop2)
    }
    
  }

  # #(5) Transpose to ODE format VE[type,dose]
  # VE_result <- pivot_wider(workshop,
  #                          id_cols = c(vaccine_type,age_group),
  #                          names_from = dose,
  #                          values_from = VE)
  # VE_result <- VE_result %>%
  #   arrange(vaccine_type)
  # VE_result <- VE_result[,-c(1)]
  # VE_result[is.na(VE_result)] <-0 #J&J second dose correction
  
  ##instead retain tidy form
  VE_tidy = workshop
  
  return(VE_tidy)
  
}

# strain_now = 'delta'
# date_now = as.Date('2021-08-20') 
# test <- VE_time_step(strain_now,date_now)
# test












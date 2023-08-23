###This script creates VE_waning_distribution for VE_time_step in two steps:
#(1) Averaging booster dose effectiveness across heterogenous combinations
#(2) Forcing dose 1 < dose 2 < ...

VE_waning_distribution_expander <- function(VE_waning_distribution, 
                                            this_outcome,
                                            booster_combinations){ 
  
  local_VE_waning_distribution = VE_waning_distribution %>% filter(outcome == this_outcome)
  
  ### Step One: average booster dose effectiveness across heterogeneous combinations of each vaccine-dose combination
  workshop = data.frame()
  if (nrow(booster_combinations)>0){ #if booster dose exists
    for (this_dose in unique(booster_combinations$dose)){ # for each booster dose
      for (this_vax in unique(booster_combinations$vaccine_type[booster_combinations$dose == this_dose])){ # for each booster type
        
        # First Choice = exact primary dose + booster dose combination
        this_combo = local_VE_waning_distribution %>% 
          filter(schedule == "booster" & 
                   dose == this_dose & 
                   primary_if_booster %in% unique(booster_combinations$FROM_vaccine_type[booster_combinations$dose == this_dose & booster_combinations$vaccine_type == this_vax]) &
                   vaccine_type == this_vax) %>%
          group_by(schedule,vaccine_mode,strain,outcome,vaccine_type,dose,days,.add = TRUE) %>%
          summarise(VE_days = mean(VE_days,na.rm=TRUE),.groups = "keep") 
        #small edit for J&J
        if (this_vax == "Johnson & Johnson" & nrow(this_combo) == 0){
          this_combo = local_VE_waning_distribution %>% 
            filter(schedule == "booster" & 
                     dose == this_dose & 
                     vaccine_type == this_vax) %>%
            group_by(schedule,vaccine_mode,strain,outcome,vaccine_type,dose,days,.add = TRUE) %>%
            summarise(VE_days = mean(VE_days,na.rm=TRUE),.groups = "keep") 
        }
        
        # Second Choice = same primary schedule + booster of same vaccine mode
        if (nrow(this_combo) == 0){
          this_vax_mode = unique(booster_combinations$vaccine_mode[booster_combinations$vaccine_type == this_vax])
          this_combo = local_VE_waning_distribution %>% 
            filter(schedule == "booster" & dose == this_dose & 
                     primary_if_booster %in% unique(booster_combinations$FROM_vaccine_type[booster_combinations$dose == this_dose & booster_combinations$vaccine_type == this_vax]) &
                     vaccine_mode == this_vax_mode) %>%
            group_by(schedule,vaccine_mode,strain,outcome,dose,days,.add = TRUE) %>%
            summarise(VE_days = mean(VE_days,na.rm=TRUE),.groups = "keep") %>%
            mutate(vaccine_type = this_vax)
        }
        
        # Third Choice = same primary schedule + any booster
        if (nrow(this_combo) == 0){ 
          this_combo = local_VE_waning_distribution %>% 
            filter(schedule == "booster" & dose == this_dose & 
                     primary_if_booster %in% unique(booster_combinations$FROM_vaccine_type[booster_combinations$dose == this_dose & booster_combinations$vaccine_type == this_vax])) %>%
            group_by(schedule,vaccine_mode,strain,outcome,dose,days,.add = TRUE) %>%
            summarise(VE_days = mean(VE_days,na.rm=TRUE),.groups = "keep")  %>%
            mutate(vaccine_type = this_vax) 
        }
        
        # Otherwise... rethink!
        if (nrow(this_combo) == 0){stop('Need a VE for this booster!')}
        
        workshop = rbind(workshop,this_combo)
      }
    }
  }
  
  local_VE_waning_distribution = local_VE_waning_distribution %>% 
    filter(schedule == "primary") %>%
    select(-primary_if_booster)
  local_VE_waning_distribution = rbind(local_VE_waning_distribution,workshop)
  
  ### Step Two: force dose 1 <= dose 2 <= dose 3 <= ... 
  #(NB: in stochastic runs of the antiviral model force this at stage of point estimates)
  for (loop_dose in 2:max(booster_combinations$dose)){
    issues_pt1 = local_VE_waning_distribution %>%
      filter(dose == loop_dose - 1) %>%
      rename(lower_dose = VE_days)
    issues_pt2 = local_VE_waning_distribution %>%
      filter(dose == loop_dose) %>%
      rename(upper_dose = VE_days)
    if ("age_group" %in% colnames(issues_pt1)){
      issues = issues_pt1 %>% 
        left_join(issues_pt2,by = c("strain", "vaccine_type", "dose", "outcome", "days",  "vaccine_mode", "schedule","age_group")) %>%
        group_by(schedule,vaccine_mode,strain,outcome,vaccine_type,days,.add = TRUE) %>%
        filter((is.na(upper_dose) == FALSE & upper_dose<lower_dose))
    } else{
      issues = issues_pt1 %>% 
        left_join(issues_pt2,by = c("strain", "vaccine_type", "dose", "outcome", "days",  "vaccine_mode", "schedule")) %>%
        group_by(schedule,vaccine_mode,strain,outcome,vaccine_type,days,.add = TRUE) %>%
        filter((is.na(upper_dose) == FALSE & upper_dose<lower_dose))
    }
    
    
    while(nrow(issues)>0){
      row = issues[1,]
      row$upper_dose[is.na(row$upper_dose)] <- 0
      this_vax = issues$vaccine_type[1]
      this_strain = issues$strain[1]
      if(row$lower_dose == 0){stop('lower dose == 0')}
      
      if (row$upper_dose < row$lower_dose) {
        local_VE_waning_distribution$VE_days[local_VE_waning_distribution$dose == loop_dose &
                                               local_VE_waning_distribution$vaccine_type == this_vax &
                                               local_VE_waning_distribution$strain == this_strain ] =
          local_VE_waning_distribution$VE_days[local_VE_waning_distribution$dose == loop_dose - 1 &
                                                 local_VE_waning_distribution$vaccine_type == this_vax &
                                                 local_VE_waning_distribution$strain == this_strain ]
      } 
      
      issues_pt1 = local_VE_waning_distribution %>%
        filter(dose == loop_dose - 1) %>%
        rename(lower_dose = VE_days)
      issues_pt2 = local_VE_waning_distribution %>%
        filter(dose == loop_dose) %>%
        rename(upper_dose = VE_days)
      if ("age_group" %in% colnames(issues_pt1)){
        issues = issues_pt1 %>% 
          left_join(issues_pt2,by = c("strain", "vaccine_type", "dose", "outcome", "days",  "vaccine_mode", "schedule","age_group")) %>%
          group_by(schedule,vaccine_mode,strain,outcome,vaccine_type,days,.add = TRUE) %>%
          filter((is.na(upper_dose) == FALSE & upper_dose<lower_dose))
      } else{
        issues = issues_pt1 %>% 
          left_join(issues_pt2,by = c("strain", "vaccine_type", "dose", "outcome", "days", "vaccine_mode", "schedule")) %>%
          group_by(schedule,vaccine_mode,strain,outcome,vaccine_type,days,.add = TRUE) %>%
          filter((is.na(upper_dose) == FALSE & upper_dose<lower_dose))
      }
    }
  }
  
  VE_waning_distribution = bind_rows(VE_waning_distribution[VE_waning_distribution$outcome != this_outcome,],local_VE_waning_distribution)
  
  return(VE_waning_distribution)
}
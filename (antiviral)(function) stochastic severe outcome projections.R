### This program trickles through the uncertainty of severe outcome projections in the transmission model
### It combines the four scripts from the original transmission model:
### (1) (mech shop) severe outcome age distribution
### (2) (mech shop) severe outcome setting-specific rates
### (3) (5)_severe_outcomes_calc
### (4) (function)_severe_outcome_proj

##### SETUP
age_groups_num = c(0,4,9,17,29,44,59,69,110)
age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100')
load(file = '1_inputs/severe_outcome_age_distribution_RAW_v2.Rdata' )
pop_estimates <- read.csv(paste(rootpath,"inputs/pop_estimates.csv",sep=''), header=TRUE)


# stochastic_severe_outcomes <- function(
    setting = 'SLE'
#     incidence_log,
#     exposed_log
# ){
    
   ### PART ONE: sampling raw age distributions -> RR by age  #############################################
    sampled_value = mapply(rlnorm,1,severe_outcome_age_distribution_RAW_v2$lognorm_a, severe_outcome_age_distribution_RAW_v2$lognorm_b)
    workshop = cbind(severe_outcome_age_distribution_RAW_v2,sampled_value)
    workshop = workshop %>% mutate(sampled_value = case_when(
      mean == LB & mean == UB ~ mean,
      TRUE ~ sampled_value
    )) %>%
      select(agegroup,outcome,sampled_value)
    
    #Join overall value back on to age-specific values to calculate RR
    overall = workshop %>%
      filter(agegroup == 'overall') %>% 
      rename(overall=sampled_value) %>% 
      select(outcome,overall)
    workshop = workshop %>%
      filter(agegroup != 'overall')  %>%
      left_join(overall, by = "outcome") %>%
      mutate(RR = sampled_value/overall) %>%
      select(agegroup,outcome,RR)
    
    rm(sampled_value,overall)
    ########################################################################################################   
    
    
    ### PART TWO: Adjust age-distributions to setting ######################################################
    severe_outcome_country_level <- read.csv('1_inputs/severe_outcome_country_level.csv')
    severe_outcome_country_level = severe_outcome_country_level %>% filter(country == setting) 
    
    sampled_value = mapply(runif,1,severe_outcome_country_level$LB, severe_outcome_country_level$UB) 
    severe_outcome_country_level = cbind(severe_outcome_country_level,sampled_value)
    severe_outcome_country_level = severe_outcome_country_level %>% 
      rename(pop_est = sampled_value) %>%
      select(outcome,pop_est)
    workshop = workshop %>% 
      left_join(severe_outcome_country_level, by = "outcome") %>%
      mutate(age_est = pop_est * RR)
   
    
    #<intermission: ensure pop-level estimate aligns with age_est * %pop >
    age_groups_RAW = c(0,9,19,29,39,49,59,69,100)
    age_group_labels_RAW = c('0 to 9','10 to 19','20 to 29','30 to 39','40 to 49','50 to 59','60 to 69','70 to 100')
    
    pop_bands_RAW <- pop_estimates  %>% #pop banding by Seedat et al. 
      filter(country == setting) %>%
      mutate(agegroup = cut(age,breaks = age_groups_RAW, include.lowest = T,labels = age_group_labels_RAW)) %>%
      group_by(agegroup) %>%
      summarise(pop = sum(population))%>%  
      mutate(pop_percentage = pop/sum(pop_bands_RAW$pop))  
    
    workshop = workshop %>% 
      left_join(pop_bands_RAW, by = "agegroup") %>%
      mutate(interim = pop_percentage * age_est) 
    
    workshop_sum = workshop %>%
      group_by(outcome) %>%
      summarise(interim_est = sum(interim)) %>% 
      left_join(severe_outcome_country_level, by = c("outcome")) %>%
      mutate(correction = pop_est/interim_est) %>%
      select(outcome,correction)
    
    workshop = workshop %>% 
      left_join(workshop_sum, by = "outcome") %>%
      mutate(RR2 = RR * correction,
             age_est2 = pop_est * RR2,
             interim2 = pop_percentage * age_est2)
    
    check = workshop %>%
      group_by(outcome) %>%
      summarise(interim_est = sum(interim2)) %>% 
      left_join(severe_outcome_country_level, by = "outcome") %>%
      mutate(correction = pop_est/interim_est)
    if (nrow(check[round(check$correction,digits=2)>1,])>0){stop('issue with correction')}
    #<end intermission>
    
    workshop = workshop %>%
      select(agegroup,outcome,RR2) %>%
      rename(RR = RR2,
             agegroup_RAW = agegroup)
    
    rm(check,sampled_value,workshop_sum)
    ########################################################################################################   
    
    
    ### PART THREE: convert RR to model age groups ########################################################
    pop_setting = pop_estimates %>%
      filter(country == setting) %>%
      mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
      ungroup() %>%
      group_by(age_group) %>%
      summarise(pop = sum(population)) %>%
      ungroup() %>% 
      mutate(pop_percent = pop/sum(pop))
    pop_w <- pop_estimates %>%
      filter(country == setting) %>%
      mutate(agegroup_RAW = cut(age,breaks = age_groups_RAW, include.lowest = T,labels = age_group_labels_RAW),
             agegroup_model = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
      ungroup() %>%
      group_by(agegroup_model) %>%
      mutate(group_percent = population/sum(population)) %>%
      select(age,agegroup_RAW,agegroup_model,group_percent)
    
    workshop = workshop %>% 
      left_join(pop_w, by = c("agegroup_RAW")) %>%
      mutate(interim = RR*group_percent)
    workshop_sum =  workshop %>% 
      group_by(outcome,agegroup_model) %>%
      summarise(RR = sum(interim)) %>%
      rename(age_group = agegroup_model)
    
    #check
    workshop_sum = workshop_sum %>%
      left_join(pop_setting, by = c("age_group")) %>%
      mutate(interim =RR*pop_percent)
    if(sum(workshop_sum$pop_percent) != length(unique(workshop_sum$outcome))){stop('ah!')}
    #aggregate(workshop_sum$interim, by=list(category= workshop_sum$outcome), FUN=sum) #1! perfect!
    
    age_distribution_RR =  workshop_sum %>%
      select(outcome,age_group,RR)
    
    rm(workshop_sum,workshop,pop_w,pop_setting)
    ########################################################################################################   
    
    
    ### PART FOUR:  ########################################################
    

  
  
  
 
  #######################################################################################################
  
  



  
#  return()
#}
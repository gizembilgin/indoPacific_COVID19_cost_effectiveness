### This program samples from all variables used for severe outcome projections in the transmission model
### It combines the components of four scripts from the original transmission model:
### (1) (mech shop) severe outcome age distribution
### (2) (mech shop) severe outcome setting-specific rates
### (3) (5)_severe_outcomes_calc
### (4) (6)_severe_outcome_proj

stochastic_severe_outcomes_sampling <- function(
    
    booster_combinations,
    setting = 'SLE',
    num_time_steps = 365,
    strain_now = 'omicron',
    vaccine_type_list = input_vaccine_type_list,
    risk_group_name = toggle_vax_scenario_risk_group,
    num_age_groups = 8,
    age_groups_num = c(0,4,9,17,29,44,59,69,110),
    age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100'),
    local_stochastic_VE_sampling = 'uniform'
    
){
  
  ##### SETUP
  load(file = '1_inputs/severe_outcome_age_distribution_RAW_v2.Rdata' )
  load(file = '1_inputs/delta_multiplier.Rdata' )
  load(file = '1_inputs/omicron_multiplier.Rdata' )
  load(file = '1_inputs/YLL_FINAL.Rdata' )
  load(file = '1_inputs/RR_sample.Rdata')
  load(file = '1_inputs/rho_SO_sample.Rdata') 
  load(file = "1_inputs/UN_world_population_prospects/UN_pop_est.Rdata")
  load(file = "1_inputs/severe_outcome_country_level.Rdata")

  source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) stochastic_VE.R",sep=""))
  source(paste(getwd(),"/(function)_VE_time_step.R",sep=""))
  
  risk_group_labels = c('general_public',risk_group_name)
  
  
  ### PART ONE: sampling raw age distributions -> RR by age  #############################################
    sampled_value = mapply(rlnorm,1,severe_outcome_age_distribution_RAW_v2$lognorm_a, severe_outcome_age_distribution_RAW_v2$lognorm_b)
    workshop = cbind(severe_outcome_age_distribution_RAW_v2,sampled_value)
    workshop = workshop %>%
      filter(outcome %in% c('death','severe_disease','hosp')) %>%
      mutate(sampled_value = case_when(
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
    severe_outcome_country_level = severe_outcome_country_level %>%
      filter(country == setting & outcome %in% c('death','severe_disease','hosp')) 
    
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
    
    pop_bands_RAW <- UN_pop_est  %>% 
      rename(country = ISO3_code,
             population = PopTotal,
             age = AgeGrp) %>% #pop banding by Seedat et al. 
      filter(country == setting) %>%
      mutate(agegroup = cut(age,breaks = age_groups_RAW, include.lowest = T,labels = age_group_labels_RAW)) %>%
      group_by(agegroup) %>%
      summarise(pop = sum(population),.groups = "keep")
    pop_bands_RAW = pop_bands_RAW %>%  
      mutate(pop_percentage = pop/sum(pop_bands_RAW$pop))  
    
    workshop = workshop %>% 
      left_join(pop_bands_RAW, by = "agegroup") %>%
      mutate(interim = pop_percentage * age_est) 
    
    workshop_sum = workshop %>%
      group_by(outcome) %>%
      summarise(interim_est = sum(interim),.groups = "keep") %>% 
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
      summarise(interim_est = sum(interim2),.groups = "keep") %>% 
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
    pop_setting = UN_pop_est %>% 
      rename(country = ISO3_code,
             population = PopTotal,
             age = AgeGrp) %>%
      filter(country == setting) %>%
      mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
      ungroup() %>%
      group_by(age_group) %>%
      summarise(pop = sum(population),.groups = "keep") %>%
      ungroup() %>% 
      mutate(pop_percent = pop/sum(pop))
    pop_w <- UN_pop_est %>% 
      rename(country = ISO3_code,
             country_long = Location,
             population = PopTotal,
             population_female = PopFemale,
             age = AgeGrp) %>%
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
      summarise(RR = sum(interim),.groups = "keep") %>%
      rename(age_group = agegroup_model)
    
    #check
    workshop_sum = workshop_sum %>%
      left_join(pop_setting, by = c("age_group")) %>%
      mutate(interim =RR*pop_percent)
    if(sum(workshop_sum$pop_percent) != length(unique(workshop_sum$outcome))){stop('ah!')}
    #aggregate(workshop_sum$interim, by=list(category= workshop_sum$outcome), FUN=sum) #1! perfect!
    
    age_distribution_RR =  workshop_sum %>%
      select(outcome,age_group,RR)
    
    rm(workshop_sum,workshop,pop_w)
    ########################################################################################################   
    
    
    ### PART FOUR: RR * pop-level setting-specific estimates of severe outcomes ###########################
    #NB: use severe_outcome_country_level (as sampled in part three)
    sampled_value = mapply(rnorm,1,delta_multiplier$multiplier, delta_multiplier$sd)
    workshop_d = cbind(delta_multiplier,sampled_value)
    workshop_d = workshop_d %>% select(outcome,multiplier = sampled_value)
    
    sampled_value = mapply(rnorm,1,omicron_multiplier$multiplier, omicron_multiplier$sd)
    workshop_o = cbind(omicron_multiplier,sampled_value)
    workshop_o = workshop_o %>% select(outcome,multiplier = sampled_value)
    
    variant_multiplier = workshop_d %>%
      mutate(multiplier = case_when(
        outcome == 'hosp' ~ multiplier*workshop_o$multiplier[workshop_o$outcome == 'hosp'],
        outcome %in% c('ICU','death') ~ multiplier*workshop_o$multiplier[workshop_o$outcome == 'hosp_long'])) #ASSUMPTION: hosp_long proportional to ICU and death
    
    severe_outcome_country_level = severe_outcome_country_level %>%
      mutate(pop_est = case_when(
        outcome == 'death' ~ pop_est * variant_multiplier$multiplier[variant_multiplier$outcome == 'death'],
        outcome == 'severe_disease' ~ pop_est * variant_multiplier$multiplier[variant_multiplier$outcome == 'ICU'], #ASSUMPTION
        outcome == 'hosp' ~ pop_est * variant_multiplier$multiplier[variant_multiplier$outcome == 'hosp']
      )) %>%
      left_join(age_distribution_RR, by = c('outcome')) %>%
      mutate(pop_est = (pop_est * RR)/100) %>%
      rename(percentage = pop_est)
    
    # discounting_rate = 0
    # #apply discounting using continuous approach, as per larson et al.
    # if (discounting_rate >0){YLL_FINAL$life_expectancy = (1/discounting_rate)*(1-exp(-discounting_rate*YLL_FINAL$life_expectancy ))}
    YLL_FINAL = YLL_FINAL %>% filter(ISO3_code == setting)
    YLL_row = severe_outcome_country_level %>%
      filter(outcome == 'death') %>%
      mutate(outcome = 'YLL') %>%
      left_join(YLL_FINAL, by = "age_group") %>%
      mutate(percentage = percentage*YLL) %>%
      select(-YLL,-ISO3_code)
    
    severe_outcome_country_level = rbind(severe_outcome_country_level,YLL_row)
    
    rm(workshop_d, workshop_o, sampled_value, variant_multiplier,YLL_row)
    
    #if risk-group included, then adjust general population incidence rate so pop-level estimates stay the same
    if (length(risk_group_labels)>1){
      
      severe_outcomes_list = unique(severe_outcome_country_level$outcome)
      severe_outcome_country_level_wRisk = data.frame()
      
      #create pop distribution of high-risk group
      if (risk_group_name %in% c('adults_with_comorbidities')) {
        risk_dn = read.csv('1_inputs/risk_group_distribution.csv')
        risk_dn = risk_dn[risk_dn$risk_group_name == risk_group_name, ]
      } else if (risk_group_name %in% c('pregnant_women')) {
        load(file = "1_inputs/prevalence_pregnancy.Rdata")
        risk_dn = prevalence_pregnancy %>%
          filter(country == setting)
      } else {
        stop('risk_group_name not a valid value')
      }
      risk_dn = risk_dn %>%  select(age_group, prop)
      
      pop_high_risk = pop_setting %>% 
        left_join(risk_dn, by = "age_group") %>%
        mutate(risk_group = risk_group_name,
               pop = round(pop*prop)) %>% 
        select(risk_group,age_group,pop)
      
      pop_general_public   = pop_setting %>% 
        left_join(risk_dn, by = "age_group") %>%
        mutate(risk_group = 'general_public',
               pop = round(pop*(1-prop))) %>% 
        select(risk_group,age_group,pop)
      
      pop_risk_group_dn = rbind(pop_general_public,pop_high_risk)
      
      #sample RR from distribution
      RR_estimate = rnorm(1,RR_sample$RR[RR_sample$risk == risk_group_name], RR_sample$sd[RR_sample$risk == risk_group_name])
      
      #adjust incidence ratio general public and incidence ratio risk group
      for (o in 1:length(severe_outcomes_list)){
        this_outcome = severe_outcomes_list[o]
        for (i in 1:num_age_groups){
          this_age = age_group_labels[i]
          row = severe_outcome_country_level %>% filter(outcome == this_outcome & age_group == this_age)
          
          IR = row$percentage
          P = pop_setting$pop[pop_setting$age_group == this_age]
          P_general = pop_risk_group_dn$pop[pop_risk_group_dn$risk_group == 'general_public' & pop_risk_group_dn$age_group == this_age]
          P_risk = pop_risk_group_dn$pop[pop_risk_group_dn$risk_group == risk_group_name & pop_risk_group_dn$age_group == this_age]
          if (P != (P_general+P_risk)){stop('Line 15 P_general + P_risk != P_overall')}
          
          IR_gen = ((IR*P)/((RR_estimate*P_risk)/P_general +1))/P_general
          IR_risk = ((IR*P)/(P_general/(RR_estimate*P_risk) +1))/P_risk
          
          row_gen = row %>% mutate(percentage = IR_gen,risk_group = 'general_public')
          row_risk = row %>% mutate(percentage = IR_risk,risk_group = risk_group_name)
          severe_outcome_country_level_wRisk = rbind(severe_outcome_country_level_wRisk,row_gen,row_risk)
        }
      }
      severe_outcome_country_level_wRisk$percentage[is.na(severe_outcome_country_level_wRisk$percentage)]=0
      severe_outcome_country_level = severe_outcome_country_level_wRisk
    }
    ########################################################################################################   
    
    
    ### PART FIVE: Initalise of VE ########################################################################
    ##NB: ALWAYS, assume lower VE in older adults and adults with comorbidities
    #(1) Adjust so outcome_VE
    severe_outcome_country_level = severe_outcome_country_level %>%
      mutate(outcome_VE = case_when(
        outcome %in% c('death','YLL') ~ 'death',
        outcome %in% c('hosp','severe_disease') ~ 'severe_disease'
      ))
    
    #(2) Load stochastic VE distribution
    #VE_waning_distribution_SO - sample UNIFORM from all point estimates, waning distribution, ratios between age groups etc. 
    VE_waning_distribution = stochastic_VE(booster_combinations = booster_combinations,
                                           setting = setting,
                                           toggle_sampling = local_stochastic_VE_sampling) %>%
      filter(strain == strain_now & vaccine_type %in% vaccine_type_list)
    
    rho_SO_est = rbeta(1,rho_SO_sample$beta_a, rho_SO_sample$beta_b)
    

    result = list(
      SAVE_severe_outcome_country_level = severe_outcome_country_level,
      SAVE_VE_waning_distribution = VE_waning_distribution,
      SAVE_rho_SO_est = rho_SO_est
    ) 
  ########################################################################################################
  
  return(result)
}
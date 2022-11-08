### This program allows for alterations in severe outcome projections in the transmission model
### It combines the four scripts from the original transmission model:
### (1) (mech shop) severe outcome age distribution
### (2) (mech shop) severe outcome setting-specific rates
### (3) (5)_severe_outcomes_calc
### (4) (6)_severe_outcome_proj

##### SETUP
age_groups_num = c(0,4,9,17,29,44,59,69,110)
age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100')

load(file = '1_inputs/severe_outcome_age_distribution_RAW_v2.Rdata' )
load(file = '1_inputs/delta_multiplier.Rdata' )
load(file = '1_inputs/omicron_multiplier.Rdata' )
load(file = '1_inputs/YLL_FINAL.Rdata' )
load(file = '1_inputs/SA_VE_older_muted_SO.Rdata')
load(file = '1_inputs/RR_sample.Rdata')
load(file = '1_inputs/rho_SO_sample.Rdata') 

severe_outcome_country_level_input <- read.csv('1_inputs/severe_outcome_country_level.csv')
pop_estimates <- read.csv(paste(rootpath,"inputs/pop_estimates.csv",sep=''), header=TRUE)
source(paste(getwd(),"/(antiviral)(function) stochastic_VE.R",sep=""))
source(paste(getwd(),"/(function)_VE_time_step.R",sep=""))

sensitivity_analysis_toggles = list(VE_older_adults = "reduced",VE_adults_comorb = 0.9)

sensitivity_severe_outcomes <- function(
    incidence_log = this_incidence_log,
    incidence_log_tidy = this_incidence_log_tidy,
    vaccination_history_FINAL = this_vaccination_history_FINAL,
    exposed_log = this_exposed_log,
    
    setting = 'SLE',
    num_time_steps = 365,
    strain_now = 'omicron',
    risk_group_name = toggle_vax_scenario_risk_group,
    date_start = toggle_antiviral_start_date,
    toggle_intervention = 'antiviral',
    
    #non-local variables
    prop_sympt_LOCAL = prop_sympt,
    
    toggle_severe_outcome_proj_multiplier = 1,
    toggle_reinfection_ratio_multiplier = 1,
    toggle_VE_multiplier = 1
    
){
  
  risk_group_labels = unique(incidence_log_tidy$risk_group)  
  
  ### PART ONE: import raw age distributions (not setting dependent) -> RR by age  #############################################
  workshop = severe_outcome_age_distribution_RAW_v2 %>%
    select(agegroup,outcome,mean)
  
  #Join overall value back on to age-specific values to calculate RR
  overall = workshop %>%
    filter(agegroup == 'overall') %>% 
    rename(overall=mean) %>% 
    select(outcome,overall)
  workshop = workshop %>%
    filter(agegroup != 'overall')  %>%
    left_join(overall, by = "outcome") %>%
    mutate(RR = mean/overall) %>%
    select(agegroup,outcome,RR)
  
  rm(overall)
  ########################################################################################################   
  
  
  ### PART TWO: Adjust age-distributions to setting ######################################################
  severe_outcome_country_level = severe_outcome_country_level_input %>%
    filter(country == setting & outcome %in% c('death','severe_disease','hosp')) %>%
    mutate(percentage = toggle_severe_outcome_proj_multiplier* percentage/100) %>%  #make percentage between 0-1 #APPLY SENSITIVITY TOGGLE
    rename(pop_est = percentage) %>%
    select(outcome,pop_est)
  workshop = workshop %>%
    filter( outcome %in% c('death','severe_disease','hosp')) %>% 
    left_join(severe_outcome_country_level, by = "outcome") %>%
    mutate(age_est = pop_est * RR)
  
  #<intermission: ensure pop-level estimate aligns with age_est * %pop >
  age_groups_RAW = c(0,9,19,29,39,49,59,69,100)
  age_group_labels_RAW = c('0 to 9','10 to 19','20 to 29','30 to 39','40 to 49','50 to 59','60 to 69','70 to 100')
  
  pop_bands_RAW <- pop_estimates  %>% #pop banding by Seedat et al. 
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
  pop_setting = pop_estimates %>%
    filter(country == setting) %>%
    mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
    ungroup() %>%
    group_by(age_group) %>%
    summarise(pop = sum(population),.groups = "keep") %>%
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
  workshop_d = delta_multiplier %>% select(outcome,multiplier)
  workshop_o = omicron_multiplier %>% select(outcome,multiplier)
  
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
    mutate(pop_est = pop_est * RR)
  
  # discounting_rate = 0
  # #apply discounting using continuous approach, as per larson et al.
  # if (discounting_rate >0){YLL_FINAL$life_expectancy = (1/discounting_rate)*(1-exp(-discounting_rate*YLL_FINAL$life_expectancy ))}
  
  YLL_row = severe_outcome_country_level %>%
    filter(outcome == 'death') %>%
    mutate(outcome = 'YLL') %>%
    left_join(YLL_FINAL, by = "age_group") %>%
    mutate(pop_est = pop_est*YLL) %>%
    select(-YLL)
  
  severe_outcome_country_level = rbind(severe_outcome_country_level,YLL_row)
  
  rm(workshop_d, workshop_o, sampled_value, variant_multiplier,YLL_row)
  ########################################################################################################   
  
  
  ### PART FIVE: Initalise of VE ########################################################################
  ##NB: ALWAYS, assume lower VE in older adults and adults with comorbidities
  #(1) Adjust so outcome_VE
  severe_outcome_country_level = severe_outcome_country_level %>%
    mutate(outcome_VE = case_when(
      outcome %in% c('death','YLL') ~ 'death',
      outcome %in% c('hosp','severe_disease') ~ 'severe_disease'
    ))
  
  #(2) Load stochastic VE distirbution
  #VE_waning_distribution_SO - sample UNIFORM from all point estimates, waning distribution, ratios between age groups etc. 
  VE_waning_distribution = SA_VE_older_muted_SO %>% filter(waning == TRUE)
  
  #(3) Calculate VE against severe outcome by day
  VE_tracker = data.frame()
  for (outcome in c('death','severe_disease')){
    for (day in 0:num_time_steps){
      workshop = VE_time_step(strain_now,
                              date_now = date_start+day,
                              outcome,
                              VE_waning_LOCAL=VE_waning_distribution, 
                              vaccination_history_LOCAL = vaccination_history_FINAL)
      workshop = workshop %>% mutate(date=day,
                                     outcome_VE=outcome)
      VE_tracker = rbind(VE_tracker,workshop)
    }
  }
  VE_tracker$date = date_start + VE_tracker$date
  
  workshop = crossing(risk_group = risk_group_labels,
                      dose = 0,
                      vaccine_type = "unvaccinated",
                      age_group = age_group_labels,
                      date = unique(incidence_log_tidy$date),
                      outcome_VE = c('death','severe_disease'),
                      VE = 0)
  VE_tracker = rbind(VE_tracker,workshop)
  
  if ('VE_adults_comorb' %in% names(sensitivity_analysis_toggles)){
    if (risk_group_name == 'adults_with_comorbidities'){
      VE_tracker =  VE_tracker %>%
        mutate(VE = case_when(
          risk_group == risk_group_name & age_group %in% c("30 to 44","45 to 59") ~ VE * sensitivity_analysis_toggles$VE_adults_comorb,
          TRUE ~ VE
        ))
    }
  }
  
  VE_tracker = VE_tracker %>% ungroup() %>% mutate(VE = VE*toggle_VE_multiplier)
  VE_tracker$VE[VE_tracker$VE>1] = 1
  ########################################################################################################   
  
  
  ### PART SIX: Apply VE ########################################################################
  if (length(risk_group_labels)>1){
    #if risk-group included, then adjust general population incidence rate so pop-level estimates stay the same
    
    severe_outcomes_list = unique(severe_outcome_country_level$outcome)
    severe_outcome_country_level_wRisk = data.frame()
    
    #create pop distribution of high-risk group
    if (risk_group_name %in% c('adults_with_comorbidities')) {
      risk_dn = read.csv('1_inputs/risk_group_distribution.csv')
      risk_dn = risk_dn[risk_dn$risk_group_name == risk_group_name, ]
    } else if (risk_group_name %in% c('pregnant_women')) {
      load(file = "1_inputs/prevalence_pregnancy.Rdata")
      risk_dn = prevalence_pregnancy
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
    RR_estimate = RR_sample$RR[RR_sample$risk == risk_group_name]
    
    #adjust incidence ratio general public and incidence ratio risk group
    for (o in 1:length(severe_outcomes_list)){
      this_outcome = severe_outcomes_list[o]
      for (i in 1:length(unique(vaccination_history_FINAL$age_group))){
        this_age = age_group_labels[i]
        row = severe_outcome_country_level %>% filter(outcome == this_outcome & age_group == this_age)
        
        IR = row$pop_est
        P = pop_setting$pop[pop_setting$age_group == this_age]
        P_general = pop_risk_group_dn$pop[pop_risk_group_dn$risk_group == 'general_public' & pop_risk_group_dn$age_group == this_age]
        P_risk = pop_risk_group_dn$pop[pop_risk_group_dn$risk_group == risk_group_name & pop_risk_group_dn$age_group == this_age]
        if (P != (P_general+P_risk)){stop('Line 15 P_general + P_risk != P_overall')}
        
        IR_gen = ((IR*P)/((RR_estimate*P_risk)/P_general +1))/P_general
        IR_risk = ((IR*P)/(P_general/(RR_estimate*P_risk) +1))/P_risk
        
        row_gen = row %>% mutate(pop_est = IR_gen,risk_group = 'general_public')
        row_risk = row %>% mutate(pop_est = IR_risk,risk_group = risk_group_name)
        severe_outcome_country_level_wRisk = rbind(severe_outcome_country_level_wRisk,row_gen,row_risk)
      }
    }
    severe_outcome_country_level_wRisk$pop_est[is.na(severe_outcome_country_level_wRisk$pop_est)]=0
    severe_outcome_country_level = severe_outcome_country_level_wRisk
  }
  
  #join VE over time with severe outcome projections
  if (length(risk_group_labels)>1){
    severe_outcome_this_run = severe_outcome_country_level %>% 
      left_join(VE_tracker, by = c("age_group", "outcome_VE", "risk_group")) %>%
      mutate(pop_est = pop_est*(1-VE)) %>%
      select(date,outcome,age_group,risk_group,vaccine_type,dose,pop_est)
  } else{
    severe_outcome_this_run = severe_outcome_country_level %>% 
      left_join(VE_tracker, by = c("age_group", "outcome_VE")) %>%
      mutate(pop_est = pop_est*(1-VE)) %>%
      select(date,outcome,age_group,risk_group,vaccine_type,dose,pop_est)
  }
  rm(IR_gen, IR_risk, row_gen, row_risk, severe_outcomes_list, severe_outcome_country_level_wRisk, row, this_age, P, P_general, P_risk, VE_tracker, severe_outcome_country_level,workshop)
  ########################################################################################################   
  
  
  ### PART SEVEN: Final calc ########################################################################   
  #(A) infection-derived immunity against severe outcomes
  rho_SO_est = rbeta(1,rho_SO_sample$beta_a, rho_SO_sample$beta_b)
  
  reinfection_protection = exposed_log %>%
    mutate(protection = reinfection_ratio * toggle_reinfection_ratio_multiplier * rho_SO_est) %>%
    select(date,age_group,protection)
  
  #(B)Join incidence_log_tidy with severe outcome incidence by vax status
  workshop = severe_outcome_this_run %>%
    left_join(incidence_log_tidy, by = c("date", "age_group", "risk_group", "vaccine_type", "dose")) %>%
    mutate(proj = incidence*pop_est/100) %>%
    left_join(reinfection_protection, by = c("date", "age_group")) %>%
    mutate(proj = proj*(1-protection))
  
  severe_outcome_log_tidy = workshop %>% 
    select(date,risk_group,age_group,dose,vaccine_type,outcome,proj) %>%
    filter(date< as.Date('2024-01-01')) #considering impacts within 2023 only!
  
  if (toggle_intervention == 'antiviral'){
    severe_outcome_log_tidy = severe_outcome_log_tidy %>% 
      filter(date >= date_start) #considering severe outcomes after antiviral start date only!!! and within 2023!
  } 
  ######################################################################################################## 
  
  
  ### PART EIGHT: Translate to antiviral model dependencies ##############################################
  outcomes_without_antivirals = severe_outcome_log_tidy  %>%
    group_by(outcome) %>%
    summarise(overall = sum(proj,na.rm=TRUE),.groups = "keep")
  #adding some extra detail
  append_high_risk = severe_outcome_log_tidy  %>%
    filter(risk_group == risk_group_name) %>%
    group_by(outcome) %>%
    summarise(high_risk = sum(proj,na.rm=TRUE),.groups = "keep")
  outcomes_without_antivirals = outcomes_without_antivirals %>% left_join(append_high_risk, by = 'outcome')
  
  likelihood_severe_outcome = severe_outcome_this_run %>%
    left_join(reinfection_protection, by = c("date", "age_group")) %>%
    mutate(pop_est = pop_est*(1-protection)) %>%
    select(-protection) %>%
    left_join(prop_sympt_LOCAL,by= c('age_group' = 'agegroup')) %>%
    mutate(pop_est = pop_est * (1/value)) %>%
    select(-value) %>%
    rename(percentage = pop_est)
  
  result = list(outcomes_without_antivirals = outcomes_without_antivirals,
                likelihood_severe_outcome = likelihood_severe_outcome) 
  ########################################################################################################
  
  return(result)
}
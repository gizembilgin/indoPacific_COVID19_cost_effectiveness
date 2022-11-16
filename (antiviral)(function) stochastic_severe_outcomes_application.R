### This program applied sampled values related to severe outcome projections to a given scenario
### It combines the components of two scripts from the original transmission model:
### (1) (5)_severe_outcomes_calc
### (2) (6)_severe_outcome_proj


stochastic_severe_outcomes_application <- function(
    incidence_log_tidy           = this_incidence_log_tidy,
    vaccination_history_FINAL    = this_vaccination_history_FINAL,
    exposed_log                  = this_exposed_log,
    risk_group_name              = toggle_vax_scenario_risk_group,
    prop_sympt_LOCAL             = prop_sympt,
    VE_waning_distribution       = sampled_VE_waning_distribution,
    severe_outcome_country_level = sampled_severe_outcome_country_level,
    rho_SO_est                   = sampled_rho_SO_est,
    
    num_time_steps   = 365,
    strain_now       = 'omicron',
    date_start       = as.Date('2023-01-01'),
    age_groups_num   = c(0,4,9,17,29,44,59,69,110),
    age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100')

){
  
  risk_group_labels = c('general_public',risk_group_name) 
  sensitivity_analysis_toggles = list(VE_older_adults = "reduced",VE_adults_comorb = 0.9) #ALWAYS HAVE LOWER VE IN OLDER ADULTS AND ADULTS WITH COMORB IN ANTIVIRAL MODEL

  ### PART ONE: CALCULATE VE AGAINST SEVE OUTCOMES BY DAY #####################################################
  VE_tracker = data.frame()
  for (outcome in c('death','severe_disease')){
    for (day in 0:num_time_steps){
      workshop = VE_time_step(strain_now,
                              date_start+day,
                              outcome,
                              VE_waning_LOCAL=VE_waning_distribution, 
                              vaccination_history_LOCAL = vaccination_history_FINAL,
                              SA_toggles_local = sensitivity_analysis_toggles)
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
  
  if (risk_group_name == 'adults_with_comorbidities'){
    VE_tracker =  VE_tracker %>%
      mutate(VE = case_when(
        risk_group == risk_group_name & age_group %in% c("30 to 44","45 to 59") ~ VE * sensitivity_analysis_toggles$VE_adults_comorb,
        TRUE ~ VE
      ))
  }
  ########################################################################################################   
  
  
  ### PART TWO: Apply VE ########################################################################
  #join VE over time with severe outcome projections
  if (length(risk_group_labels)>1){
    severe_outcome_this_run = severe_outcome_country_level %>% 
      left_join(VE_tracker, by = c("age_group", "outcome_VE", "risk_group")) %>%
      mutate(percentage = percentage*(1-VE)) %>%
      select(date,outcome,age_group,risk_group,vaccine_type,dose,percentage)
  } else{
    severe_outcome_this_run = severe_outcome_country_level %>% 
      left_join(VE_tracker, by = c("age_group", "outcome_VE")) %>%
      mutate(percentage = percentage*(1-VE)) %>%
      select(date,outcome,age_group,risk_group,vaccine_type,dose,percentage)
  }
  rm(VE_tracker, severe_outcome_country_level,workshop)
  ########################################################################################################   
  
  
  ### PART THREE: Final calc ########################################################################   
  #(A) infection-derived immunity against severe outcomes
  reinfection_protection = exposed_log %>%
    mutate(protection = reinfection_ratio * rho_SO_est) %>%
    select(date,age_group,protection)
  
  #(B)Join incidence_log_tidy with severe outcome incidence by vax status
  workshop = severe_outcome_this_run %>%
    left_join(incidence_log_tidy, by = c("date", "age_group", "risk_group", "vaccine_type", "dose")) %>%
    mutate(proj = incidence*percentage) %>%
    left_join(reinfection_protection, by = c("date", "age_group")) %>%
    mutate(proj = proj*(1-protection))
  
  severe_outcome_log_tidy = workshop %>% 
    select(date,risk_group,age_group,dose,vaccine_type,outcome,proj) %>%
    filter(date< as.Date('2024-01-01')) #considering impacts within 2023 only!
  
  likelihood_severe_outcome = severe_outcome_this_run %>%
    left_join(reinfection_protection, by = c("date", "age_group")) %>%
    mutate(percentage = percentage*(1-protection)) %>%
    select(-protection) %>%
    left_join(prop_sympt_LOCAL,by= c('age_group' = 'agegroup')) %>%
    mutate(percentage = percentage * (1/value)) %>%
    select(-value)
  
  result = list(severe_outcome_log_tidy = severe_outcome_log_tidy,
                likelihood_severe_outcome = likelihood_severe_outcome) 
  ########################################################################################################
  
  return(result)
}
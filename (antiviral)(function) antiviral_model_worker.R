### This function runs the model as per the request of antiviral_model_manager.
### The function is structured so that severe outcome estimates are sampled once, and are consistent across vaccine scenarios.
### Antiviral start dates are nestled within vaccine scenarios to save computational energy. 

antiviral_model_worker <- function(
    manager_scenario_dataframe,
    RECORD_antiviral_setup,
  
    #copy all variables into the local memory of the function
    local_LIST_antiviral_target_group = LIST_antiviral_target_group,
    local_LIST_antiviral_start_date = LIST_antiviral_start_date,
    local_LIST_antiviral_type = LIST_antiviral_type,
    local_stochastic_SO = toggle_stochastic_SO,
    local_compare_to_vaccine_effect = toggle_compare_to_vaccine_effect,
    local_sensitivity_analysis = toggle_sensitivity_analysis,
    local_pathway_to_care = pathway_to_care,
    local_antiviral_delivery_capacity = toggle_antiviral_delivery_capacity,
    local_fixed_antiviral_coverage = toggle_fixed_antiviral_coverage,
    local_antiviral_effectiveness = antiviral_effectiveness,
    local_booster_start_date = as.Date('2023-01-01'),
    
    worker_stochastic_VE_sampling = "normal",
  
    stochastic_severe_outcomes_sampling = copy_sampling_fx_into_cluster,
    stochastic_severe_outcomes_application = copy_application_fx_into_cluster
){
  
  ### INITALISE
  this_worker_result = data.frame()
  if (local_compare_to_vaccine_effect == "on"){
    manager_scenario_dataframe$vax_scenario = factor(manager_scenario_dataframe$vax_scenario, levels = c('all willing adults vaccinated with a primary schedule',
                                                                                                         'all willing adults vaccinated with a primary schedule and high risk group recieve a booster', 
                                                                                                         'all willing adults vaccinated with a primary schedule plus booster dose'))
    manager_scenario_dataframe = manager_scenario_dataframe %>% arrange(vax_scenario) # order
  }
  
  
  ### SAMPLE
  # Sample antiviral effectiveness by type
  toggle_antiviral_effectiveness = local_antiviral_effectiveness %>%
    filter(antiviral_type %in% local_LIST_antiviral_type)
  sampled_value = mapply(rbeta,1,toggle_antiviral_effectiveness$beta_a, toggle_antiviral_effectiveness$beta_b)
  toggle_antiviral_effectiveness = cbind(toggle_antiviral_effectiveness,sampled_value) 
  toggle_antiviral_effectiveness = toggle_antiviral_effectiveness %>% 
    select(antiviral_type,outcome,sampled_value) %>%
    rename(AE = sampled_value)
  YLL_addition = toggle_antiviral_effectiveness %>% filter(outcome == 'death') %>% mutate(outcome = 'YLL')
  toggle_antiviral_effectiveness = rbind(toggle_antiviral_effectiveness,YLL_addition)
  
  # Sample parameters related to severe outcome projections
  if (local_stochastic_SO == "on"){
    input_vaccine_type_list = unique(RECORD_antiviral_setup$vaccination_history_FINAL$vaccine_type)
    
    SO_sample = stochastic_severe_outcomes_sampling( vaccine_type_list = input_vaccine_type_list,
                                                     risk_group_name = unique(manager_scenario_dataframe$vax_scenario_risk_group),
                                                     local_stochastic_VE_sampling = worker_stochastic_VE_sampling)
    sampled_severe_outcome_country_level = SO_sample$SAVE_severe_outcome_country_level
    sampled_VE_waning_distribution = SO_sample$SAVE_VE_waning_distribution
    sampled_rho_SO_est = SO_sample$SAVE_rho_SO_est
  }
  #_______________________________________________________
  
  
  
  ### RUN OVER SIMULATIONS
  for (run_number in 1:nrow(manager_scenario_dataframe)){
    
    ### LOAD VAX SCENARIO ########################################################
    toggle_vax_scenario            = manager_scenario_dataframe$vax_scenario[run_number]
    toggle_vax_scenario_risk_group = manager_scenario_dataframe$vax_scenario_risk_group[run_number]
    
    outcomes_without_antivirals = RECORD_antiviral_setup$outcomes_without_antivirals %>%
      filter(
        vax_scenario            == toggle_vax_scenario,
        vax_scenario_risk_group == toggle_vax_scenario_risk_group
      )
    likelihood_severe_outcome = RECORD_antiviral_setup$likelihood_severe_outcome %>%
      filter(
        vax_scenario            == toggle_vax_scenario,
        vax_scenario_risk_group == toggle_vax_scenario_risk_group
      ) 
    this_incidence_log_tidy = RECORD_antiviral_setup$incidence_log_tidy %>%
      filter(
        vax_scenario            == toggle_vax_scenario,
        vax_scenario_risk_group == toggle_vax_scenario_risk_group
      )
    this_incidence_log = RECORD_antiviral_setup$incidence_log %>%
      filter(
        vax_scenario            == toggle_vax_scenario,
        vax_scenario_risk_group == toggle_vax_scenario_risk_group
      )
    this_exposed_log= RECORD_antiviral_setup$exposed_log %>%
      filter(
        vax_scenario            == toggle_vax_scenario,
        vax_scenario_risk_group == toggle_vax_scenario_risk_group
      )
    this_vaccination_history_FINAL = RECORD_antiviral_setup$vaccination_history_FINAL %>%
      filter(
        vax_scenario            == toggle_vax_scenario,
        vax_scenario_risk_group == toggle_vax_scenario_risk_group
      )
    prop_sympt = RECORD_antiviral_setup$prop_sympt
    #____________________________________________________________________________
    
    
    
    ### CALCULATE SEVERE OUTCOME PROJECTIONS FOR THIS SCENARIO ###################
    if (local_stochastic_SO == "on"){
      SO_applied = stochastic_severe_outcomes_application (
        incidence_log_tidy = this_incidence_log_tidy,
        vaccination_history_FINAL = this_vaccination_history_FINAL,
        exposed_log = this_exposed_log,
        
        risk_group_name = toggle_vax_scenario_risk_group,
        prop_sympt_LOCAL = prop_sympt,
        VE_waning_distribution = sampled_VE_waning_distribution,
        severe_outcome_country_level = sampled_severe_outcome_country_level,
        rho_SO_est = sampled_rho_SO_est
      )
      severe_outcome_log_tidy = SO_applied$severe_outcome_log_tidy
      likelihood_severe_outcome = SO_applied$likelihood_severe_outcome
    }
    #____________________________________________________________________________
    
    
    
    ### ESTIMATE IMPACT OF ANTIVIRALS ##########################################
    prevented_by_antivirals = data.frame()
    for (a in 1:length(local_LIST_antiviral_type)){
      this_antiviral_effectiveness = toggle_antiviral_effectiveness %>% filter(antiviral_type == local_LIST_antiviral_type[[a]])
    for (b in 1:length(local_LIST_antiviral_start_date)){
      toggle_antiviral_start_date = local_LIST_antiviral_start_date[[b]]
      
      for (c in 1:length(local_LIST_antiviral_target_group)){
        toggle_antiviral_target = local_LIST_antiviral_target_group[[c]]
        #CHECK
        if (toggle_antiviral_target %in% c('adults_with_comorbidities', 'pregnant_women')) {
          if (!toggle_antiviral_target %in% unique(this_incidence_log_tidy$risk_group)) {
            stop('target for antivirals not included as a risk group in model run!')
          }
        }
        
        ### Calculate outcomes without antivirals
        if (local_stochastic_SO == "on"){
          save_booster_dose_info = outcomes_without_antivirals %>% filter(outcome == 'booster_doses_delivered')
          outcomes_without_antivirals = severe_outcome_log_tidy  %>%
            filter(date >= toggle_antiviral_start_date) %>%
            group_by(outcome) %>%
            summarise(overall = sum(proj,na.rm=TRUE),.groups = "keep")
          #adding some extra detail
          append_high_risk = severe_outcome_log_tidy  %>%
            filter(date >= toggle_antiviral_start_date) %>%
            filter(risk_group == toggle_vax_scenario_risk_group) %>%
            group_by(outcome) %>%
            summarise(high_risk = sum(proj,na.rm=TRUE),.groups = "keep")
          outcomes_without_antivirals = outcomes_without_antivirals %>% left_join(append_high_risk, by = 'outcome')
          outcomes_without_antivirals = rbind(outcomes_without_antivirals,save_booster_dose_info)
          
          if (toggle_vax_scenario == 'all willing adults vaccinated with a primary schedule') {
            OWA_no_booster_doses = severe_outcome_log_tidy  %>%
              filter(date >= local_booster_start_date) %>%
              group_by(outcome) %>%
              summarise(overall = sum(proj, na.rm = TRUE), .groups = "keep")
            append_high_risk = severe_outcome_log_tidy  %>%
              filter(date >= local_booster_start_date) %>%
              filter(risk_group == toggle_vax_scenario_risk_group) %>%
              group_by(outcome) %>%
              summarise(high_risk = sum(proj, na.rm = TRUE), .groups = "keep")
            OWA_no_booster_doses = OWA_no_booster_doses %>% left_join(append_high_risk, by = 'outcome')
            OWA_no_booster_doses = rbind(OWA_no_booster_doses, save_booster_dose_info)
          } else{
            OWA_with_booster_doses = severe_outcome_log_tidy  %>%
              filter(date >= local_booster_start_date) %>%
              group_by(outcome) %>%
              summarise(overall = sum(proj, na.rm = TRUE), .groups = "keep")
            append_high_risk = severe_outcome_log_tidy  %>%
              filter(date >= local_booster_start_date) %>%
              filter(risk_group == toggle_vax_scenario_risk_group) %>%
              group_by(outcome) %>%
              summarise(high_risk = sum(proj, na.rm = TRUE), .groups = "keep")
            OWA_with_booster_doses = OWA_with_booster_doses %>% left_join(append_high_risk, by = 'outcome')
            OWA_with_booster_doses = rbind(OWA_with_booster_doses, save_booster_dose_info)
          }
        } else if (local_stochastic_SO == "off") {
          OWA_no_booster_doses = RECORD_antiviral_setup$outcomes_without_antivirals %>%
            filter(
              vax_scenario            == 'all willing adults vaccinated with a primary schedule',
              vax_scenario_risk_group == toggle_vax_scenario_risk_group
            )
          
          OWA_with_booster_doses = RECORD_antiviral_setup$outcomes_without_antivirals %>%
            filter(
              vax_scenario            == toggle_vax_scenario,
              vax_scenario_risk_group == toggle_vax_scenario_risk_group
            )
        } 
        #carry baseline of outcomes with no booster or antiviral to calculate vax effect
  
        
        
        ### SELECT TARGET GROUP ######################################################
        antiviral_target = this_incidence_log_tidy %>%
          filter(!age_group %in% c("0 to 4", "5 to 9", "10 to 17")) #ASSUME ADULTS ONLY
        
        if (toggle_antiviral_target %in% c('adults_with_comorbidities', 'pregnant_women')) {
          antiviral_target = antiviral_target %>%
            filter(risk_group == toggle_antiviral_target)
        } else if (toggle_antiviral_target == 'unvaccinated_adults') {
          antiviral_target = antiviral_target %>%
            filter(dose == 0)
        } else if (toggle_antiviral_target == 'all_adults') {
          
        } else if (toggle_antiviral_target == 'unvaccinated_adults_AND_adults_with_comorbidities') {
          antiviral_target = antiviral_target %>%
            filter(dose == 0 | risk_group == 'adults_with_comorbidities')
        } else {
          stop('pick a valid toggle_antiviral_target!')
        }
        antiviral_target = antiviral_target %>% filter(incidence > 0,
                                                       date >= toggle_antiviral_start_date) #COMEBACK - plus or minus 5?
        
        antiviral_delivery_length = as.numeric(max(this_incidence_log_tidy$date) - toggle_antiviral_start_date)
        #____________________________________________________________________________
        
        
        ### SELECT SYMPTOMATIC INDIVIDUAL#############################################
        #ASSUMPTION: vaccination does not affect the likelihood of an individual being symptomatic
        antiviral_target = antiviral_target %>%
          left_join(prop_sympt, by = c('age_group' = 'agegroup')) %>%
          mutate(symptomatic = incidence * value) %>%
          select(-incidence,-value)
        
        antiviral_target_individuals = antiviral_target %>%
          mutate(symptomatic = round(symptomatic)) %>%
          select(-temp_risk) %>%
          filter(symptomatic > 0)
        
        rm(antiviral_target)
        #____________________________________________________________________________
        
        
        ### CREATE DATASET WITH INDIVIDUALS ############################################
        workshop = as.data.frame(
          lapply(
            antiviral_target_individuals,
            rep,
            antiviral_target_individuals$symptomatic
          )
        )
        if (!nrow(workshop) == sum(antiviral_target_individuals$symptomatic)) {
          stop('split to individuals has lost individuals')
        }
        
        antiviral_target_individuals = workshop %>% select(-symptomatic)
        antiviral_target_individuals$ID <-
          seq.int(nrow(antiviral_target_individuals)) #helpful for checking
        
        total_target = nrow(antiviral_target_individuals)
        #____________________________________________________________________________
        
        
  
        if (local_pathway_to_care == 'realistic') {
          ### PATHWAY TO CARE STEP ONE: Does this individual seek care? ################
          healthcare_seeking = function(age_group) {
            #COMEBACK: need real data to estimate
            if (age_group %in% c("0 to 4",    "5 to 9",    "10 to 17")) {
              sample = rbinom(1, 1, 0.5) #rbinom(number of observations,number of trials,probability of success on each trial)
            } else{
              sample = rbinom(1, 1, 0.25)
            }
            return(sample)
          }
          
          workshop <-
            as.data.frame(sapply(
              antiviral_target_individuals$age_group,
              healthcare_seeking
            ))
          colnames(workshop) = c('healthcare_seeking')
          workshop = cbind(antiviral_target_individuals, workshop)
          
          antiviral_target_individuals_run = workshop %>%
            filter(healthcare_seeking == 1) %>% #retain those who seek care
            select(-healthcare_seeking)
          
          rm(workshop)
          #____________________________________________________________________________
            
            
          ### PATHWAY TO CARE STEP TWO: How many days after symptom onset can the individual access care?#######
          healthcare_access = function(age_group) { #ASSUMPTION: incidence is day 0 of symptom onset
            #COMEBACK: need real data to estimate
            if (age_group %in% c("0 to 4",    "5 to 9",    "10 to 17")) {
              sample = round(runif(1, min = 0, max = 7))
            } else{
              sample = round(runif(1, min = 0, max = 7))
            }
            return(sample)
          }
          
          workshop <-
            as.data.frame(sapply(
              antiviral_target_individuals_run$age_group,
              healthcare_access
            ))
          colnames(workshop) = c('healthcare_access')
          workshop = cbind(antiviral_target_individuals_run, workshop)
          
          antiviral_target_individuals_run = workshop %>%
            filter(healthcare_access < 6) %>% #retain those who access care within 5 days
            mutate(min_date_access = date + healthcare_access,
                   max_date_access = date + 5) %>%
            select(-healthcare_access)
          
          rm(workshop)
          #____________________________________________________________________________
            
            
          ### PATHWAY TO CARE STEP THREE: Is the individual allocated antivirals?#######
          antiviral_delivery_tracker = data.frame()
          
          for (day in 0:antiviral_delivery_length) { #ASSUMPTION: allocated at random
            this_date =  toggle_antiviral_start_date + day
            
            #include all presenting within
            presentations_this_date = antiviral_target_individuals_run %>%
              filter(this_date >= min_date_access &
                       this_date <= max_date_access) %>%
              filter(!ID %in% antiviral_delivery_tracker$ID) #make sure previous recipients removed from future decisions
            
            num_to_sample = min(nrow(presentations_this_date),
                                local_antiviral_delivery_capacity) #making sure number of doses delivered <= number of available recipients
            
            antiviral_recipients_this_date = data.frame(ID = sample(presentations_this_date$ID, num_to_sample, replace =
                                                                      FALSE))
            antiviral_recipients_this_date = antiviral_recipients_this_date %>% mutate(date = this_date)
            antiviral_delivery_tracker = rbind(antiviral_delivery_tracker,
                                               antiviral_recipients_this_date)
          }
          
          antiviral_target_individuals_run = antiviral_delivery_tracker %>%
            select(-date) %>%
            left_join(antiviral_target_individuals_run, by = 'ID') #remove all not selected for antivirals
          rm(antiviral_delivery_tracker)
          #____________________________________________________________________________
          
        } else if (local_pathway_to_care == 'fixed') {
          #randomly sample the fixed proportion from the target population
          num_to_sample = total_target * local_fixed_antiviral_coverage
          antiviral_recipients = data.frame(ID = sample(antiviral_target_individuals$ID, num_to_sample, replace = FALSE))
          
          antiviral_target_individuals_run = antiviral_recipients %>%
            left_join(antiviral_target_individuals, by = 'ID') #remove all not selected for antivirals
          
        } else{
          stop('select a valid pathway_to_care!')
          
        }
        length_antiviral_delivery_tracker = nrow(antiviral_target_individuals_run)
          
          
        ### PATHWAY TO CARE STEP FOUR: How many cases of severe disease are prevented?#######
        workshop = antiviral_target_individuals_run %>%
          left_join(
            likelihood_severe_outcome,
            by = c("date", "risk_group", "age_group", "dose", "vaccine_type")
          ) %>%
          left_join(this_antiviral_effectiveness, by = 'outcome') %>%
          mutate(percentage = percentage * AE)
        workshop = na.omit(workshop)
        
        prevented_by_antivirals_this_date = workshop %>%
          group_by(outcome) %>%
          summarise(n = sum(percentage)) %>%
          mutate(antiviral_start_date = toggle_antiviral_start_date) %>% 
          mutate(antiviral_type = local_LIST_antiviral_type[[a]],
                 antiviral_target_group = toggle_antiviral_target,
                 intervention = paste('antiviral',toggle_antiviral_start_date), 
                 evaluation_group = 'overall',
                 intervention_doses_delivered = length_antiviral_delivery_tracker)
        #____________________________________________________________________________
        
        prevented_by_antivirals = rbind(prevented_by_antivirals,prevented_by_antivirals_this_date)
      }
      }
    } #end loop over different antiviral start dates, antiviral types, and antiviral target groups

    
    
    ### ESTIMATE ISOLATED EFFECT OF VACCINE #####################################
    if (local_compare_to_vaccine_effect == "on" & toggle_vax_scenario != 'all willing adults vaccinated with a primary schedule'){
      #Step One: load outcomes_without_antivirals (OWA) of no booster dose scenario
      workshop = OWA_no_booster_doses %>%
        select(-vax_scenario,-vax_scenario_risk_group) %>%
        rename(no_booster_overall = overall,
               no_booster_high_risk = high_risk)
      
      #Step Two: compare OWA between booster and no-booster scenario
      vax_effect_comparison = OWA_with_booster_doses %>% 
        select(-vax_scenario,-vax_scenario_risk_group) %>%
        left_join(workshop, by = c('outcome')) %>%
        mutate(overall = no_booster_overall - overall,
               high_risk = no_booster_high_risk - high_risk) %>%
        select(-no_booster_high_risk, -no_booster_overall) %>%
        pivot_longer(
          cols = c('overall','high_risk') ,
          names_to = 'evaluation_group',
          values_to = 'n'
        ) %>% 
        mutate(intervention = 'vaccine')
      
      workshop = vax_effect_comparison %>% filter(outcome == 'booster_doses_delivered') %>% group_by(intervention,evaluation_group) %>% summarise(intervention_doses_delivered = abs(mean(n)),.groups = "keep")
      vax_effect_comparison = vax_effect_comparison %>%
        filter(outcome != 'booster_doses_deliverd') %>%
        left_join(workshop, by = c("evaluation_group", "intervention"))
      
      prevented_by_antivirals = bind_rows(prevented_by_antivirals,vax_effect_comparison)
      
    }
    #____________________________________________________________________________
    

  
    ### CREATE OUTPUT #################################################
    #select dataset to be used as comparison to calculate percentage
    #if comparing vaccine effect to antiviral effect than compare to no vax no antiviral
    if (local_compare_to_vaccine_effect == "on" ){
      #NOTE: if comparing vax and antiviral, then baseline is no booster and no antiviral; otherwise, baseline is only no antiviral
      outcomes_without_antivirals = OWA_no_booster_doses %>%
        pivot_longer(
          cols = c('overall','high_risk') ,
          names_to = 'evaluation_group',
          values_to = 'overall'
        ) %>%
        select(outcome,evaluation_group,overall)
    
    #if comparing between antiviral effects than compare to same vaccine scenario no antiviral
    } else if (local_compare_to_vaccine_effect != "on"){
      outcomes_without_antivirals = OWA_with_booster_doses %>%
        pivot_longer(
          cols = c('overall','high_risk') ,
          names_to = 'evaluation_group',
          values_to = 'overall'
        ) %>%
        select(outcome,evaluation_group,overall)
    }
    
    one_complete_run <-
      prevented_by_antivirals %>%
      left_join(outcomes_without_antivirals, by = c("outcome",'evaluation_group')) %>%
      mutate(
        percentage = n / overall * 100
      ) %>%
      select(outcome, antiviral_type,antiviral_target_group,intervention,evaluation_group, intervention_doses_delivered, n,
             percentage) %>% 
      mutate(vax_scenario = toggle_vax_scenario,
             vax_scenario_risk_group = toggle_vax_scenario_risk_group)
    
    this_worker_result = rbind(one_complete_run, this_worker_result)
    
  } #end vaccination scenario loop
  
   return(this_worker_result)
}
load(file = '1_inputs/antiviral_effectiveness.Rdata' )

source(paste(getwd(),"/(antiviral)(function) stochastic severe outcome projections.R",sep=""))
source(paste(getwd(),"/(antiviral)(sensitivity)(function) severe outcome projections.R",sep=""))

antiviral_model <- function(toggle_antiviral_start_date, 
                            toggle_antiviral_type,
                            toggle_antiviral_target,
                            toggle_vax_scenario,
                            toggle_vax_scenario_risk_group,
                            
                            RECORD_antiviral_setup,
                            
                            toggle_number_of_runs = 100,
                            toggle_stochastic_SO = "off",
                            toggle_sensitivity_analysis = list(),
                            toggle_compare_to_vaccine_effect = "off",
                            toggle_antiviral_delivery_capacity = NA,
                            toggle_fixed_antiviral_coverage = NA
                            ){
  #CAUTION - if toggle_antiviral_start_date > 01/01/2023 doses per outcome averted okay, but percentage of pop averted not accurate (needs to be restricted to this timeframe), potential
  # for quick fix if stochatic_SO
  
  
  ###SKIP IF NONSENSE
  skip = 0
  if (toggle_vax_scenario_risk_group == 'none' & 
      toggle_vax_scenario == 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster'){
    #require risk group to exist to be able to deliver additional booster doses
    skip = 1
  } 
  if (toggle_antiviral_target %in% c('pregnant_women','adults_with_comorbidities') &
      toggle_vax_scenario_risk_group != toggle_antiviral_target){
    #require risk group name to exist to be able to deliver antivirals
    skip = 1
  }

  if (skip == 1){
    return(NULL)
  } else{
    ###LOAD DEPENDENCIES
    #DEPENDENCIES FROM PRIMARY MODEL (n=5): incidence_log_tidy, severe_outcome_log_tidy, severe_outcome_this_run, reinfection_protection, param_age (-> prop_sympt)
    #DEPENDENCIES FROM '(antiviral) set up.R' (n=4): incidence_log_tidy,outcomes_without_antivirals,likelihood_severe_outcome,prop_sympt
    
    outcomes_without_antivirals = RECORD_antiviral_setup$outcomes_without_antivirals %>%
      filter(
        vax_scenario            == toggle_vax_scenario,
        vax_scenario_risk_group == toggle_vax_scenario_risk_group
      )
    likelihood_severe_outcome = RECORD_antiviral_setup$likelihood_severe_outcome %>%
      filter(
        vax_scenario            == toggle_vax_scenario,
        vax_scenario_risk_group == toggle_vax_scenario_risk_group
      ) %>%
      filter(date >= toggle_antiviral_start_date)
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
    vaccination_history_FINAL = RECORD_antiviral_setup$vaccination_history_FINAL %>%
      filter(
        vax_scenario            == toggle_vax_scenario,
        vax_scenario_risk_group == toggle_vax_scenario_risk_group
      )
    prop_sympt = RECORD_antiviral_setup$prop_sympt
    
    #CHECK
    if (toggle_antiviral_target %in% c('adults_with_comorbidities', 'pregnant_women')) {
      if (!toggle_antiviral_target %in% unique(this_incidence_log_tidy$risk_group)) {
        stop('target for antivirals not included as a risk group in model run!')
      }
    }
    #____________________________________________________________________________
    
    
    ### INITALISE DATA FRAMES #################################################################
    this_scenario_tracker = data.frame()
    if (toggle_compare_to_vaccine_effect == "on"){
      vax_effect_tracker = data.frame()
    }
    #____________________________________________________________________________
    
    
    
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
    
    
    ###### BEGIN STOCHASTIC
    if (length(toggle_sensitivity_analysis)>0){
      severe_outcome_proj_multiplier = 1
      if ('toggle_severe_outcome_proj_multiplier' %in% names(toggle_sensitivity_analysis)){severe_outcome_proj_multiplier = toggle_sensitivity_analysis$toggle_severe_outcome_proj_multiplier}
      reinfection_ratio_multiplier = 1
      if ('toggle_reinfection_ratio_multiplier' %in% names(toggle_sensitivity_analysis))  {reinfection_ratio_multiplier   = toggle_sensitivity_analysis$toggle_reinfection_ratio_multiplier}
      VE_multiplier = 1
      if ('toggle_VE_multiplier' %in% names(toggle_sensitivity_analysis))                 {VE_multiplier                  = toggle_sensitivity_analysis$toggle_VE_multiplier}
      
      
      load_SO = sensitivity_severe_outcomes(
        incidence_log = this_incidence_log,
        incidence_log_tidy = this_incidence_log_tidy,
        vaccination_history_FINAL = vaccination_history_FINAL,
        exposed_log = this_exposed_log,
        
        setting = 'SLE',
        num_time_steps = 365,
        strain_now = 'omicron',
        risk_group_name = toggle_vax_scenario_risk_group,
        date_start = toggle_antiviral_start_date,
        prop_sympt_LOCAL = prop_sympt,
        
        toggle_severe_outcome_proj_multiplier = severe_outcome_proj_multiplier,
        toggle_reinfection_ratio_multiplier = reinfection_ratio_multiplier,
        toggle_VE_multiplier = VE_multiplier
      )
      save_booster_dose_info = outcomes_without_antivirals %>% filter(outcome == 'booster_doses_delivered')
      outcomes_without_antivirals = rbind(load_SO$outcomes_without_antivirals,save_booster_dose_info)
      likelihood_severe_outcome   = load_SO$likelihood_severe_outcome
    }
    
    for (run_number in 1:toggle_number_of_runs) {
      
      if (toggle_stochastic_SO == "on"){
        # Sample severe outcome likelihood
        load_SO = stochastic_severe_outcomes(
          incidence_log = this_incidence_log,
          incidence_log_tidy = this_incidence_log_tidy,
          vaccination_history_FINAL = vaccination_history_FINAL,
          exposed_log = this_exposed_log,
          
          setting = 'SLE',
          num_time_steps = 365,
          strain_now = 'omicron',
          risk_group_name = toggle_vax_scenario_risk_group,
          date_start = toggle_antiviral_start_date,
          prop_sympt_LOCAL = prop_sympt
        )
        save_booster_dose_info = outcomes_without_antivirals %>% filter(outcome == 'booster_doses_delivered')
        outcomes_without_antivirals = rbind(load_SO$outcomes_without_antivirals,save_booster_dose_info)
        likelihood_severe_outcome   = load_SO$likelihood_severe_outcome
      }
      
      # Sample antiviral effectiveness by type
      toggle_antiviral_effectiveness = antiviral_effectiveness %>%
        filter(antiviral_type == toggle_antiviral_type)
      sampled_value = mapply(rbeta,1,toggle_antiviral_effectiveness$beta_a, toggle_antiviral_effectiveness$beta_b)
      toggle_antiviral_effectiveness = cbind(toggle_antiviral_effectiveness,sampled_value) 
      toggle_antiviral_effectiveness = toggle_antiviral_effectiveness %>% 
        select(antiviral_type,outcome,sampled_value) %>%
        rename(AE = sampled_value)
      YLL_addition = toggle_antiviral_effectiveness %>% filter(outcome == 'death') %>% mutate(outcome = 'YLL')
      toggle_antiviral_effectiveness = rbind(toggle_antiviral_effectiveness,YLL_addition)
      
      
      if (pathway_to_care == 'realistic'){
        
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
        #ASSUMPTION: incidence is day 0 of symptom onset
        
        healthcare_access = function(age_group) {
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
        #ASSUMPTION: allocated at random
        antiviral_delivery_tracker = data.frame()
        
        for (day in 0:antiviral_delivery_length) {
          this_date =  toggle_antiviral_start_date + day
          
          #include all presenting within
          presentations_this_date = antiviral_target_individuals_run %>%
            filter(this_date >= min_date_access &
                     this_date <= max_date_access) %>%
            filter(!ID %in% antiviral_delivery_tracker$ID) #make sure previous recipients removed from future decisions
          
          num_to_sample = min(nrow(presentations_this_date),
                              toggle_antiviral_delivery_capacity) #making sure number of doses delivered <= number of available recipients
          
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
      } else if (pathway_to_care == 'fixed'){
        
        #randomly sample the fixed proportion from the target population
        num_to_sample = total_target * toggle_fixed_antiviral_coverage
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
        left_join(toggle_antiviral_effectiveness, by = 'outcome') %>%
        mutate(percentage = percentage * AE)
      workshop = na.omit(workshop)
      
      prevented_by_antivirals = workshop %>%
        group_by(outcome) %>%
        summarise(n = sum(percentage))
      
      this_scenario_tracker = rbind(this_scenario_tracker, prevented_by_antivirals)
      #____________________________________________________________________________
      
      
      ### ESTIMATE ISOLATED EFFECT OF VACCINE #####################################
      if (toggle_compare_to_vaccine_effect == "on" & toggle_vax_scenario != 'all willing adults vaccinated with a primary schedule'){
        #Step One: load outcomes_without_antivirals (OWA) of no booster dose scenario
        if (toggle_stochastic_SO == "off"){
          if (run_number == 1){
            vax_effect_OWA = RECORD_antiviral_setup$outcomes_without_antivirals %>%
              filter(
                vax_scenario            == 'all willing adults vaccinated with a primary schedule',
                vax_scenario_risk_group == toggle_vax_scenario_risk_group
              )

            vax_effect_OWA = vax_effect_OWA %>% 
              rename(no_booster_overall = overall,
                     no_booster_high_risk = high_risk)
          }
        } else if (toggle_stochastic_SO == "on"){
          vax_effect_ILT = RECORD_antiviral_setup$incidence_log_tidy %>%
            filter(
              vax_scenario            == 'all willing adults vaccinated with a primary schedule',
              vax_scenario_risk_group == toggle_vax_scenario_risk_group
            )
          vax_effect_IL = RECORD_antiviral_setup$incidence_log %>%
            filter(
              vax_scenario            == 'all willing adults vaccinated with a primary schedule',
              vax_scenario_risk_group == toggle_vax_scenario_risk_group
            )
          vax_effect_EL= RECORD_antiviral_setup$exposed_log %>%
            filter(
              vax_scenario            == 'all willing adults vaccinated with a primary schedule',
              vax_scenario_risk_group == toggle_vax_scenario_risk_group
            )
          vax_effect_VHF = RECORD_antiviral_setup$vaccination_history_FINAL %>%
            filter(
              vax_scenario            == 'all willing adults vaccinated with a primary schedule',
              vax_scenario_risk_group == toggle_vax_scenario_risk_group
            )
          
          LOADED_severe_outcome_country_level = load_SO$SAVE_severe_outcome_country_level
          LOADED_VE_waning_distribution       = load_SO$SAVE_VE_waning_distribution
          LOADED_rho_SO_est                   = load_SO$SAVE_rho_SO_est
          rm(load_SO)
          
          vax_effect_load_stochastic_SO = stochastic_severe_outcomes(
            incidence_log = vax_effect_IL,
            incidence_log_tidy = vax_effect_ILT,
            vaccination_history_FINAL = vax_effect_VHF,
            exposed_log = vax_effect_EL,
            
            setting = 'SLE',
            num_time_steps = 365,
            strain_now = 'omicron',
            risk_group_name = toggle_vax_scenario_risk_group,
            date_start = toggle_antiviral_start_date,
            prop_sympt_LOCAL = prop_sympt,
            
            toggle_intervention = 'booster',
            save_point = "on",
            SAVE_severe_outcome_country_level = LOADED_severe_outcome_country_level,
            SAVE_VE_waning_distribution = LOADED_VE_waning_distribution,
            SAVE_rho_SO_est = LOADED_rho_SO_est
          )
          vax_effect_OWA = vax_effect_load_stochastic_SO$outcomes_without_antivirals
          
          vax_effect_OWA = vax_effect_OWA %>% 
            rename(no_booster_overall = overall,
                   no_booster_high_risk = high_risk)
          row = data.frame(outcome = 'booster_doses_delivered',no_booster_overall = 0, no_booster_high_risk = 0)
          vax_effect_OWA = rbind(vax_effect_OWA,row)
          
          rm(vax_effect_ILT,vax_effect_IL,vax_effect_EL,vax_effect_VHF,vax_effect_load_stochastic_SO)
        }
        
        #Step Two: compare OWA between booster and no-booster scenario

          vax_effect_comparison = outcomes_without_antivirals %>% 
          select(-vax_scenario) %>%
          left_join(vax_effect_OWA, by = c('outcome')) %>%
          mutate(overall = no_booster_overall - overall,
                 high_risk = no_booster_high_risk - high_risk) %>%
          select(-no_booster_high_risk, -no_booster_overall) %>%
          pivot_longer(
            cols = c('overall','high_risk') ,
            names_to = 'evaluation_group',
            values_to = 'n'
          ) 
        
        vax_effect_tracker = rbind(vax_effect_tracker,vax_effect_comparison)
      }
      #____________________________________________________________________________
      
      
    } #END RUN NUMBER AND STOCHASTIC SAMPLING
    rm(
      antiviral_target_individuals_run,
      workshop,
      prevented_by_antivirals,
      likelihood_severe_outcome,
      antiviral_target_individuals
    )
    #____________________________________________________________________________
    
    
    ### CREATE SUMMARY OVER RUNS #################################################
    this_scenario_tracker = this_scenario_tracker %>% mutate(intervention = 'antiviral', evaluation_group = 'overall')
    intervention_doses_delivered = data.frame(intervention = 'antiviral', evaluation_group = 'overall', doses = length_antiviral_delivery_tracker )
    
    if (toggle_compare_to_vaccine_effect == "on" & toggle_vax_scenario != 'all willing adults vaccinated with a primary schedule'){
      vax_effect_tracker = vax_effect_tracker %>% mutate(intervention = 'vaccine')
      
      workshop = vax_effect_tracker %>% filter(outcome == 'booster_doses_delivered') %>% group_by(intervention,evaluation_group) %>% summarise(doses = abs(mean(n)))
      intervention_doses_delivered = rbind(intervention_doses_delivered,workshop)
      
      vax_effect_tracker = vax_effect_tracker %>% filter(outcome != 'booster_doses_delivered')
      this_scenario_tracker = bind_rows(this_scenario_tracker,vax_effect_tracker)
      
      #NOTE: if comparing vax and antiviral, then baseline is no booster and no antiviral; otherwise, baseline is only no antiviral
      outcomes_without_antivirals = RECORD_antiviral_setup$outcomes_without_antivirals %>%
        filter(
          vax_scenario            == 'all willing adults vaccinated with a primary schedule',
          vax_scenario_risk_group == toggle_vax_scenario_risk_group
        ) %>%
        pivot_longer(
          cols = c('overall','high_risk') ,
          names_to = 'evaluation_group',
          values_to = 'overall'
        ) %>%
        select(outcome,evaluation_group,overall)

    } else{
      outcomes_without_antivirals = outcomes_without_antivirals %>% mutate(evaluation_group = 'overall')
    }
    
    summary_over_runs <-
      this_scenario_tracker %>%
      group_by(outcome,intervention, evaluation_group) %>%
      dplyr::summarise(
        average = mean(n),
        sd = sd(n),
        UCI = average - qnorm(0.975) * sd,
        LCI = average - qnorm(0.025) * sd, .groups = 'keep'
      ) %>%
      left_join(outcomes_without_antivirals, by = c("outcome",'evaluation_group')) %>%
      mutate(
        percentage = average / overall * 100,
        UCI_percentage = UCI / overall * 100,
        LCI_percentage = LCI / overall * 100
      ) %>%
      select(outcome, intervention,evaluation_group, average, UCI, LCI,
             percentage, UCI_percentage, LCI_percentage)
    
    summary_over_runs_tidy = summary_over_runs %>%
      pivot_longer(
        cols = 4:ncol(summary_over_runs) ,
        names_to = 'result',
        values_to = 'value'
      ) 
    
    workshop = summary_over_runs_tidy %>%
      filter(result %in% c('average','UCI','LCI')) %>%
      left_join(intervention_doses_delivered, by = c('intervention','evaluation_group')) %>%
      mutate(result = paste(result,'_doses_per_outcome_averted',sep=''),
             value = doses/value) %>%
      select(-doses)
    
    summary_over_runs_tidy = rbind(summary_over_runs_tidy,workshop) %>%
      mutate(evaluation_group = case_when(evaluation_group == 'overall' ~ 'pop_level', TRUE ~ evaluation_group))
    

        # mutate(doses_per_outcome_averted = outcomes_without_antivirals$overall[outcomes_without_antivirals$outcome == 'booster_doses_delivered']/overall,
        #        doses_per_outcome_averted_risk = outcomes_without_antivirals$high_risk[outcomes_without_antivirals$outcome == 'booster_doses_delivered']/high_risk)
        # 

    
    rm(this_scenario_tracker,
       summary_over_runs,
       outcomes_without_antivirals)
    
    #monitor if daily capacity being used or not enough seeking/accessing care
    if (pathway_to_care == 'realistic'){
      antiviral_rollout_capacity_utilised = round(
        100 * length_antiviral_delivery_tracker / (
          toggle_antiviral_delivery_capacity * antiviral_delivery_length
        ),
        digits = 1
      )
      antiviral_eligible_pop_coverage = round(100 *
                                                length_antiviral_delivery_tracker / total_target, digits = 1)
      row1 = c(outcome = 'program_measure',
               result = 'antiviral_rollout_capacity_utilised',
               value = antiviral_rollout_capacity_utilised)
      row2 = c(outcome = 'program_measure',
               result = 'antiviral_eligible_pop_coverage',
               value = antiviral_eligible_pop_coverage)
      
      summary_over_runs_tidy = rbind(summary_over_runs_tidy, row1, row2)
    } else if (pathway_to_care == 'fixed'){
      summary_over_runs_tidy = summary_over_runs_tidy %>% 
        mutate(antiviral_cov = toggle_fixed_antiviral_coverage,
               antiviral_delivered = length_antiviral_delivery_tracker,
               antiviral_start_date = toggle_antiviral_start_date)
    }
    #____________________________________________________________________________
    
    

    
    
    #time.end.AntiviralModel=proc.time()[[3]]
    #time.end.AntiviralModel-time.start.AntiviralModel
    #1 run takes 7.36 seconds (16/09)
    #10 runs takes 65.54 seconds(16/09)
    #100 runs take 667.32 seconds (16/09)
    #estimating 1000 runs will take ~2 hours (although would CI converge too much?)
    
    
    result = summary_over_runs_tidy %>%
      mutate(
        antiviral_type = toggle_antiviral_type,
        antiviral_target = toggle_antiviral_target,
        vax_scenario = toggle_vax_scenario,
        vax_scenario_risk_group = toggle_vax_scenario_risk_group
      )
    
    if (length(toggle_sensitivity_analysis)>0){
      result = result %>% mutate(toggle_sensitivity_analysis = paste(names(toggle_sensitivity_analysis),toggle_sensitivity_analysis))
    }
    

    
    return(result)
  }
}
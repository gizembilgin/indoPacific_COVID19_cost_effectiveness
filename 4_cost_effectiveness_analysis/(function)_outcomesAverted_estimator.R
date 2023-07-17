### This function calculates the QALYs averted by setting, booster_vax_scenario, intervention, and intervention_target_group
### Italso returns additional outcomes such as death and hosp
#NB: There is no uncertainty in QALY conversion estimates as data sources are expert point estimates of:
#       population (UN), life expectancy (UN), HRQoL (Robinson, Eber & Hammitt), and age_severity_specific_QALYs (Robinson, Eber & Hammitt)

outcomesAverted_estimator <- function(
    LIST_CEA_settings,
    MASTER_antiviral_simulations,
    ARRAY_additional_outcomes = c("death","hosp"),
    toggle_longCOVID = "off",
    toggle_discounting_rate = 0.03, #NB: limitation can only change discounting of YLL of fatal cases, not YLD of critical cases due to restrictions of underlying data
    this_risk_group = "adults_with_comorbidities"
    ){

  ### PART ONE: loading QALY estimates############################################
  load(file = "2_inputs/QALY_estimates.Rdata")
  QALY_estimates = QALY_estimates %>%
    filter(setting %in% LIST_CEA_settings &
             (discounting_rate == toggle_discounting_rate | is.na(discounting_rate))) %>%
    select(-discounting_rate)
  if (toggle_longCOVID == "off"){
    QALY_estimates = QALY_estimates %>% 
      filter(outcome != "total_cases")
  }
  ##############################################################################
  
  
  
  ### PART TWO: load antiviral simulation ######################################
  TRANSLATED_antiviral_simulations = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == FALSE) %>% #we want age-specific incidence
    rename(count_outcomes = value) %>%
    filter(! (outcome %in% c("YLL","neonatal_deaths","booster_doses_delivered","ICU"))) %>%
    select(evaluation_level,setting, outcome, booster_vax_scenario, intervention, intervention_target_group, age_group,count_outcomes)
  
  if (nrow(TRANSLATED_antiviral_simulations[!(TRANSLATED_antiviral_simulations$intervention == "no intervention" ),]) #intervention_target_group is understandably NA 
      != nrow(na.omit(TRANSLATED_antiviral_simulations))){stop("NA introduced")}
  
  #add back mild for scenarios with antivirals but no booster dose
  #(antivirals don't affect mild presentations of disease, but mild presentations still add to the net QALYs!)
  add_mild_to_net = TRANSLATED_antiviral_simulations %>%
    filter(outcome == "mild" & evaluation_level == "net" & booster_vax_scenario == "no booster dose")
  structure = TRANSLATED_antiviral_simulations %>%
    select(intervention,intervention_target_group) %>%
    filter(!(intervention %in% add_mild_to_net$intervention) &
             intervention != "booster dose 2023-03-01")
  structure = unique(structure)
  add_mild_to_net = add_mild_to_net %>%
    select(-intervention,-intervention_target_group)
  add_mild_to_net = crossing(add_mild_to_net,structure)
  TRANSLATED_antiviral_simulations = rbind(TRANSLATED_antiviral_simulations,add_mild_to_net)
  ##############################################################################
  
  
  
  ### PART THREE: calculating QALYs ############################################
  QALY_breakdown = TRANSLATED_antiviral_simulations %>%
    left_join(QALY_estimates, by = c("setting","outcome","age_group")) %>%
    filter(is.na(QALYs) == FALSE) %>%
    mutate(count_outcomes = count_outcomes * QALYs) %>%
    select(-QALYs) %>%
    mutate(outcome_source = outcome,
           outcome = "QALYs") %>%
    group_by(evaluation_level,setting,outcome,outcome_source,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(count_outcomes = sum(count_outcomes),
              .groups = "keep")
  
  outcomes_averted = QALY_breakdown %>%
    #collapsing outcome and age_group to calculate QALYs
    group_by(evaluation_level,setting,outcome,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(count_outcomes = sum(count_outcomes),
              .groups = "keep") 
  
  ###Plot to breakdown where QALYs from
  # ggplot(QALY_breakdown[QALY_breakdown$evaluation_level == "incremental",]) + geom_col(aes(x=outcome_source,y=count_outcomes)) +
  #   facet_grid(booster_vax_scenario ~.)
  # ggplot(QALY_breakdown[QALY_breakdown$evaluation_level == "net" & 
  #                         QALY_breakdown$intervention =="nirmatrelvir_ritonavir 2023-01-01" & 
  #                         QALY_breakdown$intervention_target_group ==  "adults_with_comorbidities" ,]) + 
  #   geom_col(aes(x=outcome_source,y=count_outcomes)) +
  #   facet_grid(booster_vax_scenario ~.)
  #COME BACK - plot QALY breakdown in SM incremental vs net for complete scenarios (i.e., booster scenario + antiviral scenario combinations)
  ##############################################################################
  
  #BONUS: add hosp and death outcomes while here
  if (length(ARRAY_additional_outcomes)>0){
    additional_outcomes = TRANSLATED_antiviral_simulations %>%
      filter(outcome %in% ARRAY_additional_outcomes) %>%
      #collapsing outcome and age_group 
      group_by(evaluation_level, setting,outcome,booster_vax_scenario,intervention,intervention_target_group) %>%
      summarise(count_outcomes = sum(count_outcomes),
                .groups = "keep")
    
    outcomes_averted = rbind(outcomes_averted,additional_outcomes)
  }
  rm(TRANSLATED_antiviral_simulations,additional_outcomes)
  ##############################################################################

  result = list(outcomes_averted = outcomes_averted,
                QALY_breakdown = QALY_breakdown)  
  return(result)
}
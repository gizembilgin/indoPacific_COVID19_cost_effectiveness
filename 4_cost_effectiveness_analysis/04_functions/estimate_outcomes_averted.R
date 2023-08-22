### This function calculates the QALYs averted by setting, booster_vax_scenario, intervention, and intervention_target_group
### Italso returns additional outcomes such as death and hosp
#NB: There is no uncertainty in QALY conversion estimates as data sources are expert point estimates of:
#       population (UN), life expectancy (UN), HRQoL (Robinson, Eber & Hammitt), and age_severity_specific_QALYs (Robinson, Eber & Hammitt)

estimate_outcomes_averted <- function(
    LIST_CEA_settings,
    MASTER_antiviral_simulations,
    ARRAY_additional_outcomes = c("death","hosp"),
    toggle_longCOVID = "off",
    list_discounting_rate = 0.03, #NB: limitation can only change discounting of YLL of fatal cases, not YLD of critical cases due to restrictions of underlying data
    this_risk_group = "adults_with_comorbidities"
    ){

  ### PART ONE: loading QALY estimates############################################
  load(file = "02_inputs/QALY_estimates.Rdata")
  QALY_estimates = QALY_estimates %>%
    filter(setting %in% LIST_CEA_settings &
             (discounting_rate %in% list_discounting_rate | is.na(discounting_rate)))
  if (toggle_longCOVID == "off"){
    QALY_estimates = QALY_estimates %>% 
      filter(outcome != "total_cases")
  }
  expand = QALY_estimates %>%
    filter(is.na(discounting_rate)) %>%
    select(-discounting_rate)
  expand = crossing(expand,
                    discounting_rate = list_discounting_rate)
  QALY_estimates = rbind(QALY_estimates[is.na(QALY_estimates$discounting_rate) == FALSE,],
                         expand)
  ##############################################################################
  
  
  
  ### PART TWO: load antiviral simulation ######################################
  TRANSLATED_antiviral_simulations = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == FALSE) %>% #we want age-specific incidence
    rename(count_outcomes = value) %>%
    filter(! (outcome %in% c("YLL","neonatal_deaths","booster_doses_delivered","ICU"))) %>%
    select(evaluation_level,setting, outcome, booster_vax_scenario, intervention, intervention_target_group, age_group,count_outcomes)
  
  nrow_with_intervention <- nrow(TRANSLATED_antiviral_simulations[!(TRANSLATED_antiviral_simulations$intervention == "no intervention" ),])  #intervention_target_group is understandably NA 
  nrow_omitting_NA       <- nrow(na.omit(TRANSLATED_antiviral_simulations))
  if (nrow_with_intervention != nrow_omitting_NA) stop("NA introduced in TRANSLATED_antiviral_simulations during estimate_outcomes_averted")
  ##############################################################################
  
  
  
  ### PART THREE: calculating QALYs ############################################
  QALY_breakdown = TRANSLATED_antiviral_simulations %>%
    left_join(QALY_estimates, by = c("setting","outcome","age_group"),
              relationship = "many-to-many") %>% #if multiple discounting_rate options
    filter(is.na(QALYs) == FALSE) %>%
    mutate(count_outcomes = count_outcomes * QALYs) %>%
    select(-QALYs) %>%
    mutate(outcome_source = outcome,
           outcome = "QALYs") %>%
    group_by(evaluation_level,discounting_rate,setting,outcome,outcome_source,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(count_outcomes = sum(count_outcomes),
              .groups = "keep")
  
  outcomes_averted = QALY_breakdown %>%
    #collapsing outcome and age_group to calculate QALYs
    group_by(evaluation_level,discounting_rate,setting,outcome,booster_vax_scenario,intervention,intervention_target_group) %>%
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
      group_by(evaluation_level,setting,outcome,booster_vax_scenario,intervention,intervention_target_group) %>%
      summarise(count_outcomes = sum(count_outcomes),
                .groups = "keep")
    #COMEBACK - could fiddle with expanding later if this makes the data too large
    additional_outcomes = crossing(additional_outcomes,
                                   discounting_rate = list_discounting_rate)
    
    outcomes_averted = rbind(outcomes_averted,additional_outcomes)
  }
  rm(TRANSLATED_antiviral_simulations,additional_outcomes)
  ##############################################################################

  result = list(outcomes_averted = outcomes_averted,
                QALY_breakdown = QALY_breakdown)  
  return(result)
}
require(readr); require(ggplot2); require(tidyverse)

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

  setting_list = LIST_CEA_settings
  age_groups_num = c(0,4,9,17,29,44,59,69,110)
  age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100')

  
  
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
  ## Subset #####################################################################
  # We would like a data set with the following columns:
  # setting, outcome, booster_vax_scenario, intervention, intervention target group, 
  # intervention_doses_delivered, count_outcomes_averted
  
  TRANSLATED_antiviral_simulations = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == FALSE) %>%
    select(-country,-setting_beta) %>%
    
    #created shorten name to describe booster dose eligibility
    mutate(booster_vax_scenario = case_when( 
      vax_scenario == "catchup campaign for high-risk adults: assume booster to high-risk adults who have previously completed their primary schedule but have not recieved a booster"  ~ "booster dose catch-up campaign for high-risk adults",           
      vax_scenario == "catchup campaign for all adults: assume booster to all adults who have previously completed their primary schedule but have not recieved a booster" ~ "booster dose catch-up campaign for all adults",                       
      vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults" ~ "booster to all adults, prioritised to high-risk adults",                                                           
      vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~ "booster to all adults previously willing to be vaccinated",                      
      vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~ "booster to all high-risk adults previously willing to be vaccinated",  
      vax_scenario == "all willing adults vaccinated with a primary schedule" ~ "no booster dose"           
    )) %>%
    filter(is.na(booster_vax_scenario) == FALSE) %>%
    
    #DECISION - CEA of antivirals for a whole year (as of 01/01/2023)
    filter(intervention %in% c('vaccine','antiviral 2023-01-01')) %>%
    mutate(intervention = case_when(
      intervention == "vaccine" ~ "booster dose 2023-03-01",
      antiviral_type == "molunipiravir" ~ "molunipiravir 2023-01-01",
      antiviral_type == "nirmatrelvir_ritonavir" ~ "nirmatrelvir_ritonavir 2023-01-01"
    )) %>%
    
    mutate(intervention_target_group = 
             case_when(
               intervention %in% c("molunipiravir 2023-01-01","nirmatrelvir_ritonavir 2023-01-01") ~ antiviral_target_group,
               booster_vax_scenario %in% c("booster dose catch-up campaign for high-risk adults","booster to all high-risk adults previously willing to be vaccinated") ~ vax_scenario_risk_group,
               booster_vax_scenario %in% c("booster dose catch-up campaign for all adults","booster to all adults, prioritised to high-risk adults", "booster to all adults previously willing to be vaccinated") ~ "all_adults"
             )) %>%
    
    filter(result %in% c("n")) %>%
    
    #mutate(doses_per_outcome_averted = intervention_doses_delivered/value) %>%
    rename(count_outcomes_averted = value) %>%
    
    filter(! (outcome %in% c("YLL","neonatal_deaths","booster_doses_delivered","ICU"))) %>%
    
    select(setting, outcome, booster_vax_scenario, intervention, intervention_target_group, age_group,count_outcomes_averted)
  
  if (nrow(TRANSLATED_antiviral_simulations) != nrow(na.omit(TRANSLATED_antiviral_simulations))){stop("NA introduced")}
  ##############################################################################
  
  
  
  
  
  ### PART THREE: calculating QALYs ############################################
  outcomes_averted = TRANSLATED_antiviral_simulations %>%
    left_join(QALY_estimates, by = c("setting","outcome","age_group")) %>%
    filter(is.na(QALYs) == FALSE) %>%
    mutate(count_outcomes_averted = count_outcomes_averted * QALYs) %>%
    select(-QALYs) %>%
    mutate(outcome = "QALYs")%>%
    #collapsing outcome and age_group to calculate QALYs
    group_by(setting,outcome,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(count_outcomes_averted = sum(count_outcomes_averted),
              .groups = "keep") 
  
  QALY_breakdown = TRANSLATED_antiviral_simulations %>%
    left_join(QALY_estimates, by = c("setting","outcome","age_group")) %>%
    filter(is.na(QALYs) == FALSE) %>%
    mutate(count_outcomes_averted = count_outcomes_averted * QALYs) %>%
    select(-QALYs) %>%
    mutate(outcome_source = outcome,
           outcome = "QALYs") %>%
    group_by(setting,outcome_source,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(count_outcomes_averted = sum(count_outcomes_averted),
              .groups = "keep") 
  
  ###Plot to breakdown where QALYs from
  # ggplot(QALY_breakdown) + geom_col(aes(x=outcome_source,y=count_outcomes_averted)) +
  #   facet_grid(booster_vax_scenario ~.)
  ##############################################################################
  
  #BONUS: add hosp and death outcomes while here
  if (length(ARRAY_additional_outcomes)>0){
    additional_outcomes = TRANSLATED_antiviral_simulations %>%
      filter(outcome %in% ARRAY_additional_outcomes) %>%
      #collapsing outcome and age_group 
      group_by(setting,outcome,booster_vax_scenario,intervention,intervention_target_group) %>%
      summarise(count_outcomes_averted = sum(count_outcomes_averted),
                .groups = "keep")
    
    outcomes_averted = rbind(outcomes_averted,additional_outcomes)
    rm(additional_outcomes)
    
    #CHECK
    # check = outcomes_averted %>%
    #   pivot_wider(values_from = "count_outcomes_averted",names_from = "outcome")
  }
  rm(TRANSLATED_antiviral_simulations)
  ##############################################################################

  result = list(outcomes_averted = outcomes_averted,
                QALY_breakdown = QALY_breakdown)  
  return(result)
}

#test
# outcomes_averted <- outcomes_averted_estimator(LIST_CEA_settings,toggle_discounting_rate = TOGGLE_discounting_rate)
# outcomes_averted %>% arrange(setting,booster_vax_scenario,desc(count_outcomes_averted))

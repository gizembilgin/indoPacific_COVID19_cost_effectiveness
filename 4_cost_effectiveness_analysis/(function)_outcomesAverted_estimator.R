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
  # We would like a data set with the following columns:
  # setting, outcome, booster_vax_scenario, intervention, intervention target group, 
  # intervention_doses_delivered, count_outcomes
  
  TRANSLATED_antiviral_simulations = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == FALSE) %>%
    
    #mutate(doses_per_outcome_averted = intervention_doses_delivered/value) %>%
    rename(count_outcomes = value) %>%
    
    filter(! (outcome %in% c("YLL","neonatal_deaths","booster_doses_delivered","ICU"))) %>%
    
    select(evaluation_level,setting, outcome, booster_vax_scenario, intervention, intervention_target_group, age_group,count_outcomes)
  
  if (nrow(TRANSLATED_antiviral_simulations[!(TRANSLATED_antiviral_simulations$intervention == "no intervention" ),]) #intervention_target_group is understandably NA 
      != nrow(na.omit(TRANSLATED_antiviral_simulations))){stop("NA introduced")}
  ##############################################################################
  
  
  
  ### PART THREE: recreate booster-antiviral scenarios ##########################
  #NB: the incremental effect of booster doses and antiviral scenarios were kept separate in (antiviral) run
  #    Let's combine back all combination
  Combined_0 = TRANSLATED_antiviral_simulations %>%
    filter(evaluation_level == "incremental")
  
  Combined = data.frame()
  ###vax no antiviral
  this_row = Combined_0 %>% 
    filter(intervention == "booster dose 2023-03-01") %>%
    mutate(antiviral_scenario = "no antiviral",
           antiviral_target_group = NA)  %>%
    select(evaluation_level,setting,outcome, booster_vax_scenario,antiviral_scenario,antiviral_target_group, age_group, count_outcomes)
  Combined = rbind(Combined,this_row)
  
  ###vax with antiviral
  for (this_antiviral in unique(Combined_0$intervention[Combined_0$intervention != "booster dose 2023-03-01"])){ 
    # cycle through the types of antivirals
    for (this_antiviral_target in unique(Combined_0$intervention_target_group)){
      # cycle through antiviral target groups
      this_row = Combined_0 %>% 
        filter(intervention %in% c("booster dose 2023-03-01",this_antiviral)) %>%
        filter(intervention_target_group == this_antiviral_target | intervention == "booster dose 2023-03-01") %>%
        group_by(evaluation_level,setting,outcome,booster_vax_scenario,age_group) %>%
        summarise(count_outcomes = sum(count_outcomes),
                  .groups = "keep") %>%
        mutate(antiviral_scenario = this_antiviral,
               antiviral_target_group = this_antiviral_target) %>%
        select(evaluation_level,setting,outcome, booster_vax_scenario,antiviral_scenario,antiviral_target_group, age_group, count_outcomes)
      Combined = rbind(Combined,this_row)
    }
  }
  
  TRANSLATED_antiviral_simulations = TRANSLATED_antiviral_simulations %>%
    filter(evaluation_level != "incremental") %>%
    rename(antiviral_scenario = intervention,
           antiviral_target_group = intervention_target_group) %>%
    mutate(
      antiviral_target_group = case_when(
        antiviral_scenario %in% c("booster dose 2023-03-01","no intervention") ~ NA,
        TRUE ~ antiviral_target_group
      ),
      antiviral_scenario = case_when(
        antiviral_scenario %in% c("booster dose 2023-03-01","no intervention") ~ "no antiviral",
        TRUE ~ antiviral_scenario
      ))
  
  #add back mild for scenarios with antivirals (antivirals don't affect mild presentations of disease, but mild presentations still add to the net QALYs!)
  add_mild_to_net = TRANSLATED_antiviral_simulations %>%
    filter(outcome == "mild" & evaluation_level == "net" & booster_vax_scenario == "no booster dose")
  structure = TRANSLATED_antiviral_simulations %>%
    select(antiviral_scenario,antiviral_target_group) %>%
    filter(!(antiviral_scenario %in% add_mild_to_net$antiviral_scenario))
  structure = unique(structure)
  add_mild_to_net = add_mild_to_net %>%
    select(-antiviral_scenario,-antiviral_target_group)
  add_mild_to_net = crossing(add_mild_to_net,structure)
  TRANSLATED_antiviral_simulations = rbind(TRANSLATED_antiviral_simulations,add_mild_to_net)
  
  TRANSLATED_antiviral_simulations = rbind(Combined,TRANSLATED_antiviral_simulations); rm(Combined_0,Combined,this_row)
  ##############################################################################
  
  
  
  
  
  ### PART FOUR: calculating QALYs ############################################
  outcomes_averted = TRANSLATED_antiviral_simulations %>%
    left_join(QALY_estimates, by = c("setting","outcome","age_group")) %>%
    filter(is.na(QALYs) == FALSE) %>%
    mutate(count_outcomes = count_outcomes * QALYs) %>%
    select(-QALYs) %>%
    mutate(outcome = "QALYs")%>%
    #collapsing outcome and age_group to calculate QALYs
    group_by(evaluation_level,setting,outcome,booster_vax_scenario,antiviral_scenario,antiviral_target_group) %>%
    summarise(count_outcomes = sum(count_outcomes),
              .groups = "keep") 
  
  QALY_breakdown = TRANSLATED_antiviral_simulations %>%
    left_join(QALY_estimates, by = c("setting","outcome","age_group")) %>%
    filter(is.na(QALYs) == FALSE) %>%
    mutate(count_outcomes = count_outcomes * QALYs) %>%
    select(-QALYs) %>%
    mutate(outcome_source = outcome,
           outcome = "QALYs") %>%
    group_by(evaluation_level,setting,outcome_source,booster_vax_scenario,antiviral_scenario,antiviral_target_group) %>%
    summarise(count_outcomes = sum(count_outcomes),
              .groups = "keep") 
  
  ###Plot to breakdown where QALYs from
  # ggplot(QALY_breakdown[QALY_breakdown$evaluation_level == "incremental",]) + geom_col(aes(x=outcome_source,y=count_outcomes)) +
  #   facet_grid(booster_vax_scenario ~.)
  # ggplot(QALY_breakdown[QALY_breakdown$evaluation_level == "net" & 
  #                         QALY_breakdown$antiviral_scenario =="nirmatrelvir_ritonavir 2023-01-01" & 
  #                         QALY_breakdown$antiviral_target_group ==  "adults_with_comorbidities" ,]) + 
  #   geom_col(aes(x=outcome_source,y=count_outcomes)) +
  #   facet_grid(booster_vax_scenario ~.)
  #COME BACK - plot QALY breakdown in SM incremental vs net for complete scenarios (i.e., booster scenario + antiviral scenario combinations)
  ##############################################################################
  
  #BONUS: add hosp and death outcomes while here
  if (length(ARRAY_additional_outcomes)>0){
    additional_outcomes = TRANSLATED_antiviral_simulations %>%
      filter(outcome %in% ARRAY_additional_outcomes) %>%
      #collapsing outcome and age_group 
      group_by(evaluation_level, setting,outcome,booster_vax_scenario,antiviral_scenario,antiviral_target_group) %>%
      summarise(count_outcomes = sum(count_outcomes),
                .groups = "keep")
    
    outcomes_averted = rbind(outcomes_averted,additional_outcomes)
    rm(additional_outcomes)
    
    #CHECK
    # check = outcomes_averted %>%
    #   pivot_wider(values_from = "count_outcomes",names_from = "outcome")
  }
  rm(TRANSLATED_antiviral_simulations)
  ##############################################################################

  result = list(outcomes_averted = outcomes_averted,
                QALY_breakdown = QALY_breakdown)  
  return(result)
}

#test
# outcomes_averted <- outcomes_averted_estimator(LIST_CEA_settings,toggle_discounting_rate = TOGGLE_discounting_rate)
# outcomes_averted %>% arrange(setting,booster_vax_scenario,desc(count_outcomes))

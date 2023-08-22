CEA_worker <- function(
    numberOfRunsPerCluster,
    CEA_risk_group,
    LIST_CEA_settings,
    LIST_perspectives,
    LIST_booster_vax_scenarios,
    LIST_antiviral_elig_groups,
    LIST_antiviral_types,
    DECISION_sampling_strategy,
    DECISION_include_net,
    TOGGLE_uncertainty,
    TOGGLE_longCOVID,
    LIST_discounting_rate,
    LIST_antiviral_cost_scenario,
    TORNADO_PLOT_OVERRIDE
){
  
  ### LOAD
  source(paste0(getwd(),"/04_functions/sample_transmission_model_runs.R"),local=TRUE)
  source(paste0(getwd(),"/04_functions/estimate_outcomes_averted.R"),local=TRUE)
  source(paste0(getwd(),"/04_functions/estimate_intervention_costs.R"),local=TRUE)
  source(paste0(getwd(),"/04_functions/estimate_healthcare_costs_averted.R"),local=TRUE)
  source(paste0(getwd(),"/04_functions/estimate_productivity_losses_averted.R"),local=TRUE)
  source(paste0(getwd(),"/04_functions/summarise_this_CEA_run.R"),local=TRUE)
  load(file = "02_inputs/fitted_distributions.Rdata")
  local_fitted_distributions = fitted_distributions; rm(fitted_distributions)
  
  this_risk_group = CEA_risk_group
  
  CommandDeck_result_long = data.frame()
  
  for (ticket in 1:numberOfRunsPerCluster){
    
    ###(1/3) Load antiviral model runs
    MASTER_antiviral_simulations <- sample_transmission_model_runs (LIST_CEA_settings,
                                                                  LIST_booster_vax_scenarios,
                                                                  LIST_antiviral_elig_groups,
                                                                  LIST_antiviral_types,
                                                                  sampling_strategy = DECISION_sampling_strategy,
                                                                  toggle_uncertainty = TOGGLE_uncertainty,
                                                                  decision_include_net = DECISION_include_net)
    LIST_CEA_settings_mod = gsub("PNG_low_beta","PNG",LIST_CEA_settings)
    
    ###(2/3) Calculate QALYs, intervention costs, and healthcare costs averted
    outcomesAvertedEstimation <- estimate_outcomes_averted(LIST_CEA_settings_mod,
                                                           MASTER_antiviral_simulations,
                                                           toggle_longCOVID = TOGGLE_longCOVID,
                                                           list_discounting_rate = as.numeric(LIST_discounting_rate))
    # 0.63 seconds
    #list including QALY_breakdown by evaluation_level,setting,outcome_source,booster_vax_scenario,antiviral_type,antiviral_target_group,count_outcomes; and 
    #               outcomes_averted by evaluation_level,setting,outcome {QALYs,deaths,hospitalisations},booster_vax_scenario,antiviral_type,antiviral_target_group,count_outcomes
    
    interventionCost_estimates <- estimate_intervention_costs(LIST_CEA_settings_mod,
                                                             MASTER_antiviral_simulations,
                                                             TORNADO_PLOT_OVERRIDE,
                                                             LIST_antiviral_cost_scenario,
                                                             toggle_uncertainty = TOGGLE_uncertainty)
    # 217.43  seconds for all combinations, 3.06 for one booster + one antiviral
    
    healthcareCostEstimation <- estimate_healthcare_costs_averted(LIST_CEA_settings_mod,
                                                                 MASTER_antiviral_simulations,
                                                                 TORNADO_PLOT_OVERRIDE,
                                                                 toggle_uncertainty = TOGGLE_uncertainty)
    # 7.82 seconds for all combinations, 0.39 for one booster + one antiviral
    
    if ("societal" %in% LIST_perspectives){
      productivityCosts <- estimate_productivity_losses_averted (
        LIST_CEA_settings_mod,
        MASTER_antiviral_simulations,
        TORNADO_PLOT_OVERRIDE,
        list_discounting_rate = as.numeric(LIST_discounting_rate),
        this_risk_group = CEA_risk_group
      )
    } else{
      productivityCosts <- data.frame()
    }
    rm(MASTER_antiviral_simulations)
    
    ###(3/3) CEA per setting
    this_result <- summarise_this_CEA_run(DECISION_include_net,
                                     outcomesAvertedEstimation$outcomes_averted,
                                     interventionCost_estimates,
                                     healthcareCostEstimation$healthcareCosts_averted,
                                     productivityCosts$productivity_loss) %>%
      mutate(run_ID = random_id(n = 1, bytes = 8))
    rm(outcomesAvertedEstimation,interventionCost_estimates,healthcareCostEstimation,productivityCosts)
    
    #expand out to perspectives
    this_result = crossing(this_result,perspective = LIST_perspectives) %>%
      mutate(productivityLoss = case_when(
        perspective == "healthcare" ~ 0,
        perspective == "societal" ~ productivityLoss
      ))

    CommandDeck_result_long = rbind(CommandDeck_result_long,this_result)
    rm(this_result)
  }
  
  expand = CommandDeck_result_long %>% #5% of object size - COMEBACK can delay this step if memory becomes an issue
    filter(is.na(antiviral_cost_scenario)) %>%
    select(-antiviral_cost_scenario)
  expand = crossing(expand,
                    antiviral_cost_scenario = LIST_antiviral_cost_scenario)
  CommandDeck_result_long = bind_rows(CommandDeck_result_long[is.na(CommandDeck_result_long$antiviral_cost_scenario) == FALSE,],
                                      expand)
  rm(expand)
  
  return(CommandDeck_result_long)
}
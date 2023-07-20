CEA_worker <- function(
    numberOfRunsPerCluster,
    CEA_risk_group,
    LIST_CEA_settings,
    LIST_booster_vax_scenarios,
    LIST_antiviral_elig_groups,
    LIST_antiviral_types,
    DECISION_sampling_strategy,
    DECISION_include_net,
    TOGGLE_uncertainty,
    TOGGLE_longCOVID,
    TOGGLE_discounting_rate,
    TOGGLE_antiviral_cost_scenario,
    TORNADO_PLOT_OVERRIDE
){
  
  ### LOAD
  source(paste(getwd(),"/(function)_sample_compartmentalModel_run.R",sep=""),local=TRUE)
  source(paste(getwd(),"/(function)_outcomesAverted_estimator.R",sep=""),local=TRUE)
  source(paste(getwd(),"/(function)_interventionCost_estimator.R",sep=""),local=TRUE)
  source(paste(getwd(),"/(function)_healthCareCostsAverted_estimator.R",sep=""),local=TRUE)
  source(paste(getwd(),"/(function)_productivityCosts_estimator.R",sep=""),local=TRUE)
  source(paste(getwd(),"/(function)_simulationSummary.R",sep=""),local=TRUE)
  load(file = "2_inputs/fitted_distributions.Rdata")
  local_fitted_distributions = fitted_distributions; rm(fitted_distributions)
  
  this_risk_group = CEA_risk_group
  
  CommandDeck_result_long = data.frame()
  
  for (ticket in 1:numberOfRunsPerCluster){
    
    ###(1/3) Load antiviral model runs
    MASTER_antiviral_simulations <- sample_compartmentalModel_run(LIST_CEA_settings,
                                                                  LIST_booster_vax_scenarios,
                                                                  LIST_antiviral_elig_groups,
                                                                  LIST_antiviral_types,
                                                                  sampling_strategy = DECISION_sampling_strategy,
                                                                  toggle_uncertainty = TOGGLE_uncertainty,
                                                                  decision_include_net = DECISION_include_net)
    LIST_CEA_settings_mod = gsub("PNG_low_beta","PNG",LIST_CEA_settings)
    
    ###(2/3) Calculate QALYs, intervention costs, and healthcare costs averted
    outcomesAvertedEstimation <- outcomesAverted_estimator(LIST_CEA_settings_mod,
                                                           MASTER_antiviral_simulations,
                                                           toggle_longCOVID = TOGGLE_longCOVID,
                                                           toggle_discounting_rate = TOGGLE_discounting_rate)
    # 0.63 seconds
    #list including QALY_breakdown by evaluation_level,setting,outcome_source,booster_vax_scenario,antiviral_type,antiviral_target_group,count_outcomes; and 
    #               outcomes_averted by evaluation_level,setting,outcome {QALYs,deaths,hospitalisations},booster_vax_scenario,antiviral_type,antiviral_target_group,count_outcomes
    
    interventionCost_estimates <- interventionCost_estimator(LIST_CEA_settings_mod,
                                                             MASTER_antiviral_simulations,
                                                             TORNADO_PLOT_OVERRIDE,
                                                             antiviral_cost_scenario = TOGGLE_antiviral_cost_scenario,
                                                             toggle_uncertainty = TOGGLE_uncertainty)
    # 217.43  seconds for all combinations, 3.06 for one booster + one antiviral
    
    healthcareCostEstimation <- healthCareCostsAverted_estimator(LIST_CEA_settings_mod,
                                                                 MASTER_antiviral_simulations,
                                                                 TORNADO_PLOT_OVERRIDE,
                                                                 toggle_uncertainty = TOGGLE_uncertainty)
    # 7.82 seconds for all combinations, 0.39 for one booster + one antiviral
    
    if (TOGGLE_perspective == "societal"){
      productivityCosts <- productivityCosts_estimator (
        LIST_CEA_settings_mod,
        MASTER_antiviral_simulations,
        toggle_discounting_rate = TOGGLE_discounting_rate,
        this_risk_group = CEA_risk_group
      )
    } else{
      productivityCosts <- data.frame()
    }
    
    
    ###(3/3) CEA per setting
    this_result <- simulationSummary(DECISION_include_net,
                                     outcomesAvertedEstimation,
                                     interventionCost_estimates,
                                     healthcareCostEstimation,
                                     productivityCosts) %>%
      mutate(run_ID = random_id(n = 1, bytes = 8))
    CommandDeck_result_long = rbind(CommandDeck_result_long,this_result)
    
  }
  
  return(CommandDeck_result_long)
}
### This function queues all of the model runs you have requested

antiviral_model_manger <- function(
  
    LIST_antiviral_start_date,   # options: any date >= 01/01/2023 (due to '(antiviral) set up.R' configuration)
    LIST_vax_scenario,           # options: 
                                 # "all willing adults vaccinated with a primary schedule"
                                 # "all willing adults vaccinated with a primary schedule and high risk group receive a booster"
                                 # "all willing adults vaccinated with a primary schedule plus booster dose"
    LIST_antiviral_target_group, # options:'adults_with_comorbidities', 'pregnant_women','unvaccinated_adults','all_adults','unvaccinated_adults_AND_adults_with_comorbidities'
    LIST_antiviral_type = 'nirmatrelvir_ritonavir', # options:"nirmatrelvir_ritonavir","molunipiravir"     
    toggle_high_risk_group = "adults_with_comorbidities",  #options:'adults_with_comorbidities', 'pregnant_women'
    
    RECORD_antiviral_setup,                           # results file of '(antiviral) set up.R'
    setting = "SLE",                                  # setting from loop in (antiviral)(plot)
                            
    toggle_number_of_runs = 100,
    toggle_cluster_number = 4,                        # number of computer cores to engage
    toggle_stochastic_SO = "off",                     # run severe outcomes stochastically
    toggle_compare_to_vaccine_effect = "off",
    toggle_sensitivity_analysis = list(),
    pathway_to_care = "fixed",                        # options: "fixed","realistic"
    toggle_antiviral_delivery_capacity = NA,          # this toggle is used in combination with pathway_to_care == "realistic"
    toggle_fixed_antiviral_coverage = 0.2,            # this toggle is used in combination with pathway_to_care == "fixed"
    
    antiviral_model_worker = copy_function_into_cluster,
    manager_stochastic_VE_sampling = "uniform"
    
){
 
  ### Step One: create list of scenarios
  manager_scenario_dataframe = crossing(
                                vax_scenario = LIST_vax_scenario,
                                vax_scenario_risk_group = toggle_high_risk_group
                                )
  booster_start_date = RECORD_antiviral_setup$generic_booster_toggles$start_date

  #load functions to be copied into clusters
  load(file = '1_inputs/antiviral_effectiveness.Rdata' )
  source(paste(getwd(),"/(antiviral)(function) stochastic_severe_outcomes_sampling.R",sep=""))
  source(paste(getwd(),"/(antiviral)(function) stochastic_severe_outcomes_application.R",sep=""))
  copy_sampling_fx_into_cluster = stochastic_severe_outcomes_sampling
  copy_application_fx_into_cluster = stochastic_severe_outcomes_application
  #____________________________________
  
  
  
  ### Step Two: set parallel runs
  CLUSTER <- parallel::makeCluster(toggle_cluster_number) # create cluster
  doParallel::registerDoParallel(CLUSTER) # activate cluster
  
  #nesting foreach loops as per https://cran.r-project.org/web/packages/foreach/vignettes/nested.html
  system.time({
    RECORD_antiviral_model_simulations <- foreach::foreach(
      model_run_number = c(1:toggle_number_of_runs),
      .packages = c('tidyverse'),
      .combine = rbind,
      .inorder = FALSE
    )  %dopar% {
        
      
      antiviral_model_worker(
        manager_scenario_dataframe,
        RECORD_antiviral_setup,
        setting = setting,

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
        local_booster_start_date = booster_start_date,
        
        worker_stochastic_VE_sampling = manager_stochastic_VE_sampling,
        
        stochastic_severe_outcomes_sampling = copy_sampling_fx_into_cluster,
        stochastic_severe_outcomes_application = copy_application_fx_into_cluster
      )
      
    }
  })
  
  parallel::stopCluster(CLUSTER)
  #____________________________________
  
  
  
  ### Step Three: summary over runs
  # summary_over_runs <-
  #   RECORD_antiviral_model_simulations %>% 
  #   group_by(antiviral_type,vax_scenario, vax_scenario_risk_group, antiviral_target_group, outcome,intervention, evaluation_group) %>%
  #   dplyr::summarise(
  #     intervention_doses_delivered = mean(intervention_doses_delivered),
  #     
  #     average = mean(n),
  #     sd = sd(n),
  #     UCI = average - qnorm(0.975) * sd,
  #     LCI = average - qnorm(0.025) * sd, 
  #     percentage = mean(percentage),
  #     percentage_sd = sd(percentage),
  #     UCI_percentage = average - qnorm(0.975) * percentage_sd,
  #     LCI_percentage = average - qnorm(0.025) * percentage_sd, 
  #     
  #     .groups = 'keep'
  #   )  %>%
  #   select(-sd, - percentage_sd)
  # 
  # summary_over_runs_tidy = summary_over_runs %>%
  #   pivot_longer(
  #     cols = 9:ncol(summary_over_runs) ,
  #     names_to = 'result',
  #     values_to = 'value'
  #   ) 
  # 
  # workshop = summary_over_runs_tidy %>%
  #   filter(result %in% c('average','UCI','LCI')) %>%
  #   mutate(result = paste(result,'_doses_per_outcome_averted',sep=''),
  #          value = intervention_doses_delivered/value) 
  # 
  # summary_over_runs_tidy = rbind(summary_over_runs_tidy,workshop) %>%
  #   select(-intervention_doses_delivered) %>%
  #   mutate(evaluation_group = case_when(evaluation_group == 'overall' ~ 'pop_level', TRUE ~ evaluation_group))
  # 
  # 
  # #monitor if daily capacity being used or not enough seeking/accessing care
  # if (pathway_to_care == 'realistic') {
  #   antiviral_rollout_capacity_utilised = round(
  #     100 * length_antiviral_delivery_tracker / (
  #       local_antiviral_delivery_capacity * antiviral_delivery_length
  #     ),
  #     digits = 1
  #   )
  #   antiviral_eligible_pop_coverage = round(100 *
  #                                             length_antiviral_delivery_tracker / total_target,
  #                                           digits = 1)
  #   row1 = c(outcome = 'program_measure',
  #            result = 'antiviral_rollout_capacity_utilised',
  #            value = antiviral_rollout_capacity_utilised)
  #   row2 = c(outcome = 'program_measure',
  #            result = 'antiviral_eligible_pop_coverage',
  #            value = antiviral_eligible_pop_coverage)
  #   
  #   summary_over_runs_tidy = rbind(summary_over_runs_tidy, row1, row2)
  # } 
  # #____________________________________________________________________________
  # 
  # 
  # if (length(toggle_sensitivity_analysis)>0){
  #   summary_over_runs_tidy = summary_over_runs_tidy %>% mutate(sensitivity_analysis = paste(names(toggle_sensitivity_analysis),toggle_sensitivity_analysis))
  # }
  
  ### alternative
  RECORD_antiviral_model_simulations_tidy = RECORD_antiviral_model_simulations %>%
    pivot_longer(
      cols = "n":"percentage" ,
      names_to = 'result',
      values_to = 'value'
    ) 
  
  workshop = RECORD_antiviral_model_simulations_tidy %>%
    filter(result %in% c('n')) %>%
    mutate(result = 'doses_per_outcome_averted',
           value = intervention_doses_delivered/value) 
  
  RECORD_antiviral_model_simulations_tidy = rbind(RECORD_antiviral_model_simulations_tidy,workshop) %>%
    mutate(evaluation_group = case_when(evaluation_group == 'overall' ~ 'pop_level', TRUE ~ evaluation_group))
  
  
  #monitor if daily capacity being used or not enough seeking/accessing care
  if (pathway_to_care == 'realistic') {
    antiviral_rollout_capacity_utilised = round(
      100 * length_antiviral_delivery_tracker / (
        local_antiviral_delivery_capacity * antiviral_delivery_length
      ),
      digits = 1
    )
    antiviral_eligible_pop_coverage = round(100 *
                                              length_antiviral_delivery_tracker / total_target,
                                            digits = 1)
    row1 = c(outcome = 'program_measure',
             result = 'antiviral_rollout_capacity_utilised',
             value = antiviral_rollout_capacity_utilised)
    row2 = c(outcome = 'program_measure',
             result = 'antiviral_eligible_pop_coverage',
             value = antiviral_eligible_pop_coverage)
    
    RECORD_antiviral_model_simulations_tidy = rbind(RECORD_antiviral_model_simulations_tidy, row1, row2)
  } 
  #____________________________________________________________________________
  
  
  if (length(toggle_sensitivity_analysis)>0){
    RECORD_antiviral_model_simulations_tidy = RECORD_antiviral_model_simulations_tidy %>% mutate(sensitivity_analysis = paste(names(toggle_sensitivity_analysis),toggle_sensitivity_analysis))
  }
  
  return(RECORD_antiviral_model_simulations_tidy)
  #return(summary_over_runs_tidy) 
}
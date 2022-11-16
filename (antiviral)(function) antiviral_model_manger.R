
antiviral_model_manger <- function(
  
    LIST_antiviral_start_date, 
    LIST_vax_scenario,
    LIST_antiviral_target_group,
    toggle_high_risk_group = "adults_with_comorbidities",
    
    RECORD_antiviral_setup,
                            
    toggle_number_of_runs = 100,
    toggle_cluster_number = 4,
    
    toggle_stochastic_SO = "off",
    toggle_compare_to_vaccine_effect = "off",
    toggle_antiviral_type = 'paxlovid',
    toggle_sensitivity_analysis = list(),
    pathway_to_care = "fixed", #options: "fixed","realistic"
    toggle_antiviral_delivery_capacity = NA,
    toggle_fixed_antiviral_coverage = 0.2,
    
    antiviral_model_worker = copy_function_into_cluster
    
){
 
  ### Step one: create list of scenarios
  manager_scenario_dataframe = crossing(
                                vax_scenario = LIST_vax_scenario,
                                antiviral_target_group = LIST_antiviral_target_group,
                                vax_scenario_risk_group = toggle_high_risk_group
                                )
  
  load(file = '1_inputs/antiviral_effectiveness.Rdata' )
  source(paste(getwd(),"/workshop - new sampling severe outcome projections variables.R",sep=""))
  source(paste(getwd(),"/workshop - new apply severe outcome projections to scenario.R",sep=""))
  
  copy_sampling_fx_into_cluster = stochastic_severe_outcomes_sampling
  copy_application_fx_into_cluster = stochastic_severe_outcomes_application
  #____________________________________
  
  
  
  ### Step two: set parallel runs
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

        local_LIST_antiviral_start_date = LIST_antiviral_start_date,
        local_antiviral_type = toggle_antiviral_type,
        local_stochastic_SO = toggle_stochastic_SO,
        local_compare_to_vaccine_effect = toggle_compare_to_vaccine_effect,
        local_sensitivity_analysis = toggle_sensitivity_analysis,
        local_pathway_to_care = pathway_to_care,
        local_antiviral_delivery_capacity = toggle_antiviral_delivery_capacity,
        local_fixed_antiviral_coverage = toggle_fixed_antiviral_coverage,
        local_antiviral_effectiveness = antiviral_effectiveness,
        
        stochastic_severe_outcomes_sampling = copy_sampling_fx_into_cluster,
        stochastic_severe_outcomes_application = copy_application_fx_into_cluster
      )
      
    }
  })
  
  parallel::stopCluster(CLUSTER)
  #____________________________________
  
  
  
  ### Step three: summary over runs
 summary_over_runs <-
    RECORD_antiviral_model_simulations %>%
    group_by(vax_scenario, vax_scenario_risk_group, antiviral_target_group, outcome,intervention, evaluation_group) %>%
    dplyr::summarise(
      intervention_doses_delivered = mean(intervention_doses_delivered),
      
      average = mean(n),
      sd = sd(n),
      UCI = average - qnorm(0.975) * sd,
      LCI = average - qnorm(0.025) * sd, 
      percentage = mean(percentage),
      percentage_sd = sd(percentage),
      UCI_percentage = average - qnorm(0.975) * percentage_sd,
      LCI_percentage = average - qnorm(0.025) * percentage_sd, 
      
      .groups = 'keep'
    )  %>%
    select(-sd, - percentage_sd)
  
  summary_over_runs_tidy = summary_over_runs %>%
    pivot_longer(
      cols = 8:ncol(summary_over_runs) ,
      names_to = 'result',
      values_to = 'value'
    ) 
  
  workshop = summary_over_runs_tidy %>%
    filter(result %in% c('average','UCI','LCI')) %>%
    mutate(result = paste(result,'_doses_per_outcome_averted',sep=''),
           value = intervention_doses_delivered/value) 
  
  summary_over_runs_tidy = rbind(summary_over_runs_tidy,workshop) %>%
    select(-intervention_doses_delivered) %>%
    mutate(evaluation_group = case_when(evaluation_group == 'overall' ~ 'pop_level', TRUE ~ evaluation_group))
  

  #monitor if daily capacity being used or not enough seeking/accessing care
  if (pathway_to_care == 'realistic'){
    antiviral_rollout_capacity_utilised = round(
      100 * length_antiviral_delivery_tracker / (
        local_antiviral_delivery_capacity * antiviral_delivery_length
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
  } 
  #____________________________________________________________________________
  

  if (length(toggle_sensitivity_analysis)>0){
    summary_over_runs_tidy = summary_over_runs_tidy %>% mutate(sensitivity_analysis = paste(names(toggle_sensitivity_analysis),toggle_sensitivity_analysis))
  }
  
  return(summary_over_runs_tidy) 
}
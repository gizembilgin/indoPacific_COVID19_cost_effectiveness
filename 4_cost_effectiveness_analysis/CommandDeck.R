
#rm(list = ls())



### LOADING ####################################################################
require(beepr); require(parallel); require(foreach); require(ids); require(readr); require(ggplot2); require(tidyverse); 
source(paste(getwd(),"/(function)_CEA_worker.R",sep=""),local=TRUE)
################################################################################



### TOGGLES ####################################################################
CEA_risk_group = this_risk_group = "adults_with_comorbidities"
LIST_CEA_settings = list("PNG_low_beta")
LIST_booster_vax_scenarios = list(
  "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule"
  # "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule"                    
  # "catchup campaign for all adults: assume booster to all adults who have previously completed their primary schedule but have not recieved a booster"                       
  # "catchup campaign for high-risk adults: assume booster to high-risk adults who have previously completed their primary schedule but have not recieved a booster"           
  # "all willing adults vaccinated with a primary schedule"  
)
LIST_antiviral_elig_groups = list(
  "adults_with_comorbidities" 
  # "all_adults" 
  # "unvaccinated_adults" 
)
LIST_antiviral_types = list(
  #"molunipiravir"          
  "nirmatrelvir_ritonavir"
)

TOGGLE_perspective = "healthcare" #options: societal, healthcare
TOGGLE_uncertainty = "rand" #fixed or rand
TOGGLE_numberOfRuns = 100
TOGGLE_clusterNumber = 1
TOGGLE_discounting_rate = 0.03
TOGGLE_longCOVID = "off"
TOGGLE_antiviral_cost_scenario = "middle_income_cost"# options: low_generic_cost,middle_income_cost, high_income_cost
TORNADO_PLOT_OVERRIDE = list()
this_sampling_strategy = "empirical_distribution"

if (exists("CommandDeck_CONTROLS") == FALSE){CommandDeck_CONTROLS = list()}
if (length(CommandDeck_CONTROLS)>0){
  TORNADO_PLOT_OVERRIDE              = CommandDeck_CONTROLS
  if("TOGGLE_perspective" %in% names(CommandDeck_CONTROLS))        {TOGGLE_perspective                 = CommandDeck_CONTROLS$TOGGLE_perspective}
  if("TOGGLE_uncertainty" %in% names(CommandDeck_CONTROLS))        {TOGGLE_uncertainty                 = CommandDeck_CONTROLS$TOGGLE_uncertainty}
  if("LIST_booster_vax_scenarios" %in% names(CommandDeck_CONTROLS)){LIST_booster_vax_scenarios         = CommandDeck_CONTROLS$LIST_booster_vax_scenarios}
  if("LIST_antiviral_elig_groups" %in% names(CommandDeck_CONTROLS)){LIST_antiviral_elig_groups         = CommandDeck_CONTROLS$LIST_antiviral_elig_groups}
  if("LIST_antiviral_types" %in% names(CommandDeck_CONTROLS))      {LIST_antiviral_types               = CommandDeck_CONTROLS$LIST_antiviral_types}
  if("TOGGLE_discounting_rate" %in% names(CommandDeck_CONTROLS))   {TOGGLE_discounting_rate            = CommandDeck_CONTROLS$TOGGLE_discounting_rate}
  if("TOGGLE_longCOVID" %in% names(CommandDeck_CONTROLS))          {TOGGLE_longCOVID                   = CommandDeck_CONTROLS$TOGGLE_longCOVID}
  if("TOGGLE_antiviral_cost_scenario" %in% names(CommandDeck_CONTROLS)){TOGGLE_antiviral_cost_scenario = CommandDeck_CONTROLS$TOGGLE_antiviral_cost_scenario}
  if("sampling_strategy" %in% names(CommandDeck_CONTROLS))             {this_sampling_strategy         = CommandDeck_CONTROLS$sampling_strategy}
}
if (TOGGLE_uncertainty == "fixed"){TOGGLE_numberOfRuns = 1;TOGGLE_clusterNumber = 1}
################################################################################



### MODEL RUN ##################################################################
numberOfRunsPerCluster = TOGGLE_numberOfRuns/TOGGLE_clusterNumber
CLUSTER <- parallel::makeCluster(TOGGLE_clusterNumber) # create cluster
doParallel::registerDoParallel(CLUSTER) # activate cluster

system.time({
  CommandDeck_result_long <- foreach::foreach(
    model_run_number = c(1:TOGGLE_clusterNumber),
    .packages = c('tidyverse','ids'),
    .combine = rbind,
    .inorder = FALSE
  )  %dopar% {
    
    CEA_worker(
      numberOfRunsPerCluster,
      CEA_risk_group,
      LIST_CEA_settings,
      LIST_booster_vax_scenarios,
      LIST_antiviral_elig_groups,
      LIST_antiviral_types,
      this_sampling_strategy,
      TOGGLE_uncertainty,
      TOGGLE_longCOVID,
      TOGGLE_discounting_rate,
      TOGGLE_antiviral_cost_scenario,
      TORNADO_PLOT_OVERRIDE
    )
    
  }
})
#t = 102.11/99.80  seconds for 10 runs of societal/healthcare ; NB: using one cluster and antiviral model also running at the same time
#i.e., 3 hours per settings without parallel running
parallel::stopCluster(CLUSTER)

if (TOGGLE_numberOfRuns>10){
  if(shapiro.test(CommandDeck_result_long$interventionCost)$p.value > 0.05 |
     shapiro.test(CommandDeck_result_long$healthcareCostAverted)$p.value  > 0.05 |
     shapiro.test(CommandDeck_result_long$QALYs)$p.value > 0.05 |
     shapiro.test(CommandDeck_result_long$death)$p.value > 0.05 |
     shapiro.test(CommandDeck_result_long$hosp)$p.value > 0.05){
    warning("Model outputs are not normally distributed")
  }
  if(TOGGLE_perspective == "societal"){
    if(shapiro.test(CommandDeck_result_long$productivityLoss)$p.value > 0.05){
      warning("Model outputs are not normally distributed")
    }
  }
  
}

#calculating 'expected' of each
CommandDeck_result = CommandDeck_result_long %>%
  mutate(netCost = interventionCost - healthcareCostAverted - productivityLoss,
         cost_per_QALY_averted = netCost/QALYs,
         cost_per_death_averted = netCost/death,
         cost_per_hosp_averted = netCost/hosp) %>%
  pivot_longer(cols = c("interventionCost", "healthcareCostAverted","productivityLoss", "QALYs", "death","hosp","netCost","cost_per_QALY_averted","cost_per_death_averted","cost_per_hosp_averted"),
               names_to = "variable",
               values_to = "value_raw") %>%
  mutate(variable_type = case_when(
    variable %in% c("QALYs","death","hosp") ~ "outcome",
    variable %in% c("interventionCost", "healthcareCostAverted","productivityLoss","netCost") ~ "cost",
    variable %in% c("cost_per_QALY_averted","cost_per_death_averted","cost_per_hosp_averted") ~ "ICER"
  )) 
if(TOGGLE_numberOfRuns == 1){
  CommandDeck_result = CommandDeck_result %>%
    group_by(setting,booster_vax_scenario,antiviral_scenario,variable_type,variable) %>%
    summarise(mean = mean(value_raw),
              .groups = "keep"
    )  
} else{
  CommandDeck_result = CommandDeck_result %>%
    group_by(setting,booster_vax_scenario,antiviral_scenario,variable_type,variable) %>%
    summarise(mean = mean(value_raw),
              LPI = mean(value_raw)-qt(.975,df=(TOGGLE_numberOfRuns-1))*sd(value_raw)*sqrt(1+(1/TOGGLE_numberOfRuns)),
              UPI = mean(value_raw)+qt(.975,df=(TOGGLE_numberOfRuns-1))*sd(value_raw)*sqrt(1+(1/TOGGLE_numberOfRuns)),
              .groups = "keep"
    )  
}

CommandDeck_result_long = CommandDeck_result_long %>%
  pivot_longer(cols = c("QALYs","death","hosp"),
               names_to = "outcome",
               values_to = "count_outcomes_averted") %>%
  mutate(netCost = interventionCost - healthcareCostAverted - productivityLoss,
         cost_per_outcome_averted = netCost / count_outcomes_averted)

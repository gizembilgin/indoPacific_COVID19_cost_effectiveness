
require(readr); require(ggplot2); require(tidyverse); require(beepr)

source(paste(getwd(),"/(function)_sample_compartmentalModel_run.R",sep=""),local=TRUE)
source(paste(getwd(),"/(function)_outcomesAverted_estimator.R",sep=""),local=TRUE)
source(paste(getwd(),"/(function)_interventionCost_estimator.R",sep=""),local=TRUE)
source(paste(getwd(),"/(function)_healthCareCostsAverted_estimator.R",sep=""),local=TRUE)
source(paste(getwd(),"/(function)_simulationSummary.R",sep=""),local=TRUE)

load(file = "2_inputs/fitted_distributions.Rdata")

CEA_risk_group = "adults_with_comorbidities"
LIST_CEA_settings = list("IDN")
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

TOGGLE_uncertainty = "rand" #fixed or rand
TOGGLE_numberOfRuns = 100
TOGGLE_discounting_rate = 0.03
TOGGLE_longCOVID = "off"
TOGGLE_antiviral_cost_scenario = "middle_income_cost"# options: low_generic_cost,middle_income_cost, high_income_cost
TORNADO_PLOT_OVERRIDE = list()
this_sampling_strategy = "empirical_distribution"

if (exists("CommandDeck_CONTROLS") == FALSE){CommandDeck_CONTROLS = list()}
if (length(CommandDeck_CONTROLS)>0){
  TORNADO_PLOT_OVERRIDE              = CommandDeck_CONTROLS
  if("TOGGLE_uncertainty" %in% names(CommandDeck_CONTROLS))        {TOGGLE_uncertainty                 = CommandDeck_CONTROLS$TOGGLE_uncertainty}
  if("LIST_booster_vax_scenarios" %in% names(CommandDeck_CONTROLS)){LIST_booster_vax_scenarios         = CommandDeck_CONTROLS$LIST_booster_vax_scenarios}
  if("LIST_antiviral_elig_groups" %in% names(CommandDeck_CONTROLS)){LIST_antiviral_elig_groups         = CommandDeck_CONTROLS$LIST_antiviral_elig_groups}
  if("LIST_antiviral_types" %in% names(CommandDeck_CONTROLS))      {LIST_antiviral_types               = CommandDeck_CONTROLS$LIST_antiviral_types}
  if("TOGGLE_discounting_rate" %in% names(CommandDeck_CONTROLS))   {TOGGLE_discounting_rate            = CommandDeck_CONTROLS$TOGGLE_discounting_rate}
  if("TOGGLE_longCOVID" %in% names(CommandDeck_CONTROLS))          {TOGGLE_longCOVID                   = CommandDeck_CONTROLS$TOGGLE_longCOVID}
  if("TOGGLE_antiviral_cost_scenario" %in% names(CommandDeck_CONTROLS)){TOGGLE_antiviral_cost_scenario = CommandDeck_CONTROLS$TOGGLE_antiviral_cost_scenario}
  if("sampling_strategy" %in% names(CommandDeck_CONTROLS))             {this_sampling_strategy         = CommandDeck_CONTROLS$sampling_strategy}
}
if (TOGGLE_uncertainty == "fixed"){TOGGLE_numberOfRuns = 1}


CommandDeck_result = CommandDeck_result_long = data.frame()

for (ticket in 1:TOGGLE_numberOfRuns){
  
  ###(1/3) Load antiviral model runs
  MASTER_antiviral_simulations <- sample_compartmentalModel_run(LIST_CEA_settings,
                                                                LIST_booster_vax_scenarios,
                                                                LIST_antiviral_elig_groups,
                                                                LIST_antiviral_types,
                                                                sampling_strategy = this_sampling_strategy,
                                                                toggle_uncertainty = TOGGLE_uncertainty)
  
  ###(2/3) Calculate QALYs, intervention costs, and healthcare costs averted
  outcomesAvertedEstimation <- outcomesAverted_estimator(LIST_CEA_settings,
                                                         MASTER_antiviral_simulations,
                                                         toggle_longCOVID = TOGGLE_longCOVID,
                                                         toggle_discounting_rate = TOGGLE_discounting_rate)
  # 0.63 seconds
  #list including QALY_breakdown by setting,outcome_source,booster_vax_scenario,intervention,intervention_target_group, and 
  #               outcomes_averted by setting,outcome,booster_vax_scenario,intervention,intervention_target_group
    
  interventionCost_estimates <- interventionCost_estimator(LIST_CEA_settings,
                                                           MASTER_antiviral_simulations,
                                                           TORNADO_PLOT_OVERRIDE,
                                                           antiviral_cost_scenario = TOGGLE_antiviral_cost_scenario,
                                                           toggle_uncertainty = TOGGLE_uncertainty)
  # 217.43  seconds for all combinations, 3.06 for one booster + one antiviral
  
  healthcareCostEstimation <- healthCareCostsAverted_estimator(LIST_CEA_settings,
                                                               MASTER_antiviral_simulations,
                                                               TORNADO_PLOT_OVERRIDE,
                                                               toggle_uncertainty = TOGGLE_uncertainty)
  # 7.82 seconds for all combinations, 0.39 for one booster + one antiviral

  
  ###(3/3) CEA per setting
  this_result <- simulationSummary(outcomesAvertedEstimation,
                            interventionCost_estimates,
                            healthcareCostEstimation)
  CommandDeck_result_long = rbind(CommandDeck_result_long,this_result)
  
}
### TIME =?

#calculating 'expected' of each
CommandDeck_result = CommandDeck_result_long %>%
  group_by(setting,booster_vax_scenario,antiviral_scenario) %>%
  summarise(interventionCost = mean(interventionCost),
            healthcareCostAverted = mean(healthcareCostAverted),
            QALYs = mean(QALYs),
            death = mean(death),
            hosp = mean(hosp),
            .groups = "keep") %>%
  pivot_longer(cols = c("QALYs","death","hosp"),
               names_to = "outcome",
               values_to = "count_outcomes_averted") %>%
  mutate(netCost = interventionCost - healthcareCostAverted,
         cost_per_outcome_averted = netCost / count_outcomes_averted)

CommandDeck_result_long = CommandDeck_result_long %>%
  pivot_longer(cols = c("QALYs","death","hosp"),
               names_to = "outcome",
               values_to = "count_outcomes_averted") %>%
  mutate(netCost = interventionCost - healthcareCostAverted,
         cost_per_outcome_averted = netCost / count_outcomes_averted)

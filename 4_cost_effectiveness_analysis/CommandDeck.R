
#rm(list = ls())



### LOADING ####################################################################
require(beepr); require(parallel); require(foreach); require(ids); require(readr); require(ggplot2); require(tidyverse); 
source(paste(getwd(),"/(function)_CEA_worker.R",sep=""),local=TRUE)
################################################################################



### TOGGLES ####################################################################
CEA_risk_group = this_risk_group = "adults_with_comorbidities"
LIST_CEA_settings = list("PNG_low_beta","TLS")
LIST_booster_vax_scenarios = list(
  "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule"
   ,"all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule"                    
   ,"catchup campaign for all adults: assume booster to all adults who have previously completed their primary schedule but have not recieved a booster"                       
   ,"catchup campaign for high-risk adults: assume booster to high-risk adults who have previously completed their primary schedule but have not recieved a booster"           
   ,"all willing adults vaccinated with a primary schedule"  
)
LIST_antiviral_elig_groups = list(
  "adults_with_comorbidities" 
   ,"all_adults" 
   ,"unvaccinated_adults" 
)
LIST_antiviral_types = list(
  "molunipiravir"          
  ,"nirmatrelvir_ritonavir"
)

TOGGLE_perspective = "societal" #options: societal, healthcare
TOGGLE_uncertainty = "rand" #fixed or rand
TOGGLE_numberOfRuns = 10#1000
TOGGLE_clusterNumber = 5 #4
TOGGLE_discounting_rate = 0.03
TOGGLE_longCOVID = "off"
TOGGLE_antiviral_cost_scenario = "middle_income_cost"# options: low_generic_cost,middle_income_cost, high_income_cost
TORNADO_PLOT_OVERRIDE = list()
DECISION_save_result = "N" #"Y"
DECISION_sampling_strategy = "empirical_distribution" #options: "single_run" & "empirical_distribution", NB: need to force "single_run" for Net <-> incremental to align for SM table, otherwise sampling from distribution of both
DECISION_CEA_agreement = "Y"

if (exists("CommandDeck_CONTROLS") == FALSE){CommandDeck_CONTROLS = list()}
if (length(CommandDeck_CONTROLS)>0){
  TORNADO_PLOT_OVERRIDE              = CommandDeck_CONTROLS
  if("TOGGLE_perspective" %in% names(CommandDeck_CONTROLS))        {TOGGLE_perspective                 = CommandDeck_CONTROLS$TOGGLE_perspective}
  if("TOGGLE_uncertainty" %in% names(CommandDeck_CONTROLS))        {TOGGLE_uncertainty                 = CommandDeck_CONTROLS$TOGGLE_uncertainty}
  if("TOGGLE_numberOfRuns" %in% names(CommandDeck_CONTROLS))       {TOGGLE_numberOfRuns                = CommandDeck_CONTROLS$TOGGLE_numberOfRuns}  
  if("TOGGLE_clusterNumber" %in% names(CommandDeck_CONTROLS))      {TOGGLE_clusterNumber               = CommandDeck_CONTROLS$TOGGLE_clusterNumber}
  if("DECISION_save_result" %in% names(CommandDeck_CONTROLS))      {DECISION_save_result               = CommandDeck_CONTROLS$DECISION_save_result}
  if("LIST_booster_vax_scenarios" %in% names(CommandDeck_CONTROLS)){LIST_booster_vax_scenarios         = CommandDeck_CONTROLS$LIST_booster_vax_scenarios}
  if("LIST_antiviral_elig_groups" %in% names(CommandDeck_CONTROLS)){LIST_antiviral_elig_groups         = CommandDeck_CONTROLS$LIST_antiviral_elig_groups}
  if("LIST_antiviral_types" %in% names(CommandDeck_CONTROLS))      {LIST_antiviral_types               = CommandDeck_CONTROLS$LIST_antiviral_types}
  if("TOGGLE_discounting_rate" %in% names(CommandDeck_CONTROLS))   {TOGGLE_discounting_rate            = CommandDeck_CONTROLS$TOGGLE_discounting_rate}
  if("TOGGLE_longCOVID" %in% names(CommandDeck_CONTROLS))          {TOGGLE_longCOVID                   = CommandDeck_CONTROLS$TOGGLE_longCOVID}
  if("TOGGLE_antiviral_cost_scenario" %in% names(CommandDeck_CONTROLS)){TOGGLE_antiviral_cost_scenario = CommandDeck_CONTROLS$TOGGLE_antiviral_cost_scenario}
  if("sampling_strategy" %in% names(CommandDeck_CONTROLS))             {DECISION_sampling_strategy         = CommandDeck_CONTROLS$sampling_strategy}
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
      DECISION_sampling_strategy,
      TOGGLE_uncertainty,
      TOGGLE_longCOVID,
      TOGGLE_discounting_rate,
      TOGGLE_antiviral_cost_scenario,
      TORNADO_PLOT_OVERRIDE
    )
    
  }
})
#t = 102.11/99.80  seconds for 10 runs of societal/healthcare ; NB: using one cluster and antiviral model also running at the same time
#32 minutes per setting for 100 runs
#i.e., 3 hours per settings without parallel running
parallel::stopCluster(CLUSTER)

normality_tracker = data.frame()
if (TOGGLE_numberOfRuns>10){
  for (this_setting in unique(CommandDeck_result_long$setting)){
    for (this_antiviral_type in unique(CommandDeck_result_long$antiviral_type)){
      for (this_booster_scenario in unique(CommandDeck_result_long$booster_vax_scenario)){
       
        check_normality_df = CommandDeck_result_long %>%
          filter(setting == this_setting &
                   antiviral_type == this_antiviral_type &
                   booster_vax_scenario == this_booster_scenario)
        this_row = data.frame(setting = this_setting,
                              antiviral_type = this_antiviral_type,
                              booster_vax_scenario = this_booster_scenario,
                              interventionCost = FALSE,
                              healthcareCostAverted = FALSE,
                              QALYs = FALSE,
                              hosp = FALSE,
                              death = FALSE,
                              productivityLoss = FALSE)
        
        if (nrow(check_normality_df)>0){
          if (shapiro.test(check_normality_df$interventionCost)$p.value < 0.05){this_row$interventionCost = TRUE}
          if (shapiro.test(check_normality_df$healthcareCostAverted)$p.value < 0.05){this_row$healthcareCostAverted = TRUE}
          if (shapiro.test(check_normality_df$QALYs)$p.value < 0.05){this_row$QALYs = TRUE}
          if (shapiro.test(check_normality_df$death)$p.value < 0.05){this_row$death = TRUE}
          if(TOGGLE_perspective == "societal"){
            if(shapiro.test(check_normality_df$productivityLoss)$p.value < 0.05){this_row$productivityLoss = TRUE}
          }
          if(nrow(check_normality_df[check_normality_df$hosp>0,])){
            if (shapiro.test(check_normality_df$hosp)$p.value < 0.05){this_row$hosp = TRUE}
          }
        }
        normality_tracker = rbind(normality_tracker,this_row)
      }
    }
  }
}
#On inspection, intervention costs not normal when no booster, hosp not normal when molnupiravir etc. i.e., as expected

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
    group_by(evaluation_level,setting,booster_vax_scenario,antiviral_type,antiviral_target_group,variable_type,variable) %>%
    summarise(mean = mean(value_raw),
              .groups = "keep"
    )  
} else{
  CommandDeck_result = CommandDeck_result %>%
    group_by(evaluation_level,setting,booster_vax_scenario,antiviral_type,antiviral_target_group,variable_type,variable) %>%
    summarise(mean = mean(value_raw),
              sd = sd(value_raw),
              LPI = mean(value_raw)-qt(.975,df=(TOGGLE_numberOfRuns-1))*sd(value_raw)*sqrt(1+(1/TOGGLE_numberOfRuns)),
              UPI = mean(value_raw)+qt(.975,df=(TOGGLE_numberOfRuns-1))*sd(value_raw)*sqrt(1+(1/TOGGLE_numberOfRuns)),
              .groups = "keep"
    )  
  
  if (DECISION_CEA_agreement == "Y"){ #force ICER to align with (incremental) netCost/ (incremental) netOutcomesAverted
    workshop_outcome = CommandDeck_result %>%
      filter(variable_type == "outcome" &
               evaluation_level == "incremental") %>%
      ungroup() %>%
      select(-LPI,-UPI,-variable_type) %>%
      rename(mean_outcome = mean,
             sd_outcome = sd,
             outcome = variable)
    
    workshop_cost = CommandDeck_result %>%
      filter( variable == "netCost" &
                evaluation_level == "incremental")%>%
      ungroup() %>%
      select(-LPI,-UPI,-variable_type,-variable) %>%
      rename(mean_cost = mean,
             sd_cost = sd)
    
    workshop = workshop_outcome %>%
      left_join(workshop_cost,by = join_by(evaluation_level,setting, booster_vax_scenario, antiviral_type,antiviral_target_group)) %>%
      mutate(
        mean = mean_cost/mean_outcome,
        sd = mean_cost/mean_outcome * sqrt(
          ((sd_cost/mean_cost)^2)+((sd_outcome)/mean_outcome)^2
        ),
        LPI = mean - qt(.975,df=(TOGGLE_numberOfRuns-1))*sd*sqrt(1+(1/TOGGLE_numberOfRuns)) ,
        UPI = mean + qt(.975,df=(TOGGLE_numberOfRuns-1))*sd*sqrt(1+(1/TOGGLE_numberOfRuns))
      ) %>%
      mutate(variable_type = "ICER",
             variable = paste("cost_per_",outcome,"_averted",sep=""),
             variable = gsub("QALYs","QALY",variable)) %>%
      select(evaluation_level,setting,booster_vax_scenario,antiviral_type,antiviral_target_group,variable_type,variable,mean,sd,LPI,UPI)
    
    CommandDeck_result = CommandDeck_result %>%
      filter(variable_type != "ICER")
    CommandDeck_result = rbind(CommandDeck_result,workshop); rm(workshop,workshop_cost,workshop_outcome)
    
  }
}

CommandDeck_result = CommandDeck_result %>%
  mutate(discounting_rate = this_discounting_rate,
         antiviral_cost = this_antiviral_cost_scenario,
         perspective = this_perspective) 

CommandDeck_result_long = CommandDeck_result_long %>%
  pivot_longer(cols = c("QALYs","death","hosp"),
               names_to = "outcome",
               values_to = "count_outcomes") %>%
  mutate(netCost = interventionCost - healthcareCostAverted - productivityLoss,
         cost_per_outcome_averted = netCost / count_outcomes,
         perspective = TOGGLE_perspective,
         discounting_rate = TOGGLE_discounting_rate ,
         antiviral_cost = TOGGLE_antiviral_cost_scenario)

if (DECISION_save_result == "Y"){
  CommandDeck_result_list = list(CommandDeck_result_long = CommandDeck_result_long,
                                 CommandDeck_result = CommandDeck_result)
  temp_name = ''
  time = Sys.time()
  time = gsub(':','-',time)
  time = paste(temp_name,time,sep='')
  
  save(CommandDeck_result_list, file = paste("x_results/CommandDeck_result_list_",this_risk_group,"_",TOGGLE_perspective,"_perspective_",time,".Rdata",sep=''))
}


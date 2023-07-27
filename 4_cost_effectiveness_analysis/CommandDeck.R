
#rm(list = ls())



### LOADING ####################################################################
require(beepr); require(parallel); require(foreach); require(ids); require(readr); require(ggplot2); require(tidyverse); 
source(paste(getwd(),"/(function)_CEA_worker.R",sep=""),local=TRUE)
################################################################################



### TOGGLES ####################################################################
CEA_risk_group = this_risk_group = "adults_with_comorbidities"
LIST_CEA_settings = list("PNG_low_beta","TLS","FJI","IDN") #list("PNG_low_beta","TLS","FJI","IDN")
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
LIST_perspectives = list("healthcare","societal") #options: societal, healthcare
LIST_antiviral_cost_scenario = c("low_generic_cost","middle_income_cost", "high_income_cost")# options: low_generic_cost,middle_income_cost, high_income_cost
LIST_discounting_rate = c(0.00,0.03,0.05) #options(0-0.1)

TOGGLE_uncertainty = "rand" #fixed or rand
TOGGLE_numberOfRuns = 20 #1000
TOGGLE_clusterNumber = 5 #5
TOGGLE_longCOVID = "off"
TORNADO_PLOT_OVERRIDE = list()

DECISION_save_result = "N" 
#options: "Y", "N" - should this CEA run be saved? (line 250 of this program)
DECISION_sampling_strategy = "single_run" 
#options: "single_run" takes the results of a single run of the antiviral model
#         "empirical_distribution" samples from the empirical distribution created by the all antiviral model runs available
DECISION_CEA_agreement = "Y"
#Options: "Y" calculates ICER from mean(cost)/mean(outcomes averted)
#         "N" calculates the ICER per run, and then takes the mean of these values
DECISION_include_net = "N" 
#Options: "Y" includes "net" values of QALYs and $ by booster dose and antiviral scenarios - this is for the supplementary material.
#                 With this option, incremental values will be written over my the differences in sampled net scenarios. Sampled net and incremental values
#                 will not align when DECISION_sampling_strategy "empirical_distribution" since the net and incremental values are sampled independently. 
#         "N" only includes the incremental effects of booster doses and antiviral scenarios - this is for generating results for the R Shiny.
#                 Including only incremental effects means that the distributions of inpatient costs, for example, are sampled less times.
#                 This speeds up the model when running thousands of times.

if (exists("CommandDeck_CONTROLS") == FALSE){CommandDeck_CONTROLS = list()}
if (length(CommandDeck_CONTROLS)>0){
  
  TORNADO_PLOT_OVERRIDE              = CommandDeck_CONTROLS
  
  if("LIST_perspectives" %in% names(CommandDeck_CONTROLS))         {LIST_perspectives                 = CommandDeck_CONTROLS$LIST_perspectives}
  if("TOGGLE_uncertainty" %in% names(CommandDeck_CONTROLS))        {TOGGLE_uncertainty                 = CommandDeck_CONTROLS$TOGGLE_uncertainty}
  if("TOGGLE_numberOfRuns" %in% names(CommandDeck_CONTROLS))       {TOGGLE_numberOfRuns                = CommandDeck_CONTROLS$TOGGLE_numberOfRuns}  
  if("TOGGLE_clusterNumber" %in% names(CommandDeck_CONTROLS))      {TOGGLE_clusterNumber               = CommandDeck_CONTROLS$TOGGLE_clusterNumber}
  if("LIST_discounting_rate" %in% names(CommandDeck_CONTROLS))     {LIST_discounting_rate            = CommandDeck_CONTROLS$LIST_discounting_rate}
  if("TOGGLE_longCOVID" %in% names(CommandDeck_CONTROLS))          {TOGGLE_longCOVID                   = CommandDeck_CONTROLS$TOGGLE_longCOVID}
  if("LIST_antiviral_cost_scenario" %in% names(CommandDeck_CONTROLS)){LIST_antiviral_cost_scenario = CommandDeck_CONTROLS$LIST_antiviral_cost_scenario}
  
  if("DECISION_save_result" %in% names(CommandDeck_CONTROLS))      {DECISION_save_result               = CommandDeck_CONTROLS$DECISION_save_result}
  if("DECISION_include_net" %in% names(CommandDeck_CONTROLS))      {DECISION_include_net               = CommandDeck_CONTROLS$DECISION_include_net}
  if("DECISION_sampling_strategy" %in% names(CommandDeck_CONTROLS)){DECISION_sampling_strategy         = CommandDeck_CONTROLS$DECISION_sampling_strategy}
  
  if("LIST_CEA_settings" %in% names(CommandDeck_CONTROLS))         {LIST_CEA_settings                  = CommandDeck_CONTROLS$LIST_CEA_settings}
  if("LIST_booster_vax_scenarios" %in% names(CommandDeck_CONTROLS)){LIST_booster_vax_scenarios         = CommandDeck_CONTROLS$LIST_booster_vax_scenarios}
  if("LIST_antiviral_elig_groups" %in% names(CommandDeck_CONTROLS)){LIST_antiviral_elig_groups         = CommandDeck_CONTROLS$LIST_antiviral_elig_groups}
  if("LIST_antiviral_types" %in% names(CommandDeck_CONTROLS))      {LIST_antiviral_types               = CommandDeck_CONTROLS$LIST_antiviral_types}

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
    )
    
  }
})
#t = 102.11/99.80  seconds for 10 runs of societal/healthcare ; NB: using one cluster and antiviral model also running at the same time
#32 minutes per setting for 100 runs
#i.e., 3 hours per settings without parallel running
parallel::stopCluster(CLUSTER)

# normality_tracker = data.frame()
# if (TOGGLE_numberOfRuns>10){
#   for (this_setting in unique(CommandDeck_result_long$setting)){
#     for (this_antiviral_type in unique(CommandDeck_result_long$antiviral_type)){
#       for (this_antiviral_target_group in unique(CommandDeck_result_long$antiviral_target_group)){
#         for (this_booster_scenario in unique(CommandDeck_result_long$booster_vax_scenario)){
#           for (this_antiviral_cost_scenario in unique(CommandDeck_result_long$antiviral_cost_scenario)){
#             if (!(this_booster_scenario == "no booster dose" & this_antiviral_type == "no antiviral")){
#               check_normality_df = CommandDeck_result_long %>%
#                 filter(setting == this_setting &
#                          antiviral_type == this_antiviral_type &
#                          booster_vax_scenario == this_booster_scenario &
#                          antiviral_cost_scenario == this_antiviral_cost_scenario &
#                          antiviral_target_group == this_antiviral_target_group &
#                          discounting_rate == unique(CommandDeck_result_long$discounting_rate)[1] &
#                          evaluation_level == "incremental" &
#                          perspective == unique(CommandDeck_result_long$perspective)[[1]])
#               this_row = data.frame(setting = this_setting,
#                                     antiviral_type = this_antiviral_type,
#                                     booster_vax_scenario = this_booster_scenario,
#                                     antiviral_target_group = this_antiviral_target_group,
#                                     antiviral_cost_scenario = this_antiviral_cost_scenario,
#                                     interventionCost = FALSE,
#                                     healthcareCostAverted = FALSE,
#                                     QALYs = FALSE,
#                                     hosp = FALSE,
#                                     death = FALSE,
#                                     productivityLoss = FALSE)
#               
#               if (nrow(check_normality_df)>5000){
#                 #the shapiro.test can only handle up to 5000 values
#                 check_normality_df = check_normality_df[c(1:5000),] 
#               }
#               
#               if (nrow(check_normality_df)>0){
#                 if (shapiro.test(check_normality_df$interventionCost)$p.value < 0.05){this_row$interventionCost = TRUE}
#                 if (shapiro.test(check_normality_df$healthcareCostAverted)$p.value < 0.05){this_row$healthcareCostAverted = TRUE}
#                 if (shapiro.test(check_normality_df$QALYs)$p.value < 0.05){this_row$QALYs = TRUE}
#                 if (shapiro.test(check_normality_df$death)$p.value < 0.05){this_row$death = TRUE}
#                 if("societal" == unique(CommandDeck_result_long$perspective)[[1]]){
#                   if(shapiro.test(check_normality_df$productivityLoss[check_normality_df$perspective == "societal"])$p.value < 0.05){this_row$productivityLoss = TRUE}
#                 }
#                 if(nrow(check_normality_df[check_normality_df$hosp>0,])){
#                   if (shapiro.test(check_normality_df$hosp)$p.value < 0.05){this_row$hosp = TRUE}
#                 }
#               }
#               normality_tracker = rbind(normality_tracker,this_row)
#             }
#           }
#         }
#       }
#     }
#   }
# }
#On inspection, intervention costs not normal when no booster, hosp not normal when molnupiravir etc. i.e., as expected

#calculating 'expected' of each
CommandDeck_result = CommandDeck_result_long %>%
  mutate(netCost = 
           case_when(
             evaluation_level == "incremental" ~ interventionCost - healthcareCostAverted - productivityLoss , #healthCostAverted and productivityLoss averted but interventioncost spent
             evaluation_level == "net" ~ healthcareCostAverted + productivityLoss + interventionCost
             ),
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
    group_by(evaluation_level,perspective,discounting_rate,setting,booster_vax_scenario,antiviral_cost_scenario,antiviral_type,antiviral_target_group,variable_type,variable) %>%
    summarise(mean = mean(value_raw),
              .groups = "keep"
    )  
} else{
  CommandDeck_result = CommandDeck_result %>%
    group_by(evaluation_level,perspective,discounting_rate,setting,booster_vax_scenario,antiviral_cost_scenario,antiviral_type,antiviral_target_group,variable_type,variable) %>%
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
      left_join(workshop_cost,by = join_by(evaluation_level,perspective,discounting_rate,setting, booster_vax_scenario, antiviral_cost_scenario,antiviral_type,antiviral_target_group)) %>%
      mutate(
        mean = mean_cost/mean_outcome,
        sd = mean_cost/mean_outcome * sqrt(
          ((sd_cost/mean_cost)^2)+((sd_outcome)/mean_outcome)^2
        ),
        LPI = mean - qt(.975,df=(TOGGLE_numberOfRuns-1))*sd*sqrt(1+(1/TOGGLE_numberOfRuns)) ,
        UPI = mean + qt(.975,df=(TOGGLE_numberOfRuns-1))*sd*sqrt(1+(1/TOGGLE_numberOfRuns))
      ) %>%
      mutate(variable_type = "ICER",
             variable = paste("cost_per_",outcome,"_averted",sep="")) %>%
      select(evaluation_level,perspective,discounting_rate,setting,booster_vax_scenario,antiviral_cost_scenario,antiviral_type,antiviral_target_group,variable_type,variable,mean,sd,LPI,UPI)
    
    CommandDeck_result = CommandDeck_result %>%
      filter(variable_type != "ICER")
    CommandDeck_result = rbind(CommandDeck_result,workshop); rm(workshop,workshop_cost,workshop_outcome)
    
  }
}

CommandDeck_result_long = CommandDeck_result_long %>%
  pivot_longer(cols = c("QALYs","death","hosp"),
               names_to = "outcome",
               values_to = "count_outcomes") %>%
  mutate(netCost =
           case_when(
             evaluation_level == "incremental" ~ interventionCost - healthcareCostAverted - productivityLoss , #healthCostAverted and productivityLoss averted but interventioncost spent
             evaluation_level == "net" ~ healthcareCostAverted + productivityLoss + interventionCost
           ),
         cost_per_outcome_averted = netCost / count_outcomes) %>%
  select(evaluation_level,perspective,discounting_rate,setting,booster_vax_scenario,antiviral_cost_scenario,antiviral_type,antiviral_target_group,
         interventionCost,healthcareCostAverted,productivityLoss,netCost,
         outcome,count_outcomes,cost_per_outcome_averted,discounting_rate,run_ID)

CommandDeck_result_list = list(CommandDeck_result_long = CommandDeck_result_long,
                               CommandDeck_result = CommandDeck_result)

if (DECISION_save_result == "Y"){
  temp_name = ''
  time = Sys.time()
  time = gsub(':','-',time)
  time = paste(temp_name,time,sep='')
  save(CommandDeck_result_list, file = paste("x_results/CommandDeck_result_list_",this_risk_group,"_",time,".Rdata",sep=''))
}


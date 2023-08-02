### RUN SCENARIOS FOR:
### (1) (plot)_cost_effectiveness_acceptability_curve
### (2) (plot)_ICER_table
### (3) (plot)_incremental_plane
##Note: will use LIST_CEA_settings,LIST_booster_vax_scenarios,LIST_antiviral_elig_groups,LIST_antiviral_types from CommandDeck (line 15)


### RUN SIMULATIONS ############################################################
CommandDeck_CONTROLS =
  list(
    LIST_CEA_settings = list("PNG_low_beta","TLS","FJI","IDN"),
    LIST_perspectives = c("healthcare","societal"),
    LIST_discounting_rate = seq(0,0.05,by=0.01),
    LIST_antiviral_cost_scenario = c("low_generic_cost","middle_income_cost", "high_income_cost"),
    
    TOGGLE_longCOVID = "off",
    TOGGLE_uncertainty = "rand",
    TOGGLE_numberOfRuns = 1000, #1000 eventually
    TOGGLE_clusterNumber = 5,  #3 based on test and time! (workshop - timing probabilistic model runs by number of cores)
    DECISION_include_net = "Y",
    DECISION_sampling_strategy = "single_run"
  )

source(paste(getwd(),"/CommandDeck.R",sep=""))
CommandDeck_CONTROLS = list()
################################################################################



### process results for R Shiny ################################################
# aligning results with buttons
CommandDeck_result_long = CommandDeck_result_long %>%
  mutate(
    setting = case_when(
      setting == "FJI" ~ "Fiji",
      setting == "IDN" ~ "Indonesia",
      setting == "PNG" ~ "Papua New Guinea",
      setting == "TLS" ~ "Timor-Leste",
      TRUE ~ setting
    ),
    booster_vax_scenario = case_when(
      booster_vax_scenario == "booster to all high-risk adults previously willing to be vaccinated" ~ "high risk adults",
      booster_vax_scenario == "booster to all adults previously willing to be vaccinated" ~ "all adults"    ,
      booster_vax_scenario == "booster dose catch-up campaign for all adults" ~ "all adults who have previously completed their primary schedule but have not recieved a booster"  ,
      booster_vax_scenario == "booster dose catch-up campaign for high-risk adults" ~ "high-risk adults who have previously completed their primary schedule but have not recieved a booster"   ,
      booster_vax_scenario == "no booster dose" ~ "no booster",
      TRUE ~ booster_vax_scenario
    ),
    
    antiviral_type = gsub(" 2023-01-01","",antiviral_type),
    antiviral_target_group = gsub("_"," ",antiviral_target_group),
    antiviral_target_group = case_when(
      antiviral_type == "no antiviral" ~ antiviral_type,
      TRUE ~ antiviral_target_group
    ),
    
    perspective = paste(perspective," perspective",sep = ""),      
    discounting_rate = discounting_rate * 100
  ) 
#_____________________________________________


CommandDeck_result = CommandDeck_result %>%
  mutate(
    setting = case_when(
      setting == "FJI" ~ "Fiji",
      setting == "IDN" ~ "Indonesia",
      setting == "PNG" ~ "Papua New Guinea",
      setting == "TLS" ~ "Timor-Leste",
      TRUE ~ setting
    ),
    booster_vax_scenario = case_when(
      booster_vax_scenario == "booster to all high-risk adults previously willing to be vaccinated" ~ "high risk adults",
      booster_vax_scenario == "booster to all adults previously willing to be vaccinated" ~ "all adults"    ,
      booster_vax_scenario == "booster dose catch-up campaign for all adults" ~ "all adults who have previously completed their primary schedule but have not recieved a booster"  ,
      booster_vax_scenario == "booster dose catch-up campaign for high-risk adults" ~ "high-risk adults who have previously completed their primary schedule but have not recieved a booster"   ,
      booster_vax_scenario == "no booster dose" ~ "no booster",
      TRUE ~ booster_vax_scenario
    ),
    
    antiviral_type = gsub(" 2023-01-01","",antiviral_type),
    antiviral_target_group = gsub("_"," ",antiviral_target_group),
    antiviral_target_group = case_when(
      antiviral_type == "no antiviral" ~ antiviral_type,
      TRUE ~ antiviral_target_group
    ),
    
    perspective = paste(perspective," perspective",sep = ""),      
    discounting_rate = discounting_rate * 100
  ) %>%
  rename(outcome = variable) %>%
  mutate(
    outcome = gsub("cost_per_", "", outcome),
    outcome = gsub("_averted", "", outcome)
  ) 
CommandDeck_result$outcome[CommandDeck_result$outcome == "QALY"] <- "QALYs"
#_____________________________________________


CEAC_dataframe = CommandDeck_result_long %>%
  filter(evaluation_level == "incremental" &
           cost_per_outcome_averted != -Inf &
           cost_per_outcome_averted != Inf) %>%
  group_by(outcome,setting,perspective,discounting_rate,antiviral_cost_scenario,booster_vax_scenario,antiviral_type,antiviral_target_group) %>%
  arrange(cost_per_outcome_averted) %>%
  mutate(row_number = row_number(),
         probability = row_number/TOGGLE_numberOfRuns) %>%
  rename(WTP = cost_per_outcome_averted) %>%
  select(outcome,setting,perspective,discounting_rate,antiviral_cost_scenario,booster_vax_scenario,antiviral_type,antiviral_target_group,probability,WTP)
#_____________________________________________


ICER_table = CommandDeck_result %>%
  filter(evaluation_level == "incremental") %>%
  ungroup() %>%
  filter(variable_type %in% c("ICER","outcome") | outcome == "netCost") %>%
  select(-sd,-evaluation_level)
cost_column = ICER_table %>%
  filter(variable_type == "cost") %>%
  mutate(variable_type = "netCost") %>%
  rename(netCost = mean) %>%
  ungroup() %>%
  select(-variable_type,-outcome,-LPI,-UPI)
cost_column = crossing(cost_column,outcome = unique(ICER_table$outcome[ICER_table$outcome != "netCost"]))
ICER_table = ICER_table %>%
  filter(variable_type != "cost") %>%
  left_join(cost_column, by = join_by(perspective, discounting_rate, setting, booster_vax_scenario, antiviral_cost_scenario, antiviral_type, antiviral_target_group, outcome)) %>%
  pivot_wider(names_from = variable_type,
              values_from = c(mean,LPI,UPI)) %>%
  select(-LPI_outcome,-UPI_outcome) %>%
  rename(count_outcome_averted = mean_outcome,
         net_cost = netCost,
         ICER = mean_ICER) %>%
  mutate(count_outcome_averted = round(count_outcome_averted,digits=1),
         net_cost = round(net_cost),
         ICER = round(ICER),
         LPI_ICER = round(LPI_ICER),
         UPI_ICER = round(UPI_ICER),
         PI = paste(round(LPI_ICER),"to",round(UPI_ICER)),
  ) %>%
  ungroup() %>%
  select(perspective,discounting_rate,setting,booster_vax_scenario,antiviral_cost_scenario,antiviral_type,antiviral_target_group,outcome,count_outcome_averted,net_cost,ICER,LPI_ICER,UPI_ICER)
################################################################################



### save complete results ######################################################
temp_name = ''
time = Sys.time()
time = gsub(':','-',time)
time = paste(temp_name,time,sep='')

complete_results = list(CommandDeck_result_long = CommandDeck_result_long,
                        CommandDeck_result = CommandDeck_result,
                        CEAC_dataframe = CEAC_dataframe,
                        ICER_table = ICER_table)

if (DECISION_include_net == "N"){
  #save outside of GitHub repositry since > 100 MB
  save(complete_results,file = paste0(gsub("/GitHub_vaxAllocation/4_cost_effectiveness_analysis","",rootpath),"/x_results/incremental_complete_CEA_result",time,".Rdata"))
  
  #breakdown into chunks that CAN live in the GitHub repositry
  save(ICER_table              ,file = paste0("x_results/ICER_table",time,".Rdata"))
  save(CommandDeck_result      ,file = paste0("x_results/CommandDeck_result",time,".Rdata"))
  
  #>100 MB
  CEAC_dataframe_part1 = CEAC_dataframe %>% filter(setting %in% c("Indonesia","Fiji"))
  CEAC_dataframe_part2 = CEAC_dataframe %>% filter(!(setting %in% c("Indonesia","Fiji")))
  save(CEAC_dataframe_part1,file = paste0("x_results/CEAC_dataframe_1_",time,".Rdata"))
  save(CEAC_dataframe_part2,file = paste0("x_results/CEAC_dataframe_2_",time,".Rdata"))
  
  CommandDeck_result_long_part1 = CommandDeck_result_long %>% filter(setting %in% c("Indonesia","Fiji"))
  CommandDeck_result_long_part2 = CommandDeck_result_long %>% filter(!(setting %in% c("Indonesia","Fiji")))
  save(CommandDeck_result_long_part1,file = paste0("x_results/CommandDeck_result_long_1_",time,".Rdata"))
  save(CommandDeck_result_long_part2,file = paste0("x_results/CommandDeck_result_long_2_",time,".Rdata"))

} else{
  #save outside of GitHub repositry since > 100 MB
  save(complete_results,file = paste0(gsub("/GitHub_vaxAllocation/4_cost_effectiveness_analysis","",rootpath),"/x_results/net_complete_CEA_result",time,".Rdata"))
}
################################################################################
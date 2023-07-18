### RUN SCENARIOS FOR:
### (1) (plot)_cost_effectiveness_acceptability_curve
### (2) (plot)_ICER_table
### (3) (plot)_incremental_plane
##Note: will use LIST_CEA_settings,LIST_booster_vax_scenarios,LIST_antiviral_elig_groups,LIST_antiviral_types from CommandDeck (line 15)


probab_CommandDeck_result_long = probab_CommandDeck_result = data.frame()

#66 runs in total (2*11*3), need to time one run!
for (this_discounting_rate in seq(0,0.05,by=0.01)){
  for (this_perspective in c("healthcare","societal")){
    for (this_antiviral_cost_scenario in c("low_generic_cost","middle_income_cost", "high_income_cost")){
  
      CommandDeck_CONTROLS =
        list(
          TOGGLE_perspective = this_perspective,
          TOGGLE_discounting_rate = this_discounting_rate,
          TOGGLE_antiviral_cost_scenario = this_antiviral_cost_scenario,
          
          TOGGLE_longCOVID = "off",
          TOGGLE_uncertainty = "rand",
          TOGGLE_numberOfRuns = 100, #1000 eventually
          TOGGLE_clusterNumber = 5,  #4 or 5? test and time! (workshop - timing probabilistic model runs by number of cores)
          DECISION_save_result = "N"
        )

      source(paste(getwd(),"/CommandDeck.R",sep=""))
      
      probab_CommandDeck_result = rbind(probab_CommandDeck_result,
                                        CommandDeck_result)
      
      probab_CommandDeck_result_long = rbind(probab_CommandDeck_result_long,
                                             CommandDeck_result_long)
      
    }
  }
}

CommandDeck_CONTROLS = list()

# aligning results with buttons
CommandDeck_result_long = probab_CommandDeck_result_long %>%
  filter(evaluation_level == "incremental") %>%
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

CommandDeck_result = probab_CommandDeck_result %>%
  filter(evaluation_level == "incremental") %>%
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




source(paste(getwd(),"/(run)_cost_acceptibility_by_WTP.R",sep=""),local=TRUE)

probab_result = list(CommandDeck_result_long = CommandDeck_result_long,
                     CommandDeck_result = CommandDeck_result,
                     CEAC_dataframe = CEAC_dataframe)
temp_name = ''
time = Sys.time()
time = gsub(':','-',time)
time = paste(temp_name,time,sep='')

save(probab_result,file = paste0("x_results/probab_result",time,".Rdata"))
#_______________________________________________________________________________
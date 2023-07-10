### RUN SCENARIOS FOR:
### (1) (plot)_cost_effectiveness_acceptability_curve
### (2) (plot)_ICER_table
### (3) (plot)_incremental_plane
##Note: will use LIST_CEA_settings,LIST_booster_vax_scenarios,LIST_antiviral_elig_groups,LIST_antiviral_types from CommandDeck (line 15)


probab_CommandDeck_result_long = probab_CommandDeck_result = data.frame()

#66 runs in total (2*11*3), need to time one run!
for (this_perspective in c("healthcare","societal")){
  for (this_discounting_rate in seq(0,0.1,by=0.01)){
    for (this_antiviral_cost_scenario in c("low_generic_cost","middle_income_cost", "high_income_cost")){
  
      CommandDeck_CONTROLS =
        list(
          TOGGLE_perspective = this_perspective,
          TOGGLE_discounting_rate = this_discounting_rate,
          TOGGLE_antiviral_cost_scenario = this_antiviral_cost_scenario,
          
          TOGGLE_longCOVID = "off",
          TOGGLE_uncertainty = "rand",
          TOGGLE_numberOfRuns = 1000,
          TOGGLE_clusterNumber = 4,
          DECISION_save_result = "N"
        )

      source(paste(getwd(),"/CommandDeck.R",sep=""))
      
      rows = CommandDeck_result %>%
        mutate(discounting_rate = this_discounting_rate,
               antiviral_cost = this_antiviral_cost_scenario,
               perspective = this_perspective) 
      probab_CommandDeck_result = rbind(probab_CommandDeck_result,rows)
      
      rows = CommandDeck_result_long %>%
        mutate(discounting_rate = this_discounting_rate,
               antiviral_cost = this_antiviral_cost_scenario,
               perspective = this_perspective) 
      probab_CommandDeck_result_long = rbind(probab_CommandDeck_result_long,rows)
      
    }
  }
}

CommandDeck_CONTROLS = list()

CommandDeck_result_long = probab_CommandDeck_result_long
CommandDeck_result = probab_CommandDeck_result
source(paste(getwd(),"/(run)_cost_acceptibility_by_WTP.R",sep=""),local=TRUE)

probab_result = list(CommandDeck_result_long = CommandDeck_result_long,
                     CommandDeck_result = CommandDeck_result,
                     CEAC_dataframe = CEAC_dataframe)

save(probab_result,file = "x_results/probab_result.Rdata")
#_______________________________________________________________________________
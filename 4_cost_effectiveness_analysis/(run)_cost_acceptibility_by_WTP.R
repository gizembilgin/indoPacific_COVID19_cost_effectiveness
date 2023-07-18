

### Calculate probability WTP at different cost-effectiveness thresholds
CEAC_dataframe = data.frame()

for (this_outcome in unique(CommandDeck_result_long$outcome)){
  for (this_setting in unique(CommandDeck_result_long$setting)){
    for (this_perspective in unique(CommandDeck_result_long$perspective)){
      for (this_discounting_rate in unique(CommandDeck_result_long$discounting_rate)){
        for (this_antiviral_cost in unique(CommandDeck_result_long$antiviral_cost)){
          for (this_booster_vax_scenario in unique(CommandDeck_result_long$booster_vax_scenario)){
            for (this_antiviral_type in unique(CommandDeck_result_long$antiviral_type)){
              for (this_antiviral_target_group in unique(CommandDeck_result_long$antiviral_target_group)){
                
                workshop = CommandDeck_result_long %>%
                  filter(outcome == this_outcome &
                           setting == this_setting &
                           perspective == this_perspective &
                           discounting_rate == this_discounting_rate &
                           antiviral_cost == this_antiviral_cost &
                           booster_vax_scenario == this_booster_vax_scenario &
                           antiviral_type == this_antiviral_type &
                           
                           evaluation_level == "incremental" &
                           cost_per_outcome_averted != -Inf &
                           cost_per_outcome_averted != Inf)
                
                if (is.na(this_antiviral_target_group)){
                  workshop = workshop %>% filter(is.na(antiviral_target_group))
                } else{
                  workshop = workshop %>% filter(antiviral_target_group == this_antiviral_target_group)
                }
                
                if (nrow(workshop)>0){
                  min_seq = round(min(workshop$cost_per_outcome_averted))
                  max_seq = round(max(workshop$cost_per_outcome_averted[workshop$count_outcomes != 0] ))
                  interval_seq = (max_seq-min_seq)/100
                  
                  for (this_WTP in seq(min_seq,max_seq,by =interval_seq)){
                    rows = workshop %>%
                      filter(cost_per_outcome_averted <= this_WTP) %>%
                      group_by(outcome,setting,perspective,discounting_rate,antiviral_cost,booster_vax_scenario,antiviral_type,antiviral_target_group) %>%
                      summarise(probability = n() / TOGGLE_numberOfRuns, .groups = "keep") %>%
                      mutate(WTP = this_WTP)
                    
                    if (nrow(rows[rows$probability>1,])){stop('what is going on?')}
                    
                    CEAC_dataframe = rbind(CEAC_dataframe,rows)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

CEAC_dataframe <- CEAC_dataframe %>%
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



### Calculate probability WTP at different cost-effectiveness thresholds
CEAC_dataframe = data.frame()

for (this_outcome in unique(CommandDeck_result_long$outcome)){
  workshop = CommandDeck_result_long %>%
    filter(outcome == this_outcome &
             evaluation_level == "incremental" &
             cost_per_outcome_averted != -Inf)
  
  min_seq = round(min(workshop$cost_per_outcome_averted))
  max_seq = round(max(workshop$cost_per_outcome_averted[workshop$count_outcomes != 0] ))
  interval_seq = (max_seq-min_seq)/1000
  
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


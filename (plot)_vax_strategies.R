
severe_outcome_table = data.frame() 
vax_strategy_name = 'no intervention'

###LOOP
severe_outcome_proj(incidence_log_unedited)

###END LOOP

write.csv(severe_outcome_table, "x_results/severe_outcome_table.csv", row.names = F)  

#This program runs results for section 2 of FleetMedic (FM)
#It estimates of the providing booster doses to the at risk group (general public still recieve full scheudle only)

### (1) Overarching trackers #####################################################################################################
warehouse_table = data.frame() 
warehouse_plot = data.frame()
baseline_to_compare = "no booster"

### (2) Queue strategies to run ##################################################################################################
queue = list()

#(A/F) Baseline - no booster
this_run_vax_strategy = vax_strategy_toggles_CURRENT_TARGET

this_run_risk_strategy = list(
  vax_risk_strategy = 'Y',           
  vax_risk_proportion = default_prioritisation_proportion,  
  vax_doses_general = 1,              
  vax_doses_risk = 1,
  risk_group_acceptability = vax_strategy_toggles_CURRENT_TARGET$vax_strategy_max_expected_cov
)

queue[[1]] = list(vax_strategy_description = "no booster",
                  vax_strategy_toggles = this_run_vax_strategy,
                  apply_risk_strategy_toggles = this_run_risk_strategy)


#(B/F) Provision of booster to 'at risk'
this_run_risk_strategy$vax_doses_risk = 2
this_run_vax_strategy$vax_strategy_vaccine_interval = 365/4
queue[[2]] = list(vax_strategy_description = 'booster at three months (at risk only)',
                  vax_strategy_toggles = this_run_vax_strategy,
                  apply_risk_strategy_toggles = this_run_risk_strategy)  #roll out vaccine DURING outbreak

this_run_vax_strategy$vax_strategy_vaccine_interval = 365/2
queue[[3]] = list(vax_strategy_description = 'booster at six months (at risk only)',
                  vax_strategy_toggles = this_run_vax_strategy,
                  apply_risk_strategy_toggles = this_run_risk_strategy)  #roll out vaccine DURING outbreak


#(C/F) Provision of booster to general population
this_run_risk_strategy$vax_doses_general = 2
this_run_vax_strategy$vax_strategy_vaccine_interval = 365/4
queue[[4]] = list(vax_strategy_description = 'booster at three months (all)',
                  vax_strategy_toggles = this_run_vax_strategy,
                  apply_risk_strategy_toggles = this_run_risk_strategy)  #roll out vaccine DURING outbreak

this_run_vax_strategy$vax_strategy_vaccine_interval = 365/2
queue[[5]] = list(vax_strategy_description = 'booster at six months (all)',
                  vax_strategy_toggles = this_run_vax_strategy,
                  apply_risk_strategy_toggles = this_run_risk_strategy)  #roll out vaccine DURING outbreak
this_run_risk_strategy$vax_doses_general = 1
this_run_vax_strategy$vax_strategy_vaccine_interval = 365/4


#(D/F) Difference in risk group acceptability
this_run_risk_strategy$risk_group_acceptability = 0.93
queue[[6]] = list(vax_strategy_description = 'decreased vaccine hesistance to 7%',
                  vax_strategy_toggles = this_run_vax_strategy,
                  apply_risk_strategy_toggles = this_run_risk_strategy)  #roll out vaccine DURING outbreak

this_run_risk_strategy$risk_group_acceptability = 0.98
queue[[7]] = list(vax_strategy_description = 'decreased vaccine hesistance to 2%',
                  vax_strategy_toggles = this_run_vax_strategy,
                  apply_risk_strategy_toggles = this_run_risk_strategy)  #roll out vaccine DURING outbreak

this_run_risk_strategy$risk_group_acceptability = this_run_vax_strategy$vax_strategy_max_expected_cov


#(E/F) Prioritise risk group further
this_run_risk_strategy$vax_risk_proportion = 1
queue[[8]] = list(vax_strategy_description = 'make 100% priority',
                  vax_strategy_toggles = this_run_vax_strategy,
                  apply_risk_strategy_toggles = this_run_risk_strategy)  #roll out vaccine DURING outbreak

this_run_risk_strategy$vax_risk_proportion = 0.75
queue[[9]] = list(vax_strategy_description = 'make 75% priority',
                  vax_strategy_toggles = this_run_vax_strategy,
                  apply_risk_strategy_toggles = this_run_risk_strategy)  #roll out vaccine DURING outbreak

this_run_risk_strategy$vax_risk_proportion = default_prioritisation_proportion


#(F/F) Try a different vaccine
#this_run_risk_strategy$vax_doses_general = 2              
#this_run_risk_strategy$vax_doses_risk = 3    
#COMEBACK 


### (3) Run  ##################################################################################################
for (ticket in 1:length(queue)){

  commands = queue[[ticket]]
  
  vax_strategy_description = commands$vax_strategy_description
  vax_strategy_toggles = commands$vax_strategy_toggles 
  apply_risk_strategy_toggles = commands$apply_risk_strategy_toggles

  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  severe_outcome_projections = severe_outcome_log %>% 
    mutate(label = vax_strategy_description, day = as.numeric(date - date_start ))
  warehouse_plot = rbind(warehouse_plot,severe_outcome_projections)
  
  row = row %>% mutate(scenario = vax_strategy_description,
                       date_complete_at_risk_group = date_complete_at_risk_group) %>% 
    relocate(scenario, .before = colnames(row)[[1]])
  warehouse_table = rbind(warehouse_table,row)
}
#____________________________________________________________________________________________________________________________________

### (4) Save outputs  ##################################################################################################
results_warehouse_entry = list()
results_warehouse_entry[[1]] = warehouse_table
results_warehouse_entry[[2]] = warehouse_plot

#(A/B) absolute outcome plot
warehouse_plot = warehouse_plot %>% mutate(time = day)

plot_list = list()
for (i in 1:length(unique(warehouse_plot$outcome))){
  outcome = unique(warehouse_plot$outcome)[i]
  plot_list [[i]] <- ggplot(data=warehouse_plot[warehouse_plot$outcome==outcome,]) + 
    geom_point(aes(x=time,y=proj,color=as.factor(label))) +
    labs(title=paste(outcome)) +
    theme_bw() + 
    xlab("") + 
    ylab("")}
# 1 = death, 2 = hosp, 3 = severe_disease, 4 = YLL, 5 = cases
plot = ggarrange(plot_list[[5]], plot_list[[3]], plot_list[[1]], 
                 common.legend = TRUE,
                 nrow = 1,
                 legend="bottom")
annotate_figure(plot, top = text_grob('absolute outcome by scenario', face = 'bold', size = 16))

results_warehouse_entry[[3]]= plot

#(B/B) cumulative outcome table
averted_table = warehouse_table[warehouse_table$scenario != baseline_to_compare,]
averted_table_rel = averted_table
for (i in 1:(length(queue)-1)){
  end = (length(unique(warehouse_plot$outcome))+1)
  averted_table[i,c(2:end)] = 
    warehouse_table[warehouse_table$scenario == baseline_to_compare,c(2:end)] -
    averted_table[i,c(2:end)] 
  
  averted_table_rel[i,c(2:end)] = 100 * averted_table[i,c(2:end)]/
    warehouse_table[warehouse_table$scenario == baseline_to_compare,c(2:end)]
}

table_list = list(absolute = averted_table, 
                  relative = averted_table_rel) #COMEBACK could be merged

results_warehouse_entry[[4]]= table_list

#____________________________________________________________________________________________________________________________________

results_warehouse_entry[[5]] = risk_group_name
results_warehouse_FM[[subreceipt]] = results_warehouse_entry


averted_table = averted_table %>% mutate(label = "abs")
averted_table_rel = averted_table_rel %>% mutate(label = "rel")
table_to_print = rbind(averted_table,averted_table_rel)

file_name = paste("FM run 2 ",risk_group_name, Sys.time(),".csv",sep="")
file_name = gsub(' ','_',file_name)
file_name = gsub(':','-',file_name)

write_csv(table_to_print, file = paste(rootpath,"x_results/",file_name,sep=""))

file_name = gsub('.csv','.Rdata',file_name)
save(results_warehouse_entry, file = paste(rootpath,"x_results/",file_name,sep=""))


#This program runs results for section 1 of SpecialityMedic (SM)
#It estimates of the impact prioritising the delivery of primary doses to the at risk group

### (1) Overarching trackers #####################################################################################################
warehouse_table = data.frame() 
warehouse_plot = data.frame()
baseline_to_compare = "RR = 1"

apply_risk_strategy_toggles = 
  list(
    vax_risk_strategy = 'Y',             # options: 'Y','N'
    vax_risk_proportion = default_prioritisation_proportion,           # value between 0-1 (equivalent to %) of doses prioritised to the at risk group
    vax_doses_general = 1,               # number of doses delivered to general pop
    vax_doses_risk = 1                  # number of doses delivered to risk group
    )

### (2) Queue strategies to run ##################################################################################################
queue = list()

queue[[1]] = list(vax_strategy_description = "RR = 1",
                  RR_switch = 1)

queue[[2]] = list(vax_strategy_description = "RR = 1.5",
                  RR_switch = 1.5)

queue[[3]] = list(vax_strategy_description = "RR = 2 (default)",
                  RR_switch = 2)

queue[[4]] = list(vax_strategy_description = "RR = 3",
                  RR_switch = 3)


### (3) Run  ##################################################################################################
for (ticket in 1:length(queue)){
  
  commands = queue[[ticket]]
  
  vax_strategy_description = commands$vax_strategy_description
  RR_estimate = commands$RR_switch
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  severe_outcome_projections = severe_outcome_log %>% 
    mutate(label = vax_strategy_description, day = as.numeric(date - date_start ))
  warehouse_plot = rbind(warehouse_plot,severe_outcome_projections)
  
  row = row %>% mutate(scenario = vax_strategy_description,
                       RR_estimate = RR_estimate) %>% 
    relocate(scenario, .before = colnames(row)[[1]])
  warehouse_table = rbind(warehouse_table,row)
}
RR_estimate = RR_default
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
table_list

results_warehouse_entry[[4]]= table_list

#____________________________________________________________________________________________________________________________________

results_warehouse_SM[[receipt]] = results_warehouse_entry


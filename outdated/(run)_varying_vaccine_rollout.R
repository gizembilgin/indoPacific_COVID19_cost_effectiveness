
#This program runs results for section 3 of FleetAdmiral
#It estimates of the impact of different rollout speeds DURING an outbreak

outbreak_post_rollout = "off"  #i.e. rolling out vaccine during outbreak


### (1) Overarching trackers #####################################################################################################
warehouse_table = data.frame() 
warehouse_plot = data.frame()

### (2) Queue strategies to run ##################################################################################################
queue = list()
vax_strategy_toggle = "on"

# (A/C) Baseline _________________________________________
queue[[1]] = list(vax_strategy_description = 'baseline - current roll-out',
                  vax_strategy_toggle = "on",
                  vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET)

# (B/C)  200% current roll-out speed _______________________________________________
queue[[2]] = queue[[1]]
queue[[2]]$vax_strategy_description = '200% rollout speed'
queue[[2]]$vax_strategy_toggles$vax_strategy_roll_out_speed = queue[[1]]$vax_strategy_toggles$vax_strategy_roll_out_speed * 2

queue[[3]] = queue[[1]]
queue[[3]]$vax_strategy_description = '500% rollout speed'
queue[[3]]$vax_strategy_toggles$vax_strategy_roll_out_speed = queue[[1]]$vax_strategy_toggles$vax_strategy_roll_out_speed * 5



### (3) Run  ##################################################################################################
for (ticket in 1:length(queue)){
  
  commands = queue[[ticket]]
  
  vax_strategy_description = commands$vax_strategy_description
  if ('vax_strategy_toggles' %in% names(commands)){
    vax_strategy_toggles = commands$vax_strategy_toggles
  }
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  severe_outcome_projections = severe_outcome_log %>% 
    mutate(label = vax_strategy_description, day = as.numeric(date - date_start ))
  row = row %>% mutate(scenario = vax_strategy_description)
  warehouse_table = rbind(warehouse_table,row)
  warehouse_plot = rbind(warehouse_plot,severe_outcome_projections)
}
#____________________________________________________________________________________________________________________________________

### (4) Save outputs  ##################################################################################################
results_warehouse_entry = list()
results_warehouse_entry[[1]] = warehouse_table
results_warehouse_entry[[2]] = warehouse_plot

#(A/B) absolute outcome plot
warehouse_plot = warehouse_plot %>% mutate(time = date)

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
plot = ggarrange(plot_list[[5]], plot_list[[1]], 
                 common.legend = TRUE,
                 legend="bottom")
annotate_figure(plot, top = text_grob('absolute outcome by scenario', face = 'bold', size = 16))

results_warehouse_entry[[3]]= plot

#(B/B) cumulative outcome table
averted_table = warehouse_table[warehouse_table$scenario != 'baseline - current roll-out',]
averted_table_rel = averted_table
for (i in 1:(length(queue)-1)){
  averted_table[i,c(1:length(unique(warehouse_plot$outcome)))] = 
    warehouse_table[warehouse_table$scenario == 'baseline - current roll-out',c(1:length(unique(warehouse_plot$outcome)))] -
    averted_table[i,c(1:length(unique(warehouse_plot$outcome)))] 
  
  averted_table_rel[i,c(1:length(unique(warehouse_plot$outcome)))] = 100 * averted_table[i,c(1:length(unique(warehouse_plot$outcome)))]/
    warehouse_table[warehouse_table$scenario == 'baseline - current roll-out',c(1:length(unique(warehouse_plot$outcome)))]
}
table_list = list(absolute = averted_table, 
                  relative = averted_table_rel) #COMEBACK could be merged
#table_list

results_warehouse_entry[[4]]= table_list

#____________________________________________________________________________________________________________________________________

results_warehouse[[3]] = results_warehouse_entry



#This program runs results for section 1 of FleetAdmiral
#It estimates of the impact of current program targets (1) during an outbreak, and (2) if rolled out prior to an outbreak

### (1) Overarching trackers #####################################################################################################
warehouse_table = data.frame() 
warehouse_plot = data.frame()
baseline_to_compare = "no further vaccine rollout"


### (2) Queue strategies to run ##################################################################################################
queue = list()

#(A/B) No further roll-out
queue[[1]] = list(vax_strategy_description = "no further vaccine rollout",
                  vax_strategy_plot = "off",
                  outbreak_post_rollout = "off")


#(B/B) Current program targets
queue[[2]] = list(vax_strategy_description = 'current roll-out DURING outbreak',
                  vax_strategy_plot = "on",
                  outbreak_post_rollout = "off",
                  vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET)  #roll out vaccine DURING outbreak

queue[[3]] = list(vax_strategy_description = 'current roll-out PRIOR to outbreak',
                  vax_strategy_plot = "on",
                  outbreak_post_rollout = "on",
                  vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET)  #roll out vaccine PRIOR TO outbreak




### (3) Run  ##################################################################################################
for (ticket in 1:length(queue)){
  
  commands = queue[[ticket]]
  
  vax_strategy_description = commands$vax_strategy_description
  vax_strategy_plot = commands$vax_strategy_plot
  outbreak_post_rollout = commands$outbreak_post_rollout
  if ('vax_strategy_toggles' %in% names(commands)){
    vax_strategy_toggles = commands$vax_strategy_toggles
  }
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  source(paste(getwd(),"/(function)_severe_outcome_proj.R",sep=""))
  severe_outcome_projections = severe_outcome_projections %>% 
    mutate(label = vax_strategy_description, day = as.numeric(date - date_start ))
  warehouse_plot = rbind(warehouse_plot,severe_outcome_projections)
  
  row = row %>% mutate(scenario = vax_strategy_description) %>% relocate(scenario, .before = colnames(row)[[1]])
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
plot = ggarrange(plot_list[[5]], plot_list[[1]], 
                 common.legend = TRUE,
                 legend="bottom")
annotate_figure(plot, top = text_grob('absolute outcome by scenario', face = 'bold', size = 16))

results_warehouse_entry[[3]]= plot

#(B/B) cumulative outcome table
averted_table = warehouse_table[warehouse_table$scenario != baseline_to_compare,]
averted_table_rel = averted_table
for (i in 1:(length(queue)-1)){
  averted_table[i,c(1:length(unique(warehouse_plot$outcome)))] = 
    warehouse_table[warehouse_table$scenario == baseline_to_compare,c(1:length(unique(warehouse_plot$outcome)))] -
    averted_table[i,c(1:length(unique(warehouse_plot$outcome)))] 
  
  averted_table_rel[i,c(1:length(unique(warehouse_plot$outcome)))] = 100 * averted_table[i,c(1:length(unique(warehouse_plot$outcome)))]/
    warehouse_table[warehouse_table$scenario == baseline_to_compare,c(1:length(unique(warehouse_plot$outcome)))]
}
table_list = list(absolute = averted_table, 
                  relative = averted_table_rel) #COMEBACK could be merged
table_list

results_warehouse_entry[[4]]= table_list

#____________________________________________________________________________________________________________________________________

results_warehouse[[1]] = results_warehouse_entry


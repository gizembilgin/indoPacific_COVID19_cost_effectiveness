#This program runs results for section 2 of FleetAdmiral
#It estimates of the impact of the current program strategy under varying levels of coverage:
#(1) using current age-specific eligibility and (2) expanding eligibility to children

### (1) Overarching trackers #####################################################################################################
warehouse_table = data.frame() 
warehouse_plot = data.frame()
baseline_to_compare = "no further vaccine rollout"
vax_strategy_toggle = "on"

### (2) Planning #################################################################################################################
#calculating poss level of coverage
current_level = sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose == 1]) #0.21
prop_18_plus = sum(pop[3:num_age_groups])/sum(pop) #0.54 for SLE
prop_5_plus = sum(pop[2:num_age_groups])/sum(pop)  #0.86 for SLE

#back of the envelope planning:
#0.21-0.86 for SLE
#Hence, 40,50 without kids
# 50 without kids -> 60,70,80 with kids
# 40,50,60,70,80 with kids



### (3) Queue strategies to run ##################################################################################################
queue = list()

#Section (1/3) - current plan versus if can't reach quite as high (40%)
#compare 40% to current plan
queue[[1]] = list(vax_strategy_description = 'current vaccination targets (51.6%)',
                  vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET) 

target = 0.4
workshop_doses = target - sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose == 1])/100
workshop_doses = round(workshop_doses * sum(pop))
this_vax_strategy = vax_strategy_toggles_CURRENT_TARGET
this_vax_strategy$vax_strategy_num_doses = as.integer(workshop_doses)

queue[[2]] = list(vax_strategy_description = 'lower than current vaccination targets (40.0%)',
                  vax_strategy_toggles = this_vax_strategy) 



#Section (2/3) - current plan (~50%) -> then kids 60,70,80%
#compare kids to current plan queue[[1]]
target_list = list(0.6,0.7,0.8)

for (i in 1:length(target_list)){
  target = target_list[[i]]
  target_percentage = target * 100
  workshop_doses = target - sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose == 1])/100
  workshop_doses = round(workshop_doses * sum(pop))
  this_vax_strategy = vax_strategy_toggles_CURRENT_TARGET
  this_vax_strategy$vax_strategy_num_doses = as.integer(workshop_doses)
  this_vax_strategy$vax_age_strategy = "adults_then_children"
  
  queue[[(2+i)]] = list(vax_strategy_description = paste('current then expand to children ',target_percentage,'%',sep=''),
                        vax_strategy_toggles = this_vax_strategy) 
}



#Section (3/3) - expand now to include kids 40,current, 60, 70, 80
#compare to current plan without expansion to kids queue[[1]]
target_list = list(0.4,0.516,0.6,0.7,0.8)

for (i in 1:length(target_list)){
  target = target_list[[i]]
  target_percentage = target * 100
  workshop_doses = target - sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose == 1])/100
  workshop_doses = round(workshop_doses * sum(pop))
  this_vax_strategy = vax_strategy_toggles_CURRENT_TARGET
  this_vax_strategy$vax_strategy_num_doses = as.integer(workshop_doses)
  this_vax_strategy$vax_age_strategy = "uniform"
  
  queue[[(5+i)]] = list(vax_strategy_description = paste('expand to children now ',target_percentage,'%',sep=''),
                    vax_strategy_toggles = this_vax_strategy) 
}




### (4) Run  ##################################################################################################
for (ticket in 1:length(queue)){
  
  commands = queue[[ticket]]
  
  vax_strategy_description = commands$vax_strategy_description
  vax_strategy_toggles = commands$vax_strategy_toggles
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  severe_outcome_projections = severe_outcome_log %>% 
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

scenario_table_list = list()
scenario_plot_list = list()

#(A/B) absolute outcome plot
if (outbreak_post_rollout == "on"){
  warehouse_plot = warehouse_plot %>% mutate(time = day)
} else if (outbreak_post_rollout == "off"){
  warehouse_plot = warehouse_plot %>% mutate(time = date)
}

section_1 = c('current vaccination targets (51.6%)','lower than current vaccination targets (40.0%)')
section_2 = c("current vaccination targets (51.6%)", "current then expand to children 60%", "current then expand to children 70%", "current then expand to children 80%")
section_3 = c("current vaccination targets (51.6%)","expand to children now 40%","expand to children now 51.6%","expand to children now 60%","expand to children now 70%","expand to children now 80%")
section_list = list(section_1,section_2,section_3)

for (section in 1:3){
  list_plot_commands = section_list[[section]]
  workshop = warehouse_plot[warehouse_plot$label %in% list_plot_commands, ]
  
  plot_list = list()
  for (i in 1:length(unique(workshop$outcome))){
    outcome = unique(workshop$outcome)[i]
    plot_list [[i]] <- ggplot(data=workshop[workshop$outcome==outcome,]) + 
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
  scenario_plot_list[[section]] = plot
}

results_warehouse_entry[[3]]= scenario_plot_list

#(B/B) cumulative outcome table
baseline_to_compare = "current vaccination targets (51.6%)"

 
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
    
  
for (section in 1:3){
  list_plot_commands = section_list[[section]]  
  this_averted_table = averted_table[averted_table$scenario %in% list_plot_commands,]
  this_relative_table = averted_table_rel[averted_table_rel$scenario %in% list_plot_commands,]
  
  table_list = list(absolute = this_averted_table, 
                    relative = this_relative_table)
    
  scenario_table_list[[section]] = table_list
}

results_warehouse_entry[[4]]= scenario_table_list

#____________________________________________________________________________________________________________________________________

results_warehouse[[receipt]] = results_warehouse_entry


#COMEBACK - how can current vaccine target be better than increased coverage??

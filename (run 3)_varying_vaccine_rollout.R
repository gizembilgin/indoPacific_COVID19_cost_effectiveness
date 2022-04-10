
#This program runs results for section 3 of FleetAdmiral

outbreak_post_rollout = "off"  #i.e. rolling out vaccine during outbreak


### (1) Overarching trackers #####################################################################################################
warehouse_table = data.frame() 
warehouse_plot = data.frame()

### (2) Queue strategies to run ##################################################################################################
queue = list()
vax_strategy_plot = "on"

# (A/C) Baseline _________________________________________
#18+ open
if (setting == "SLE"){
  target = 0.516
  workshop_doses = target - sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose == 1])/100
  workshop_doses = round(workshop_doses * sum(pop))
  
  vax_strategy_toggles =
    list(vax_strategy_start_date                  = as.Date('2022-04-20'),
         vax_strategy_num_doses         = as.integer(workshop_doses),
         vax_strategy_roll_out_speed    = 11075 ,               # doses delivered per day
         vax_age_strategy               = "uniform_no_children",            # options: "oldest", "youngest","50_down","uniform", OTHER?
         vax_dose_strategy              = 1,                    # options: 1,2
         vax_strategy_vaccine_type      = "Johnson & Johnson" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
         vax_strategy_vaccine_interval  = 7*3 ,                 # (days) interval between first and second dose
         vax_strategy_max_expected_cov  = 0.88                   # value between 0-1 of age group willing to be vaccinated (vaccine hesitancy est in discussion)
    )
} else if (setting == "PNG"){
  target = 0.199
  workshop_doses = target - sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose == 1])/100
  workshop_doses = round(workshop_doses * sum(pop))
  workshop_doses = workshop_doses * 2 
  
  vax_strategy_toggles =
    list(vax_strategy_start_date                  = as.Date('2022-04-20'),
         vax_strategy_num_doses         = as.integer(workshop_doses), 
         vax_strategy_roll_out_speed    = 12000 ,               # doses delivered per day
         vax_age_strategy               = "uniform_no_children",            # options: "oldest", "youngest","50_down","uniform", OTHER?
         vax_dose_strategy              = 2,                    # options: 1,2
         vax_strategy_vaccine_type      = "Pfizer" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
         vax_strategy_vaccine_interval  = 7*3 ,                 # (days) interval between first and second dose
         vax_strategy_max_expected_cov  = 0.74                   # value between 0-1 of age group willing to be vaccinated (vaccine hesitancy est in discussion)
    )
} else { stop ('pick a valid setting!')}

queue[[1]] = list(vax_strategy_description = 'baseline - current roll-out',
                  vax_strategy_plot = "on",
                  vax_strategy_toggles = vax_strategy_toggles)

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
  source(paste(getwd(),"/(function)_severe_outcome_proj.R",sep=""))
  severe_outcome_projections = severe_outcome_projections %>% 
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


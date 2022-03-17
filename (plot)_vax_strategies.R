
### (1) Overarching toggles #######################################################################################################
severe_outcome_table = data.frame() 
severe_outcome_tracker = data.frame()

### (2) Queue strategies to run ##################################################################################################
queue = list()
#COMEBACK - after the baseline and no intervention, could the remaining be in a csv?

# queue[[1]]
queue[[1]] = list(vax_strategy_description = "no further vaccine rollout",
                  vax_strategy_plot = "off")


# queue[[2]]
if (setting == "SLE"){
  vax_strategy_toggles =
    list(vax_strategy_start_date                  = as.Date('2022-04-20'),
         vax_strategy_num_doses         = as.integer(1000000),
         vax_strategy_roll_out_speed    = 50000 ,               # doses delivered per day
         vax_age_strategy               = "oldest",            # options: "oldest", "youngest","50_down","uniform", OTHER?
         vax_dose_strategy              = 1,                    # options: 1,2
         vax_strategy_vaccine_type      = "Johnson & Johnson" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
         vax_strategy_vaccine_interval  = 7*3 ,                 # (days) interval between first and second dose
         vax_strategy_max_expected_cov  = 0.8                   # value between 0-1 (equivalent to %) of age group willing to be vaccinated
    )
} else if (setting == "PNG"){
  vax_strategy_toggles =
    list(vax_strategy_start_date                  = as.Date('2022-04-20'),
         vax_strategy_num_doses         = as.integer(1000000),
         vax_strategy_roll_out_speed    = 50000 ,               # doses delivered per day
         vax_age_strategy               = "oldest",            # options: "oldest", "youngest","50_down","uniform", OTHER?
         vax_dose_strategy              = 2,                    # options: 1,2
         vax_strategy_vaccine_type      = "Pfizer" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
         vax_strategy_vaccine_interval  = 7*3 ,                 # (days) interval between first and second dose
         vax_strategy_max_expected_cov  = 0.8                   # value between 0-1 (equivalent to %) of age group willing to be vaccinated
    )
} else { stop ('pick a valid setting!')}

queue[[2]] = list(vax_strategy_description = 'baseline - current country strategy',
                  vax_strategy_plot = "on",
                  vax_strategy_toggles = vax_strategy_toggles)

# queue[[3]]
vax_strategy_toggles =
  list(vax_strategy_start_date                  = as.Date('2022-04-20'),
       vax_strategy_num_doses         = as.integer(1000000),
       vax_strategy_roll_out_speed    = 500000 ,               # doses delivered per day
       vax_age_strategy               = "oldest",            # options: "oldest", "youngest","50_down","uniform", OTHER?
       vax_dose_strategy              = 1,                    # options: 1,2
       vax_strategy_vaccine_type      = "Johnson & Johnson" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
       vax_strategy_vaccine_interval  = 7*3 ,                 # (days) interval between first and second dose
       vax_strategy_max_expected_cov  = 0.8                   # value between 0-1 (equivalent to %) of age group willing to be vaccinated
  )
queue[[3]] = list(vax_strategy_description = 'attempt 1',
                  vax_strategy_toggles = vax_strategy_toggles)

# # queue[[4]]
vax_strategy_toggles =
  list(vax_strategy_start_date                  = as.Date('2022-04-20'),
       vax_strategy_num_doses         = as.integer(1000000),
       vax_strategy_roll_out_speed    = 500000 ,               # doses delivered per day
       vax_age_strategy               = "oldest",            # options: "oldest", "youngest","50_down","uniform", OTHER?
       vax_dose_strategy              = 2,                    # options: 1,2
       vax_strategy_vaccine_type      = "Pfizer" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
       vax_strategy_vaccine_interval  = 7*3 ,                 # (days) interval between first and second dose
       vax_strategy_max_expected_cov  = 0.8                   # value between 0-1 (equivalent to %) of age group willing to be vaccinated
  )
queue[[4]] = list(vax_strategy_description = 'attempt 2',
                  vax_strategy_toggles = vax_strategy_toggles)

# 
# # queue[[5]]
# vax_strategy_description = 'baseline - current country strategy'
# vax_strategy_toggles =
#   list(vax_strategy_start_date                  = as.Date('2022-04-20'),
#        vax_strategy_num_doses         = as.integer(1000000),
#        vax_strategy_roll_out_speed    = 50000 ,               # doses delivered per day
#        vax_age_strategy               = "oldest",            # options: "oldest", "youngest","50_down","uniform", OTHER?
#        vax_dose_strategy              = 1,                    # options: 1,2
#        vax_strategy_vaccine_type      = "Johnson & Johnson" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
#        vax_strategy_vaccine_interval  = 7*3 ,                 # (days) interval between first and second dose
#        vax_strategy_max_expected_cov  = 0.8                   # value between 0-1 (equivalent to %) of age group willing to be vaccinated
#   )
# 
# queue[[5]] = list(vax_strategy_description,vax_strategy_toggles)
#____________________________________________________________________________________________________________________________________


### (3) Run
for (i in 1:length(queue)){
  commands = queue[[i]]

  vax_strategy_description = commands$vax_strategy_description
  if ('vax_strategy_plot' %in% names(commands)){
    vax_strategy_plot = commands$vax_strategy_plot
  }
  if ('vax_strategy_toggles' %in% names(commands)){
    vax_strategy_toggles = commands$vax_strategy_toggles
  }
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  source(paste(getwd(),"/(function)_severe_outcome_proj.R",sep=""))
  severe_outcome_projections = severe_outcome_projections %>% mutate(label = vax_strategy_description)
  severe_outcome_table = rbind(severe_outcome_table,row)
  severe_outcome_tracker = rbind(severe_outcome_tracker,severe_outcome_projections)
}

severe_outcome_table
#write.csv(severe_outcome_table, "x_results/severe_outcome_table.csv", row.names = F)  #easy copy pasting


plot_list = list()
for (i in 1:length(unique(severe_outcome_tracker$outcome))){
  outcome = unique(severe_outcome_tracker$outcome)[i]
  plot_list [[i]] <- ggplot(data=severe_outcome_tracker[severe_outcome_tracker$outcome==outcome,]) + 
    geom_point(aes(x=date,y=proj_cum,color=as.factor(label))) +
    labs(title=paste("projected ",outcome," by vax strategy",sep=""))
}
gridExtra::grid.arrange(grobs=plot_list)

require(ggpubr)
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],
          common.legend = TRUE,
          legend="bottom"
          )




#NOTE: need to prime, set up pop for setting
time.start=proc.time()[[3]] #let's see how long this runs for

### (1) Overarching toggles #######################################################################################################
severe_outcome_table = data.frame() 
severe_outcome_tracker = data.frame()

### (2) Queue strategies to run ##################################################################################################
queue = list()
#COMEBACK - after the baseline and no intervention, could the remaining be in a csv?

# (A/?) No further vaccine roll-out (queue[[1]]) ___________________________________
queue[[1]] = list(vax_strategy_description = "no further vaccine rollout",
                  vax_strategy_toggle = "off")


# (B/?) 'Baseline' roll-out (queue[[2-3]]) _________________________________________
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

queue[[2]] = list(vax_strategy_description = 'baseline - current roll-out',
                  vax_strategy_toggle = "on",
                  vax_strategy_toggles = vax_strategy_toggles)



# (C/?) Strategy (queue[[4-5]]) _______________________________________________
# queue[[3]]
queue[[3]] = queue[[2]]
queue[[3]]$vax_strategy_description = 'vaccinate the transmitters'
queue[[3]]$vax_strategy_toggles$vax_age_strategy = "youngest"    

queue[[4]] = queue[[2]]
queue[[4]]$vax_strategy_description = 'vaccinate the most vulnerable'
queue[[4]]$vax_strategy_toggles$vax_age_strategy = "oldest" 



# (D/?) Increase coverage _______________________________________________________
queue[[5]] = queue[[2]]
queue[[5]]$vax_strategy_description = '40% coverage'
queue[[5]]$vax_strategy_toggles$vax_strategy_num_doses = queue[[2]]$vax_strategy_toggles$vax_strategy_num_doses * 2

queue[[6]] = queue[[2]]
queue[[6]]$vax_strategy_description = '80% coverage'
queue[[6]]$vax_strategy_toggles$vax_strategy_num_doses = queue[[2]]$vax_strategy_toggles$vax_strategy_num_doses * 4


####ELSE 
# 
# # queue[[6]]
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
# queue[[6]] = list(vax_strategy_description,vax_strategy_toggles)
#____________________________________________________________________________________________________________________________________


### (3) Run  ##################################################################################################
for (ticket in 1:length(queue)){
  
  commands = queue[[ticket]]
  
  vax_strategy_description = commands$vax_strategy_description
  if ('vax_strategy_toggle' %in% names(commands)){
    vax_strategy_toggle = commands$vax_strategy_toggle
  }
  if ('vax_strategy_toggles' %in% names(commands)){
    vax_strategy_toggles = commands$vax_strategy_toggles
  }
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  source(paste(getwd(),"/(function)_severe_outcome_proj.R",sep=""))
  severe_outcome_projections = severe_outcome_projections %>% mutate(label = vax_strategy_description)
  row = row %>% mutate(scenario = vax_strategy_description)
  severe_outcome_table = rbind(severe_outcome_table,row)
  severe_outcome_tracker = rbind(severe_outcome_tracker,severe_outcome_projections)
}
#____________________________________________________________________________________________________________________________________

time.end=proc.time()[[3]]
time.end-time.start 


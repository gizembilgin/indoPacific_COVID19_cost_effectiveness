
severe_outcome_table = data.frame() 
vax_strategy_name = 'no intervention'


###LOOP
#COMEBACK, is this really a function?
source(paste(getwd(),"/(function)_severe_outcome_proj.R",sep=""))
severe_outcome_proj(incidence_log_unedited)

###END LOOP

write.csv(severe_outcome_table, "x_results/severe_outcome_table.csv", row.names = F)  


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


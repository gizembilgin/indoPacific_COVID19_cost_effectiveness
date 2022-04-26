### This program is intended runs the model in multiple ways to achieve the data required for the RMarkdown report
### Currently four main sections:
# (1) Impact of current program targets
# (2) Varying levels of coverage - w and w/out children
# (3) Varying speed of vaccine roll-out
# (4) Other sensitivity analysis
#________________________________________________________________________________________________________________
time.start.FleetAdmiral=proc.time()[[3]]
results_warehouse = list()

waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = TRUE

if (setting == "SLE"){
  gov_target = 0.516
  workshop_doses = gov_target - sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose == 1])/100
  workshop_doses = round(workshop_doses * sum(pop))
  
  vax_strategy_toggles_CURRENT_TARGET =
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
  gov_target = 0.199
  workshop_doses = gov_target - sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose == 1])/100
  workshop_doses = round(workshop_doses * sum(pop))
  workshop_doses = workshop_doses * 2 
  
  vax_strategy_toggles_CURRENT_TARGET =
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

current_coverage = c(sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose ==1]),
                     sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose ==2])) #COMEBACK - if J&J in use!
if ("Johnson & Johnson" %in% unique(vaccination_history_POP$vaccine_type)){warning('True vaccine coverage MUST consider J&J dose 1')}


###(1/4) Impact of current program targets
receipt = 1
source(paste(getwd(),"/(run 1)_impact_of_current_program_targets.R",sep=""))
#________________________________________________________________________________________________________________


### (2) Varying levels of coverage - w and w/out children
receipt = 2
outbreak_post_rollout = "off" 
#Note: roll-out must be during outbreak, otherwise waning immunity makes vaccinating kids look like a bad idea
source(paste(getwd(),"/(run 2)_varying_coverage.R",sep=""))

receipt = 3
outbreak_post_rollout = "on" 
source(paste(getwd(),"/(run 2)_varying_coverage.R",sep=""))
#________________________________________________________________________________________________________________


### (3) Varying speed of vaccine roll-out
receipt = 4
source(paste(getwd(),"/(run 3)_varying_vaccine_rollout.R",sep=""))
#________________________________________________________________________________________________________________


### (4) Other sensitivity analysis
receipt = 4
#COMEBACK - need to code/decide
#________________________________________________________________________________________________________________


save.image(file = paste("x_results/complete_model_run_",Sys.Date(),".Rdata",sep=''))

time.end.FleetAdmiral=proc.time()[[3]]
time.end.FleetAdmiral-time.start.FleetAdmiral 
# 6798.61 = 1.9 hours


time = Sys.time()
time = gsub(':','-',time)
file_name = paste( "x_results/Vaccine allocation project results",time)
file_name = gsub(' ','_',file_name)

library(rmarkdown); library(tinytex)
render('results_report_compiler_v2.Rmd',output_file = file_name)
#render('results_report_compiler_v2.Rmd',output_file = file_name, output_format = "pdf_document")
#render('results_report_compiler_v2.Rmd',output_file = file_name, output_format = "word_document")

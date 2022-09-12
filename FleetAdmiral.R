### This program is intended runs the model in multiple ways to achieve the data required for the RMarkdown report
### Currently four main sections:
# (1) Impact of current program targets
# (2) Varying levels of coverage - w and w/out children
# (3) Varying speed of vaccine roll-out
# (4) 'At risk' group consideration
#________________________________________________________________________________________________________________
time.start.FleetAdmiral=proc.time()[[3]]
results_warehouse = list()

load(file = '1_inputs/last_fit_date.Rdata')
date_start = fitted_max_date ##latest fit date

strain_inital = strain_now = 'omicron'             #options:'WT','delta','omicron'
model_weeks = 52          # how many weeks should the model run for?

waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = TRUE
waning_toggle_rho_acqusition = TRUE
rho_severe_disease = "on"

risk_group_toggle = "off"
risk_group_lower_cov_ratio = NA
risk_group_prioritisation_to_date = NA
vax_risk_strategy_toggle = "off"
sensitivity_analysis_toggles = list()

setting = "SLE"
if (setting == "SLE"){
  gov_target = 0.516
  #gov_target = 0.7
  workshop_doses = gov_target - sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose == 1])/100
  workshop_doses = round(workshop_doses * sum(pop))
  
  vax_strategy_toggles_CURRENT_TARGET =
    list(vax_strategy_start_date        = date_start,
         vax_strategy_num_doses         = as.integer(workshop_doses),
         vax_strategy_roll_out_speed    = 11075 ,               # doses delivered per day
         vax_delivery_group             = 'universal',
         vax_age_strategy               = "uniform_no_children",            # options: "oldest", "youngest","50_down","uniform", OTHER?
         vax_dose_strategy              = 1,                    # options: 1,2
         vax_strategy_vaccine_type      = "Johnson & Johnson" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
         vax_strategy_vaccine_interval  = c(90) ,                 # (days) interval between doses
         vax_strategy_max_expected_cov  = 0.88                   # value between 0-1 of age group willing to be vaccinated (vaccine hesitancy est in discussion)
    )
} else { stop ('pick a valid setting!')}




###(Table 2) cumulative outcomes for prioritisation strategies including children (one year of endemic Omicron transmission)
receipt = 1
outbreak_timing = "off" 
#Note: roll-out during steady state
#source(paste(getwd(),"/(Table 2)_varying_eligb_age.R",sep=""))

#________________________________________________________________________________________________________________



###(Table 3) At risk group analysis
receipt = 2
risk_group_name = "pregnant_women"
source(paste(getwd(),"/(Table 3) high-risk groups.R",sep=""))

receipt = 3
risk_group_name = "adults_with_comorbidities"
source(paste(getwd(),"/(Table 3) high-risk groups.R",sep="")) 
#________________________________________________________________________________________________________________




### FIGURE 2 (-> supplementary)
risk_group_toggle = "off"
vax_risk_strategy_toggle = "off"
receipt = 4
#source(paste(getwd(),"/(run 1)_impact_of_current_program_targets.R",sep=""))
receipt = 5
#source(paste(getwd(),"/(run 3)_varying_vaccine_rollout.R",sep=""))

#results_warehouse[[4]][[3]]
#results_warehouse[[5]][[3]]
#________________________________________________________________________________________________________________



###(SENSITIVITY ANALYSIS) At risk group analysis
receipt = 6
sensitivity_analysis_toggles$RR_risk_group = list(1,1.5,2.4,3)
risk_group_name = "pregnant_women"
source(paste(getwd(),"/(Table 3) high-risk groups.R",sep=""))

receipt = 7
sensitivity_analysis_toggles$RR_risk_group = list(1,1.5,2,3)
risk_group_name = "adults_with_comorbidities"
source(paste(getwd(),"/(Table 3) high-risk groups.R",sep="")) 
#________________________________________________________________________________________________________________



###(SENSITIVITY ANALYSIS) Reduced VE in older adults and adults with comorbidities
#S4.4 Reduced vaccine effectiveness in older adults - Table S4.4
receipt = 8
outbreak_timing = "off" 
sensitivity_analysis_toggles = list(VE_older_adults = "reduced")
source(paste(getwd(),"/(Table 2)_varying_eligb_age.R",sep=""))

#S4.4 Reduced vaccine effectiveness in older adults - Table S4.5
receipt = 9
risk_group_name = "pregnant_women"
sensitivity_analysis_toggles = list(VE_older_adults = "reduced")
source(paste(getwd(),"/(Table 3) high-risk groups.R",sep=""))

#S4.4 Reduced vaccine effectiveness in older adults - Table S4.5 AND
#S4.5 Reduced vaccine effectiveness in older adults and adults with comorbidities - Table S4.6
receipt = 10
risk_group_name = "adults_with_comorbidities"
sensitivity_analysis_toggles = list(VE_older_adults = "reduced",VE_adults_comorb = 0.9)
source(paste(getwd(),"/(Table 3) high-risk groups.R",sep="")) 

sensitivity_analysis_toggles = list()
#________________________________________________________________________________________________________________



current_coverage = c(sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose ==1]),
                     sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose ==2])) #COMEBACK - if J&J in use!
if ("Johnson & Johnson" %in% unique(vaccination_history_POP$vaccine_type)){warning('True vaccine coverage MUST consider J&J dose 1')}

save.image(file = paste("x_results/complete_model_run_",Sys.Date(),".Rdata",sep=''))

time.end.FleetAdmiral=proc.time()[[3]]
time.end.FleetAdmiral-time.start.FleetAdmiral 
# 6798.61 = 1.9 hours #COMEBACK - time!


time = Sys.time()
time = gsub(':','-',time)
file_name = paste( "x_results/Vaccine allocation project results",time)
file_name = gsub(' ','_',file_name)

#library(rmarkdown); library(tinytex)
#render('FleetAdmiral_compiler.Rmd',output_file = file_name)
#render('FleetAdmiral_compiler.Rmd',output_file = file_name, output_format = "pdf_document")
#render('FleetAdmiral_compiler.Rmd',output_file = file_name, output_format = "word_document")

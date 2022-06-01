### This program is intended runs the model in multiple ways to deliver targetted doses to the at-risk group
### Currently four main sections:
# (1) Prioritising primary dose coverage by x%
# (2) Delivery of booster doses to at risk group
# (3) Sensitivity analysis: (a) RR, (b) VE, (c) contact patterns, (d) vaccine hesitancy
#________________________________________________________________________________________________________________

time.start.SpecialityMedic=proc.time()[[3]]
receipt = 0
results_warehouse_SM = list()

waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = FALSE
waning_toggle_rho_acqusition = TRUE
rho_severe_disease = "on"

outbreak_post_rollout = "off"

RR_estimate = RR_default = 2
vax_strategy_toggle = "on"
vax_risk_strategy_toggle = "on"
risk_group_toggle = "on" 
risk_group_name = "pregnant_women" #options: pregnant_women, adults_with_comorbidities
risk_group_prioritisation_to_date = NA
default_prioritisation_proportion = 0.5

#make sure toggles not in CommandDeck

if (setting == "SLE"){
  gov_target = 0.516
  workshop_doses = gov_target - sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose == 1])/100
  workshop_doses = round(workshop_doses * sum(pop))
  
  vax_strategy_toggles_CURRENT_TARGET =
    list(vax_strategy_start_date        = baseline_date_start,
         vax_strategy_num_doses         = as.integer(workshop_doses),
         vax_strategy_roll_out_speed    = 11075 ,               # doses delivered per day
         vax_delivery_group             = 'universal',
         vax_age_strategy               = "uniform_no_children",            # options: "oldest", "youngest","50_down","uniform", OTHER?
         vax_dose_strategy              = 1,                    # options: 1,2
         vax_strategy_vaccine_type      = "Johnson & Johnson" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
         vax_strategy_vaccine_interval  = 7*3 ,                 # (days) interval between first and second dose
         vax_strategy_max_expected_cov  = 0.88                   # value between 0-1 of age group willing to be vaccinated (vaccine hesitancy est in discussion)
    )
} else { stop ('pick a valid setting!')}
vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET

current_coverage = c(sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose ==1]),
                     sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose ==2])) #COMEBACK - if J&J in use!
if ("Johnson & Johnson" %in% unique(vaccination_history_POP$vaccine_type)){warning('True vaccine coverage MUST consider J&J dose 1')}


###(1) Prioritising primary dose coverage by x%
source(paste(getwd(),"/(SM run 1) prioritising primary doses.R",sep=""))
#________________________________________________________________________________________________________________


### (2) Delivery of booster doses to at risk group
#source(paste(getwd(),"/(SM run 2) priority delivery of booster dose.R",sep=""))
#________________________________________________________________________________________________________________


### (3) Sensitivity analysis: (a) RR, (b) VE, (c) contact patterns, (d) vaccine hesitancy
#(A) RR 1-2.5
receipt = 3

#(B) lower VE
receipt = 4

#(C) lower/altered contact
receipt = 5

#(D) varied vaccine hesitancy (either real data or advocacy targets)
receipt = 6

#________________________________________________________________________________________________________________




save.image(file = paste("x_results/SpecialityMedic_run_",Sys.Date(),".Rdata",sep=''))

time.end.SpecialityMedic=proc.time()[[3]]
time.end.SpecialityMedic-time.start.SpecialityMedic 
# 6798.61 = 1.9 hours


time = Sys.time()
time = gsub(':','-',time)

file_name = paste( "x_results/SpecialityMedic",time)
file_name = gsub(' ','_',file_name)

library(rmarkdown)
render('SpecialityMedic_compiler.Rmd',output_file = file_name)


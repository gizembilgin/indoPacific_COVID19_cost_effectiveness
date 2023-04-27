### This script runs the primary transmission model for the scenarios of the antiviral paper.
### The incidence log and other antiviral model dependencies are then saved.

### DEPENDENCIES: nil!
if (exists("master_toggles")){
  this_risk_group_name = risk_group_name = master_toggles$risk_group_name
  setting_beta = master_toggles$setting_beta
  TOGGLE_include_second_booster_elig = master_toggles$TOGGLE_include_second_booster_elig
} else{
  rm(list=ls())
  this_risk_group_name = risk_group_name = "adults_with_comorbidities" #options: pregnant_women, adults_with_comorbidities
  setting_beta = "TLS" #options: "FJI", "SLE",PNG_high_beta, PNG_low_beta
  TOGGLE_include_second_booster_elig = FALSE
}
setting = this_setting = substr(setting_beta,1,3)

### SET UP MODEL RUN ################################################################
#start timing
time.start.AntiviralSetUp=proc.time()[[3]]

fitting = "off";plotting = "off"

#find latest model run in known dates
# list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("fitted_results_",setting_beta,"*",sep=""))
# list_poss_Rdata_details = double()
# for (i in 1:length(list_poss_Rdata)){
#   list_poss_Rdata_details = rbind(list_poss_Rdata_details,
#                                   file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
# }
# latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
#date_start = as.Date(file.info(paste("1_inputs/fit/",latest_file,sep=''))$mtime) 

###KATIE DISCUSSION
date_start = as.Date('2022-12-31')

#initialise length of model run and circulating strain
strain_inital = strain_now = 'omicron' 
outbreak_timing = "off" #roll-out during steady state
model_weeks = as.numeric((as.Date('2024-01-01') - date_start)/7) #ensure model runs for entire 2023

#turn on waning of all immunity
waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = TRUE
waning_toggle_rho_acqusition = TRUE

#turn off risk groups to start with
antiviral_setup = "on"
risk_group_toggle = "off"
vax_strategy_toggle = "off"
vax_risk_strategy_toggle = "off"
risk_group_lower_cov_ratio = NA
risk_group_prioritisation_to_date = NA
if(this_risk_group_name == "adults_with_comorbidities"){
  sensitivity_analysis_toggles = list(VE_older_adults = "reduced",VE_adults_comorb = 0.9)
} else{
  sensitivity_analysis_toggles = list(VE_older_adults = "reduced")
}
#______________________________________________________________________________________________________________



### SET UP SETTING ################################################################
generic_booster_toggles = 
  list(
    function_name = "booster_strategy_informed_prior",
    start_date = as.Date('2023-03-01'),
    dose_supply = 9999999999,
    #rollout_months = 3, #used fixed % pop rollout per day 07/02/2023
    
    delivery_risk_group = c(risk_group_name,'general_public'),
    prev_dose_floor = 2,
    prev_dose_ceiling = 5,
    age_groups = c("18 to 29",  "30 to 44",  "45 to 59", "60 to 69",  "70 to 100"),
    
    prioritised_risk = "N",
    prioritised_age = NA,
    
    vaccine_type = "Moderna"
  )
vax_strategy_toggles = "off"
vax_risk_strategy_toggle = "off"
apply_risk_strategy_toggles = "off"

if (setting == "FJI"){
  
} else if (setting == "PNG"){
  #Note: current vaccine program proj in (2)
  generic_booster_toggles$prev_dose_floor = 1
  generic_booster_toggles$vaccine_type = "Johnson & Johnson"
  
} else if (setting %in% c("IDN","TLS")){
  generic_booster_toggles$vaccine_type = "Pfizer"
  
}else{
  vax_strategy_toggles_CURRENT_TARGET =
    list(vax_strategy_start_date        = date_start,
         vax_strategy_num_doses         = 99999999, #assume that all adults who are willing have been vaccinated
         vax_strategy_roll_out_speed    = 11075 ,                           # doses delivered per day
         vax_delivery_group             = 'universal',
         vax_age_strategy               = "uniform_no_children",            # options: "oldest", "youngest","50_down","uniform"
         vax_dose_strategy              = 1,                                # options: 1,2
         vax_strategy_vaccine_type      =  ,             # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
         vax_strategy_vaccine_interval  = c(90) ,                           #  (days) interval between doses, you must specify multiple intervals if multiple doses e.g. c(21,90)
         vax_strategy_max_expected_cov  = 0.88                              # value between 0-1 of age group willing to be vaccinated
    )
  primary_only_toggles = 
    list(
      vax_risk_strategy = 'N',
      vax_risk_proportion = 0,
      vax_doses_general = vax_strategy_toggles_CURRENT_TARGET$vax_dose_strategy,
      vax_doses_risk = vax_strategy_toggles_CURRENT_TARGET$vax_dose_strategy,
      risk_group_acceptability = vax_strategy_toggles_CURRENT_TARGET$vax_strategy_max_expected_cov,
      risk_group_accessibility = FALSE,
      risk_group_age_broaden = FALSE
    )
  generic_booster_toggles = 
    list(
      function_name = "booster_strategy",
      start_date = as.Date('2023-01-01'),
      dose_allocation = 9999999999,
      rollout_speed = vax_strategy_toggles_CURRENT_TARGET$vax_strategy_roll_out_speed,
      delivery_risk_group = c('general_public'),
      delivery_includes_previously_boosted = 'N',
      age_strategy = vax_strategy_toggles_CURRENT_TARGET$vax_age_strategy,
      vaccine_type = vax_strategy_toggles_CURRENT_TARGET$vax_strategy_vaccine_type,
      vaccine_interval = 90
    )
}
RECORD_generic_booster_toggles = generic_booster_toggles
#______________________________________________________________________________________________________________
  
  
  
### QUEUE #####################################################################################################
queue = list()

### VACCINATION SCENARIO = PRIMARY ONLY 
# Adults with comorbidities
queue[[1]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule',
                  risk_group_name = this_risk_group_name,
                  risk_group_toggle = "on",
                  vax_risk_strategy_toggle = "off",
                  apply_risk_strategy_toggles = NA,
                  vax_strategy_toggles = NA,
                  booster_toggles = "no")
#______________________________________________________________________________________________________________



### VACCINATION SCENARIO = PRIMARY + BOOSTER HIGH-RISK
#booster_highRisk_toggles = primary_only_toggles
#booster_highRisk_toggles$vax_doses_risk = booster_highRisk_toggles$vax_doses_risk + 1

# Adults with comorbidities
generic_booster_toggles$delivery_risk_group = c(this_risk_group_name)

generic_booster_toggles$prev_dose_floor = 2
queue[[length(queue)+1]] = list(
  vax_strategy_description = 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster',
  vax_strategy_description_long = 'assume booster to all adults who have previously recieved a primary schedule',
  risk_group_name = this_risk_group_name,
  risk_group_toggle = "on",
  booster_toggles = generic_booster_toggles) 

generic_booster_toggles$prev_dose_floor = 3
queue[[length(queue)+1]] = list(
  vax_strategy_description = 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster',
  vax_strategy_description_long = 'assume booster to all adults who have previously recieved a first booster dose',
  risk_group_name = this_risk_group_name,
  risk_group_toggle = "on",
  booster_toggles = generic_booster_toggles) 

if (setting == "FJI" & TOGGLE_include_second_booster_elig == TRUE){
  generic_booster_toggles$prev_dose_floor = 4
  queue[[length(queue)+1]] = list(
    vax_strategy_description = 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster',
    vax_strategy_description_long = 'assume booster to all adults who have previously recieved two booster doses',
    risk_group_name = this_risk_group_name,
    risk_group_toggle = "on",
    booster_toggles = generic_booster_toggles)
}
#______________________________________________________________________________________________________________



### VACCINATION SCENARIO = PRIMARY + BOOSTER ALL
# booster_all_toggles = booster_highRisk_toggles
# booster_all_toggles$vax_doses_general = booster_highRisk_toggles$vax_doses_risk

#Adults with comorbidities
generic_booster_toggles$delivery_risk_group = c('general_public',this_risk_group_name)

generic_booster_toggles$prev_dose_floor = 2
queue[[length(queue)+1]] = list(
  vax_strategy_description = 'all willing adults vaccinated with a primary schedule plus booster dose',
  vax_strategy_description_long = 'assume booster to all adults who have previously recieved a primary schedule',
  risk_group_name = this_risk_group_name,
  risk_group_toggle = "on",
  booster_toggles = generic_booster_toggles) 

generic_booster_toggles$prev_dose_floor = 3
queue[[length(queue)+1]] = list(
  vax_strategy_description = 'all willing adults vaccinated with a primary schedule plus booster dose',
  vax_strategy_description_long = 'assume booster to all adults who have previously recieved a first booster dose',
  risk_group_name = this_risk_group_name,
  risk_group_toggle = "on",
  booster_toggles = generic_booster_toggles) 

if (setting == "FJI" & TOGGLE_include_second_booster_elig == TRUE){
  generic_booster_toggles$prev_dose_floor = 4
  queue[[length(queue)+1]] = list(
    vax_strategy_description = 'all willing adults vaccinated with a primary schedule plus booster dose',
    vax_strategy_description_long = 'assume booster to all adults who have previously recieved two booster doses',
    risk_group_name = this_risk_group_name,
    risk_group_toggle = "on",
    booster_toggles = generic_booster_toggles)
}
#______________________________________________________________________________________________________________



### VACCINATION SCENARIO = BOOSTER ALL but prioritise risk-group (unrealistic but good for SM)
generic_booster_toggles$prioritised_risk = "Y"
generic_booster_toggles$prev_dose_floor = 2

generic_booster_toggles$delivery_risk_group = c('general_public',this_risk_group_name)
queue[[length(queue)+1]] = list(
  vax_strategy_description = 'all willing adults vaccinated with a primary schedule plus booster dose',
  vax_strategy_description_long = 'prioritise delivery to high-risk adults',
  risk_group_name = this_risk_group_name,
  risk_group_toggle = "on",
  booster_toggles = generic_booster_toggles) 

generic_booster_toggles$prioritised_risk = "N"
#______________________________________________________________________________________________________________



### VACCINATION SCENARIO = CATCHUP CAMPIGN TO BOOSTER ALL PREV PRIMARY SCHEDULE
#NB: only those who completed their primary schedule
generic_booster_toggles$prev_dose_floor = generic_booster_toggles$prev_dose_ceiling = 2

#high-risk adults
generic_booster_toggles$delivery_risk_group = c(this_risk_group_name)
queue[[length(queue)+1]] = list(
  vax_strategy_description = 'catchup campaign for high-risk adults',
  vax_strategy_description_long = 'assume booster to high-risk adults who have previously completed their primary schedule but have not recieved a booster',
  risk_group_name = this_risk_group_name,
  risk_group_toggle = "on",
  booster_toggles = generic_booster_toggles) 

#all adults
generic_booster_toggles$delivery_risk_group = c('general_public',this_risk_group_name)
queue[[length(queue)+1]] = list(
  vax_strategy_description = 'catchup campaign for all adults',
  vax_strategy_description_long = 'assume booster to all adults who have previously completed their primary schedule but have not recieved a booster',
  risk_group_name = this_risk_group_name,
  risk_group_toggle = "on",
  booster_toggles = generic_booster_toggles) 
#______________________________________________________________________________________________________________



### RUN MODEL #################################################################################################
RECORD_outcomes_without_antivirals = data.frame()
RECORD_likelihood_severe_outcome = data.frame()
RECORD_vaccination_history_FINAL = data.frame()
RECORD_incidence_log_tidy = data.frame()
RECORD_incidence_log = data.frame()
RECORD_exposed_log = data.frame()


for (ticket in 1:length(queue)){
  
  commands = queue[[ticket]]
    
  VE_loop = 0
  
  vax_strategy_description = commands$vax_strategy_description
  risk_group_name = commands$risk_group_name
  risk_group_toggle = commands$risk_group_toggle
  booster_toggles = commands$booster_toggles
  if ('risk_group_toggle' %in% names(commands)) { risk_group_toggle = commands$risk_group_toggle}
  if ('vax_strategy_toggles' %in% names(commands)) { vax_strategy_toggles = commands$vax_strategy_toggles}
  if ('vax_risk_strategy_toggle' %in% names(commands)) { vax_risk_strategy_toggle = commands$vax_risk_strategy_toggle}
  if ('apply_risk_strategy_toggles' %in% names(commands)) { apply_risk_strategy_toggles = commands$apply_risk_strategy_toggles}
  if ('vax_strategy_description_long' %in% names(commands)) { vax_strategy_description = paste(vax_strategy_description,commands$vax_strategy_description_long,sep=': ')}
 
   if (risk_group_name == "pregnant_women"){
     RR_estimate  = RR_default =  2.4
   } else if (risk_group_name == "adults_with_comorbidities"){
     RR_estimate  = RR_default = 1.95
   }
 
   source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
   
   ### CREATE DEPENDENCIES OF ANTIVIRAL FUNCTION (n=4)  #################################################################
   # Recall, dependencies of antiviral function from transmission model (n=5): incidence_log_tidy, severe_outcome_log_tidy, severe_outcome_this_run, reinfection_protection, param_age 
   # Let's collapse into:
   # (1) severe_outcome_log_tidy -> outcomes_without_antivirals,
   # (2) severe_outcome_this_run & reinfection_protection -> likelihood_severe_outcome,
   # (3) param_age -> prop_sympt, and leave
   # (4) incidence_log_tidy
   #COMEBACK - could make probab symptomatic or probab severe outcomes stochastic
   
   outcomes_without_antivirals = severe_outcome_log_tidy  %>%
     filter(format(date,format = "%Y") == 2023) %>%
     group_by(outcome) %>%
     summarise(overall = sum(proj))
   
   #adding some extra detail
   append_high_risk = severe_outcome_log_tidy  %>%
     filter(format(date,format = "%Y") == 2023) %>%
     filter(risk_group == risk_group_name) %>%
     group_by(outcome) %>%
     summarise(high_risk = sum(proj))
   append_booster_doses = booster_doses_delivered %>%
     summarise(overall = sum(doses_delivered)) %>%
     mutate(outcome = 'booster_doses_delivered')
   append_booster_doses_risk = booster_doses_delivered %>%
     filter(risk_group == risk_group_name) %>%
     rename(high_risk = doses_delivered) %>%
     mutate(outcome = 'booster_doses_delivered') %>%
     select(-risk_group)
   
   outcomes_without_antivirals = rbind(outcomes_without_antivirals,append_booster_doses)
   append_risk = rbind(append_high_risk,append_booster_doses_risk)
   outcomes_without_antivirals = outcomes_without_antivirals %>% left_join(append_risk, by = 'outcome')
   
   
   #ASSUMPTION: only symptomatic cases lead to severe outcomes
   prop_sympt = param_age %>% 
     filter(country == setting) %>%
     ungroup() %>%
     filter(param == 'prop_sympt') %>%
     select(-param)
   likelihood_severe_outcome = severe_outcome_this_run %>%
     filter(date >= date_start) %>%
     left_join(reinfection_protection, by = c("date", "age_group")) %>%
     mutate(percentage = percentage*(1-protection)) %>%
     select(-outcome_long,-protection) %>%
     left_join(prop_sympt,by= c('age_group' = 'agegroup')) %>%
     mutate(percentage = percentage * (1/value)) %>%
     select(-value)
   
   ###need to include variables which inform vaccination scenario and target group
   outcomes_without_antivirals = outcomes_without_antivirals %>%
     mutate(vax_scenario = vax_strategy_description,
            vax_scenario_risk_group = risk_group_name)
   likelihood_severe_outcome = likelihood_severe_outcome %>%
     mutate(vax_scenario = vax_strategy_description,
            vax_scenario_risk_group = risk_group_name)
   incidence_log_tidy = incidence_log_tidy %>%
     filter(date >= date_start - lengthInfectionDerivedImmunity) %>%
     mutate(vax_scenario = vax_strategy_description,
            vax_scenario_risk_group = risk_group_name)
   exposed_log = exposed_log %>%
     mutate(vax_scenario = vax_strategy_description,
            vax_scenario_risk_group = risk_group_name)
   incidence_log = incidence_log %>%
     mutate(vax_scenario = vax_strategy_description,
            vax_scenario_risk_group = risk_group_name)
   vaccination_history_FINAL = vaccination_history_FINAL %>%
     mutate(vax_scenario = vax_strategy_description,
            vax_scenario_risk_group = risk_group_name)
   
   # addition for CEA
   total_cases = incidence_log_tidy %>%
     group_by(risk_group, vax_scenario, vax_scenario_risk_group) %>%
     summarise(incidence = sum(incidence))
   total_cases = data.frame(outcome = "total_cases",
                            overall = sum(total_cases$incidence),
                            high_risk = total_cases$incidence[total_cases$risk_group == risk_group_name],
                            vax_scenario = vax_strategy_description,
                            vax_scenario_risk_group = risk_group_name)
   mild = incidence_log_tidy %>%
     group_by(risk_group, age_group, vax_scenario, vax_scenario_risk_group) %>%
     summarise(incidence = sum(incidence)) %>%
     rename(agegroup = age_group) %>%
     left_join(prop_sympt, by = c("agegroup")) %>%
     mutate(incidence = incidence * value) %>%
     group_by(risk_group, vax_scenario, vax_scenario_risk_group) %>%
     summarise(incidence = sum(incidence))
   mild = data.frame(outcome = "mild",
                     overall = sum(mild$incidence),
                     high_risk = mild$incidence[mild$risk_group == risk_group_name],
                     vax_scenario = vax_strategy_description,
                     vax_scenario_risk_group = risk_group_name)
   outcomes_without_antivirals = rbind(outcomes_without_antivirals,mild,total_cases)
   
   RECORD_outcomes_without_antivirals = rbind(RECORD_outcomes_without_antivirals,outcomes_without_antivirals)
   RECORD_likelihood_severe_outcome   = rbind(RECORD_likelihood_severe_outcome,likelihood_severe_outcome)
   RECORD_incidence_log_tidy          = rbind(RECORD_incidence_log_tidy,incidence_log_tidy)
   RECORD_vaccination_history_FINAL   = rbind(RECORD_vaccination_history_FINAL,vaccination_history_FINAL)
   RECORD_exposed_log                 = rbind(RECORD_exposed_log,exposed_log)
   RECORD_incidence_log               = rbind(RECORD_incidence_log,incidence_log)
   #____________________________________________________________________________________________________________________
   ###############################################################################################################
 }
sensitivity_analysis_toggles = list()
#____________________________________________________________________________


RECORD_antiviral_setup = list(outcomes_without_antivirals = RECORD_outcomes_without_antivirals,
                              prop_sympt = prop_sympt, #only static dependency
                              likelihood_severe_outcome = RECORD_likelihood_severe_outcome,
                              incidence_log_tidy = RECORD_incidence_log_tidy,
                              exposed_log = RECORD_exposed_log,
                              incidence_log = RECORD_incidence_log,
                              vaccination_history_FINAL = RECORD_vaccination_history_FINAL,
                              generic_booster_toggles = RECORD_generic_booster_toggles)


save.image(file = paste(rootpath,"x_results/antiviralSetUp_fullImage_",setting_beta,"_",this_risk_group_name,"_",Sys.Date(),".Rdata",sep=''))
save(RECORD_antiviral_setup, file = paste(rootpath,"x_results/antiviralSetUp_",setting_beta,"_",this_risk_group_name,"_",Sys.Date(),".Rdata",sep=''))

time.end.AntiviralSetUp=proc.time()[[3]]
time.end.AntiviralSetUp - time.start.AntiviralSetUp
#23/01/23 7 hours for PNG
###############################################################################################################




### VISUALLY SENSE CHECK MODEL RUNS ############################################################################
#RECORD_outcomes_without_antivirals = RECORD_outcomes_without_antivirals %>% arrange(outcome)
#View(RECORD_outcomes_without_antivirals)

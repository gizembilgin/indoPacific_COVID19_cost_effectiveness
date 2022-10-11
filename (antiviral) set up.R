### This script runs the primary transmission model for the scenarios of the antiviral paper.
### The incidence log and other antiviral model dependencies are then saved.

### DEPENDENCIES: nil!
rm(list=ls())


### SET UP MODEL RUN ################################################################
#start timing
time.start.AntiviralSetUp=proc.time()[[3]]

#load latest fit
fitting = "off";plotting = "off"
load(file = '1_inputs/last_fit_date.Rdata')
date_start = fitted_max_date 
if (abs(fitted_max_date - Sys.Date())>7){stop('please refit!')}

#initialise length of model run and circulating strain
strain_inital = strain_now = 'omicron' 
outbreak_timing = "off" #roll-out during steady state
model_weeks = 52

#turn on waning of all immunity
waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = TRUE
waning_toggle_rho_acqusition = TRUE

#turn off risk groups to start with
risk_group_toggle = "off"
vax_risk_strategy_toggle = "off"
risk_group_lower_cov_ratio = NA
risk_group_prioritisation_to_date = NA
sensitivity_analysis_toggles = list()

#set up setting
vax_strategy_toggle = "on"
setting = "SLE"
#setup up as primary schedule ONLY
vax_strategy_toggles_CURRENT_TARGET =
  list(vax_strategy_start_date        = date_start,
       vax_strategy_num_doses         = 99999999, #assume that all adults who are willing have been vaccinated
       vax_strategy_roll_out_speed    = 11075 ,                           # doses delivered per day
       vax_delivery_group             = 'universal',
       vax_age_strategy               = "uniform_no_children",            # options: "oldest", "youngest","50_down","uniform"
       vax_dose_strategy              = 1,                                # options: 1,2
       vax_strategy_vaccine_type      = "Johnson & Johnson" ,             # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
       vax_strategy_vaccine_interval  = c(90) ,                           #  (days) interval between doses, you must specify multiple intervals if multiple doses e.g. c(21,90)
       vax_strategy_max_expected_cov  = 0.88                              # value between 0-1 of age group willing to be vaccinated
  )

#initialise data frames
RECORD_outcomes_without_antivirals = data.frame()
RECORD_likelihood_severe_outcome = data.frame()
RECORD_incidence_log_tidy = data.frame()
queue = list()
#______________________________________________________________________________________________________________



### QUEUE #####################################################################################################
### VACCINATION SCENARIO = PRIMARY ONLY 
primary_only_toggles = list(
  vax_risk_strategy = 'N',           
  vax_risk_proportion = 0,         
  vax_doses_general = vax_strategy_toggles_CURRENT_TARGET$vax_dose_strategy,               
  vax_doses_risk = vax_strategy_toggles_CURRENT_TARGET$vax_dose_strategy,
  risk_group_acceptability = vax_strategy_toggles_CURRENT_TARGET$vax_strategy_max_expected_cov,
  risk_group_accessibility = FALSE,
  risk_group_age_broaden = FALSE
)

#(A/C) Adults with comorbidities
queue[[1]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule',
                  risk_group_name = 'adults_with_comorbidities',
                  risk_group_toggle = "on",
                  vax_risk_strategy_toggle = "on",
                  apply_risk_strategy_toggles = primary_only_toggles,
                  vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET)

#(B/C) Pregnant women
queue[[2]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule',
                  risk_group_name = 'pregnant_women',
                  risk_group_toggle = "on",
                  vax_risk_strategy_toggle = "on",
                  apply_risk_strategy_toggles = primary_only_toggles,
                  vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET) 

#(C/C) No risk groups
queue[[3]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule',
                  risk_group_name = 'none',
                  risk_group_toggle = "off",
                  vax_risk_strategy_toggle = "off",
                  vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET) 
#______________________________________________________________________________________________________________



### VACCINATION SCENARIO = PRIMARY + BOOSTER HIGH-RISK
booster_highRisk_toggles = primary_only_toggles
booster_highRisk_toggles$vax_doses_risk = booster_highRisk_toggles$vax_doses_risk + 1

#(A/B) Adults with comorbidities
queue[[4]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster',
                  risk_group_name = 'adults_with_comorbidities',
                  risk_group_toggle = "on",
                  vax_risk_strategy_toggle = "on",
                  apply_risk_strategy_toggles = booster_highRisk_toggles,
                  vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET) 

#(B/B) Pregnant women
queue[[5]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster',
                  risk_group_name = 'pregnant_women',
                  risk_group_toggle = "on",
                  vax_risk_strategy_toggle = "on",
                  apply_risk_strategy_toggles = booster_highRisk_toggles,
                  vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET) 
#______________________________________________________________________________________________________________



### VACCINATION SCENARIO = PRIMARY + BOOSTER ALL
booster_all_toggles = booster_highRisk_toggles
booster_all_toggles$vax_doses_general = booster_highRisk_toggles$vax_doses_risk

#(A/C) Adults with comorbidities
queue[[6]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule plus booster dose',
                  risk_group_name = 'adults_with_comorbidities',
                  risk_group_toggle = "on",
                  vax_risk_strategy_toggle = "on",
                  apply_risk_strategy_toggles = booster_all_toggles,
                  vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET)

#(B/C) Pregnant women
queue[[7]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule plus booster dose',
                  risk_group_name = 'pregnant_women',
                  risk_group_toggle = "on",
                  vax_risk_strategy_toggle = "on",
                  apply_risk_strategy_toggles = booster_all_toggles,
                  vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET) 

#(C/C) No risk groups
this_vax_strategy = vax_strategy_toggles_CURRENT_TARGET
this_vax_strategy$vax_dose_strategy = this_vax_strategy$vax_dose_strategy + 1

queue[[8]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule plus booster dose',
                  risk_group_name = 'none',
                  risk_group_toggle = "off",
                  vax_risk_strategy_toggle = "off",
                  vax_strategy_toggles = this_vax_strategy) 
#______________________________________________________________________________________________________________



### RUN MODEL #################################################################################################
for (ticket in 1:length(queue)){
  
  commands = queue[[ticket]]
  
  VE_loop = 0
  
  vax_strategy_description = commands$vax_strategy_description
  vax_strategy_toggles = commands$vax_strategy_toggles
  
  risk_group_name = commands$risk_group_name
  risk_group_toggle = commands$risk_group_toggle
  vax_risk_strategy_toggle = commands$vax_risk_strategy_toggle
  
  if ('apply_risk_strategy_toggles' %in% names(commands)){apply_risk_strategy_toggles = commands$apply_risk_strategy_toggles}
  if ('sensitivity_analysis_toggles' %in% names(commands)){sensitivity_analysis_toggles = commands$sensitivity_analysis_toggles
  } else{ sensitivity_analysis_toggles = list()}
  
  if (risk_group_name == "pregnant_women"){
    RR_estimate  = RR_default =  2.4
  } else if (risk_group_name == "adults_with_comorbidities"){
    RR_estimate  = RR_default = 1.95
  }

  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  ### CREATE DEPENDENCIES OF ANTIVIRAL FUNCTION (n=4)  #################################################################
  # Recall, dependencies of antiviral function from transmission model (n=5): incidence_log_tidy, severe_outcome_log_tidy, severe_outcome_this_run, reinfection_protection, param_age 
  # Let's collapse into:
  # (1) severe_outcome_log_tidy -> outcomes_without_antivirals,
  # (2) severe_outcome_this_run & reinfection_protection -> likelihood_severe_outcome,
  # (3) param_age -> prop_sympt, and leave
  # (4) incidence_log_tidy
  #COMEBACK - could make probab symptomatic or probab severe outcomes stochastic
  
  outcomes_without_antivirals = severe_outcome_log_tidy  %>%
    group_by(outcome) %>%
    summarise(overall = sum(proj))
  
  #ASSUMPTION: only symptomatic cases lead to severe outcomes
  prop_sympt = param_age %>% 
    ungroup() %>%
    filter(param == 'prop_sympt') %>%
    select(-param)
  likelihood_severe_outcome = severe_outcome_this_run %>%
    left_join(reinfection_protection, by = c("date", "age_group")) %>%
    mutate(percentage = percentage*(1-protection)) %>%
    select(-outcome_long,-protection) %>%
    left_join(prop_sympt,by= c('age_group' = 'agegroup')) %>%
    mutate(percentage = percentage * (1/value)) %>%
    select(-value)
  
  ###need to include variables which inform vaccination scenario and target group
  if (length(sensitivity_analysis_toggles) >0){VE_sensitivity_analysis = "on"
  } else{VE_sensitivity_analysis = "off"} 
  outcomes_without_antivirals = outcomes_without_antivirals %>%
    mutate(vax_scenario = vax_strategy_description,
           vax_scenario_risk_group = risk_group_name,
           VE_sensitivity_analysis = VE_sensitivity_analysis)
  likelihood_severe_outcome = likelihood_severe_outcome %>%
    mutate(vax_scenario = vax_strategy_description,
           vax_scenario_risk_group = risk_group_name,
           VE_sensitivity_analysis = VE_sensitivity_analysis)
  incidence_log_tidy = incidence_log_tidy %>%
    mutate(vax_scenario = vax_strategy_description,
           vax_scenario_risk_group = risk_group_name,
           VE_sensitivity_analysis = VE_sensitivity_analysis)
  
  #COMEBACK: choice, can embed vax_strategy_description and risk_group_name into four dependencies, or store outside - depends on what is easier in 'antiviral (simulations)'
  RECORD_outcomes_without_antivirals = rbind(RECORD_outcomes_without_antivirals,outcomes_without_antivirals)
  RECORD_likelihood_severe_outcome   = rbind(RECORD_likelihood_severe_outcome,likelihood_severe_outcome)
  RECORD_incidence_log_tidy          = rbind(RECORD_incidence_log_tidy,incidence_log_tidy)
  #____________________________________________________________________________________________________________________
  
  ### SENSITIVITY ANALYSIS - reduced VE in older adults and adults with comorbidities  
  if (risk_group_name == 'adults_with_comorbidities'){
    VE_loop = 1
    sensitivity_analysis_toggles = list(VE_older_adults = "reduced",VE_adults_comorb = 0.9)
    
    source(paste(getwd(),"/(5)_severe_outcomes_calc.R",sep="")) 
    source(paste(getwd(),"/(function)_severe_outcome_proj.R",sep=""))
    
    outcomes_without_antivirals = severe_outcome_log_tidy  %>%
      group_by(outcome) %>%
      summarise(overall = sum(proj))
    
    #ASSUMPTION: only symptomatic cases lead to severe outcomes
    prop_sympt = param_age %>% 
      ungroup() %>%
      filter(param == 'prop_sympt') %>%
      select(-param)
    likelihood_severe_outcome = severe_outcome_this_run %>%
      left_join(reinfection_protection, by = c("date", "age_group")) %>%
      mutate(percentage = percentage*(1-protection)) %>%
      select(-outcome_long,-protection) %>%
      left_join(prop_sympt,by= c('age_group' = 'agegroup')) %>%
      mutate(percentage = percentage * (1/value)) %>%
      select(-value)
    
    ###need to include variables which inform vaccination scenario and target group
    if (length(sensitivity_analysis_toggles) >0){VE_sensitivity_analysis = "on"
    } else{VE_sensitivity_analysis = "off"} 
    outcomes_without_antivirals = outcomes_without_antivirals %>%
      mutate(vax_scenario = vax_strategy_description,
             vax_scenario_risk_group = risk_group_name,
            VE_sensitivity_analysis = VE_sensitivity_analysis)
    likelihood_severe_outcome = likelihood_severe_outcome %>%
      mutate(vax_scenario = vax_strategy_description,
             vax_scenario_risk_group = risk_group_name,
             VE_sensitivity_analysis = VE_sensitivity_analysis)
    incidence_log_tidy = incidence_log_tidy %>%
      mutate(vax_scenario = vax_strategy_description,
             vax_scenario_risk_group = risk_group_name,
             VE_sensitivity_analysis = VE_sensitivity_analysis)
    
    #COMEBACK: choice, can embed vax_strategy_description and risk_group_name into four dependencies, or store outside - depends on what is easier in 'antiviral (simulations)'
    RECORD_outcomes_without_antivirals = rbind(RECORD_outcomes_without_antivirals,outcomes_without_antivirals)
    RECORD_likelihood_severe_outcome   = rbind(RECORD_likelihood_severe_outcome,likelihood_severe_outcome)
    RECORD_incidence_log_tidy          = rbind(RECORD_incidence_log_tidy,incidence_log_tidy)
  }
  ###############################################################################################################
  
  
}
sensitivity_analysis_toggles = list()
#____________________________________________________________________________


RECORD_antiviral_setup = list(outcomes_without_antivirals = RECORD_outcomes_without_antivirals,
                              prop_sympt = prop_sympt, #only static dependency
                              likelihood_severe_outcome = RECORD_likelihood_severe_outcome,
                              incidence_log_tidy = RECORD_incidence_log_tidy)


save.image(file = paste(rootpath,"x_results/antiviralSetUp_fullImage_",Sys.Date(),".Rdata",sep=''))
save(RECORD_antiviral_setup, file = paste(rootpath,"x_results/antiviralSetUp_",Sys.Date(),".Rdata",sep=''))

time.end.AntiviralSetUp=proc.time()[[3]]
time.end.AntiviralSetUp - time.start.AntiviralSetUp
###############################################################################################################




### SENSE CHECK MODEL RUNS ####################################################################################
# RECORD_outcomes_without_antivirals = RECORD_outcomes_without_antivirals %>% arrange(outcome)
# View(RECORD_outcomes_without_antivirals)


# This script runs the primary transmission model for the scenarios of the antiviral paper.
# The script then saves the dependencies of the antiviral model for ease of reload.



### SET UP MODEL RUN ################################################################
load(file = '1_inputs/last_fit_date.Rdata')
date_start = fitted_max_date ##latest fit date

strain_inital = strain_now = 'omicron' 
model_weeks = 52

waning_toggle_acqusition = TRUE; waning_toggle_severe_outcome = TRUE
waning_toggle_rho_acqusition = TRUE

outbreak_timing = "off"
vax_strategy_toggle = "on"
vax_risk_strategy_toggle = "off"
risk_group_toggle = "on" 
risk_group_prioritisation_to_date = NA
risk_group_lower_cov_ratio = NA
sensitivity_analysis_toggles = list()

gov_target = 0.516
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

this_run = list(
  vax_risk_strategy = 'N',           
  vax_risk_proportion = 0,         
  vax_doses_general = vax_strategy_toggles_CURRENT_TARGET$vax_dose_strategy,               
  vax_doses_risk = vax_strategy_toggles_CURRENT_TARGET$vax_dose_strategy,
  risk_group_acceptability = vax_strategy_toggles$vax_strategy_max_expected_cov,
  risk_group_accessibility = FALSE,
  risk_group_age_broaden = FALSE
)
#____________________________________________________________________________



### <RUN MODEL> ################################################################
# Run model for all scenarios for the antiviral model

queue = list()



queue[[1]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule',
                  apply_risk_strategy_toggles = this_run)

this_run$vax_doses_risk = 2
queue[[2]] = list(vax_strategy_description = 'at risk group recieve a booster',
                  apply_risk_strategy_toggles = this_run) 


for (ticket in 1:length(queue)){
  
  commands = queue[[ticket]]
  
  VE_loop = 0
  vax_strategy_description = commands$vax_strategy_description
  apply_risk_strategy_toggles = commands$apply_risk_strategy_toggles
  
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
    filter(param == 'prop_sympt') %>%
    select(-param)
  likelihood_severe_outcome = severe_outcome_this_run %>%
    left_join(reinfection_protection, by = c("date", "age_group")) %>%
    mutate(percentage = percentage*(1-protection)) %>%
    select(-outcome_long,-protection) %>%
    left_join(prop_sympt,by= c('age_group' = 'agegroup')) %>%
    mutate(percentage = percentage * (1/value)) %>%
    select(-value)
  
  antiviral_model_dependencies = list(outcomes_without_antivirals,prop_sympt,likelihood_severe_outcome,incidence_log_tidy)
  #____________________________________________________________________________________________________________________
  

}

#____________________________________________________________________________



### This script runs the primary transmission model for the scenarios of the antiviral paper.
### The incidence log and other antiviral model dependencies are then saved.

### DEPENDENCIES: nil!
rm(list=ls())


### SET UP MODEL RUN ################################################################
#general toggles
fitting = "on"
plotting = "on"
outbreak_timing = "off"
vax_strategy_toggle = "off"
vax_risk_strategy_toggle = "off"
risk_group_lower_cov_ratio = NA
risk_group_prioritisation_to_date = NA
sensitivity_analysis_toggles = list()

#turn on all waning
waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = TRUE
waning_toggle_rho_acqusition = TRUE

date_start = as.Date('2021-03-31')
strain_inital = strain_now = 'WT' 
seed_date = c(as.Date('2021-04-25'),as.Date('2021-09-01')) #first is seed date for delta, second is omicron
model_weeks = as.numeric((Sys.Date()+1-date_start)/7) + 52

#set up setting
setting = "SLE"
source(paste(getwd(),"/(1)_simulate_setting.R",sep=""))

#general vax strategy toggles
vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET =
  list(vax_strategy_start_date        = Sys.Date(),
       vax_strategy_num_doses         = 99999999, #assume that all adults who are willing have been vaccinated
       vax_strategy_roll_out_speed    = 11075 ,                           # doses delivered per day
       vax_delivery_group             = 'universal',
       vax_age_strategy               = "uniform_no_children",            # options: "oldest", "youngest","50_down","uniform"
       vax_dose_strategy              = 1,                                # options: 1,2
       vax_strategy_vaccine_type      = "Johnson & Johnson" ,             # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
       vax_strategy_vaccine_interval  = c(90) ,                           #  (days) interval between doses, you must specify multiple intervals if multiple doses e.g. c(21,90)
       vax_strategy_max_expected_cov  = 0.88                              # value between 0-1 of age group willing to be vaccinated
  )

#risk strategy toggles
noRisk_run = list(
  vax_risk_strategy = 'N',           
  vax_risk_proportion = 0,         
  vax_doses_general = vax_strategy_toggles_CURRENT_TARGET$vax_dose_strategy,               
  vax_doses_risk = vax_strategy_toggles_CURRENT_TARGET$vax_dose_strategy,
  risk_group_acceptability = vax_strategy_toggles$vax_strategy_max_expected_cov,
  risk_group_accessibility = FALSE,
  risk_group_age_broaden = FALSE
)
pregnantWomen_run = list(
  vax_risk_strategy = 'Y',           
  vax_risk_proportion = 0,  #HERE       
  vax_doses_general = vax_strategy_toggles_CURRENT_TARGET$vax_dose_strategy,               
  vax_doses_risk = vax_strategy_toggles_CURRENT_TARGET$vax_dose_strategy,
  risk_group_acceptability = vax_strategy_toggles$vax_strategy_max_expected_cov,
  risk_group_accessibility = FALSE,
  risk_group_age_broaden = FALSE
)
adultsComorb_run = pregnantWomen_run
adultsComorb_run$vax_risk_proportion = 

if (new_variant_check == "on"){
  seed_date = c(as.Date('2021-04-25'),as.Date('2021-09-01'),as.Date('2022-09-01')) #check new variant
  model_weeks = model_weeks + 52 #to see expected trajectory
}

plot_standard = theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'))


#initialise data frames
fitted_results = list()
queue = list()
#______________________________________________________________________________________________________________



### QUEUE #####################################################################################################
### VACCINATION SCENARIO = PRIMARY ONLY 
#(A/C) Adults with comorbidities
queue[[1]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule',
                  risk_group_name = 'adults_with_comorbidities',
                  risk_group_toggle = "on",
                  vax_risk_strategy_toggle = "on",
                  apply_risk_strategy_toggles = adultsComorb_run)

#(B/C) Pregnant women
queue[[2]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule',
                  risk_group_name = 'pregnant_women',
                  risk_group_toggle = "on",
                  vax_risk_strategy_toggle = "on",
                  apply_risk_strategy_toggles = pregnantWomen_run) 

#(C/C) No risk groups
queue[[3]] = list(vax_strategy_description = 'all willing adults vaccinated with a primary schedule',
                  risk_group_toggle = "on",
                  vax_risk_strategy_toggle = "on",
                  apply_risk_strategy_toggles = noRisk_run) 
#______________________________________________________________________________________________________________



### VACCINATION SCENARIO = PRIMARY + BOOSTER HIGH-RISK
#(A/B) Adults with comorbidities
this_run = 
this_run$vax_doses_risk = 2
queue[[3]] = list(vax_strategy_description = 'at risk group recieve a booster',
                  risk_group_toggle = "on",
                  vax_risk_strategy_toggle = "on",
                  apply_risk_strategy_toggles = noRisk_run) 
#(B/B) Pregnant women
this_run = 
this_run$vax_doses_risk = 2
#______________________________________________________________________________________________________________



### VACCINATION SCENARIO = PRIMARY + BOOSTER ALL
#(A/C) Adults with comorbidities

#(B/C) Pregnant women

#(C/C) No risk groups
#______________________________________________________________________________________________________________



### SENSITIVITY ANALYSIS - reduced VE in older adults and adults with comorbidities
#(A/B) Primary schedule only

#(B/B) Primary + booster schedule
#______________________________________________________________________________________________________________
###############################################################################################################




#____________________________________________________________________________



### RUN MODEL ###############################################################################################
for (ticket in 1:length(queue)){
  
  commands = queue[[ticket]]
  
  VE_loop = 0
  vax_strategy_description = commands$vax_strategy_description
  apply_risk_strategy_toggles = commands$apply_risk_strategy_toggles
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  #CHECK
  hypoth_doses = vaccination_history_FINAL %>% 
    filter(! age_group %in% c('0 to 4','5 to 9','10 to 17')) %>%
    group_by(risk_group,age_group,dose) %>%
    summarise(doses = sum(doses_delivered_this_date),.groups = "keep") %>%
    left_join(pop_risk_group_dn, by = c("risk_group", "age_group")) %>%
    mutate(cov=doses/pop) %>%
    arrange(dose,age_group)
  if (!unique(na.omit(round(hypoth_doses$cov[hypoth_doses$dose == 1],digits=2))) == 0.88){
    warning('not all willing adults vaccinated')
  }
  
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
  
  ###need to include variables which inform vaccination scenario and target group
  
  antiviral_model_dependencies = list(outcomes_without_antivirals,prop_sympt,likelihood_severe_outcome,incidence_log_tidy)
  #____________________________________________________________________________________________________________________
  
}
#____________________________________________________________________________



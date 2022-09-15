time.start.AntiviralModel=proc.time()[[3]]

#DEPENDENCIES FROM PRIMARY MODE: severe_outcome_this_run, reinfection_protection, incidence_log_tidy, date_start, severe_outcome_log_tidy


colnames(incidence_log_tidy)
#"date"         "incidence"    "temp_risk"    "risk_group"   "age_group"    "dose"         "vaccine_type"   
#NOTE: we are assuming incidence is day 0 of symptom onset


### TOGGLES ################################################################
toggle_antiviral_target = 'adults_with_comorbidities' #options: adults_with_comorbidities (baseline), unvaccinated_adults, pregnant_women, or all_adults
toggle_antiviral_start_date = as.Date('2022-09-01')
toggle_antiviral_delivery_capacity = 250 #daily capacity for antiviral delivery
toggle_antiviral_effectiveness = 0.88 #could make stochastic
toggle_number_of_runs = 1000
#____________________________________________________________________________


### INITALISE DATA FRAMES #################################################################
this_scenario_tracker = data.frame()

#COMEBACK - could make these likelihoods stochastic
outcomes_without_antivirals = severe_outcome_log_tidy  %>%
  group_by(outcome) %>%
  summarise(overall = sum(proj))

likelihood_severe_outcome = severe_outcome_this_run %>%
  left_join(reinfection_protection, by = c("date", "age_group")) %>%
  mutate(percentage = percentage*(1-protection)) %>%
  select(-outcome_long,-protection)
#____________________________________________________________________________


### CHECKS #################################################################
if (toggle_antiviral_target %in% c('adults_with_comorbidities','pregnant_women')){
  if (! toggle_antiviral_target %in% unique(incidence_log_tidy$risk_group)){
    stop('target for antivirals not included as a risk group in model run!')
  }
}
#____________________________________________________________________________


### SELECT TARGET GROUP ######################################################
if (toggle_antiviral_target %in% c('adults_with_comorbidities','pregnant_women')){
  antiviral_target = incidence_log_tidy %>% 
    filter(risk_group == toggle_antiviral_target)
} else if (toggle_antiviral_target == 'unvaccinated_adults'){
  antiviral_target = incidence_log_tidy %>% 
    filter(dose == 0) 
} else if (toggle_antiviral_target == 'all_adults'){
  antiviral_target = incidence_log_tidy %>% 
    filter(! age_group %in% c("0 to 4","5 to 9","10 to 17")) 
} else {
  stop('pick a valid toggle_antiviral_target!')
}
antiviral_target = antiviral_target %>% filter(incidence>0,
                                               date > date_start)
#____________________________________________________________________________


### SELECT SYMPTOMATIC INDIVIDUAL#############################################
#ASSUMPTION: vaccination does not affect the likelihood of an individual being symptomatic
prop_sympt = param_age %>% 
  filter(param == 'prop_sympt') %>%
  select(-param)

antiviral_target = antiviral_target %>% 
  left_join(prop_sympt, by = c('age_group' = 'agegroup')) %>%
  mutate(symptomatic = incidence * value) %>%
  select(-incidence,-value)

antiviral_target_individuals = antiviral_target %>%
  mutate(symptomatic = round(symptomatic)) %>%
  select(-temp_risk) %>%
  filter(symptomatic>0)
#____________________________________________________________________________


### CREATE DATASET WITH INDIVIDUALS ############################################
workshop = as.data.frame(lapply(antiviral_target_individuals, rep, antiviral_target_individuals$symptomatic))
if(! nrow(workshop) == sum(antiviral_target_individuals$symptomatic)){stop('split to individuals has lost individuals')}

antiviral_target_individuals = workshop %>% select(-symptomatic)
antiviral_target_individuals$ID <- seq.int(nrow(antiviral_target_individuals)) #helpful for checking
#____________________________________________________________________________

###### BEGIN STOCHASTIC
for (run_number in 1:toggle_number_of_runs){
  ### PATHWAY TO CARE STEP ONE: Does this individual seek care? ################
  healthcare_seeking = function(age_group){
    #COMEBACK: need real data to estimate
    if (age_group %in% c( "0 to 4",    "5 to 9",    "10 to 17")){
      sample = rbinom(1,1,0.5) #rbinom(number of observations,number of trials,probability of success on each trial)
    } else{
      sample = rbinom(1,1,0.25)
    }
    return(sample)
  }
  
  workshop <- as.data.frame(sapply(antiviral_target_individuals$age_group, healthcare_seeking))
  colnames(workshop) = c('healthcare_seeking')
  workshop = cbind(antiviral_target_individuals,workshop)
  
  antiviral_target_individuals_run = workshop %>%
    filter(healthcare_seeking == 1) %>% #retain those who seek care
    select(-healthcare_seeking)
  #____________________________________________________________________________
  
  
  ### PATHWAY TO CARE STEP TWO: How many days after symptom onset can the individual access care?#######
  healthcare_access = function(age_group){
    #COMEBACK: need real data to estimate
    if (age_group %in% c( "0 to 4",    "5 to 9",    "10 to 17")){
      sample = round(runif(1, min = 0, max = 7)) 
    } else{
      sample = round(runif(1, min = 0, max = 7)) 
    }
    return(sample)
  }
  
  workshop <- as.data.frame(sapply(antiviral_target_individuals_run$age_group, healthcare_access))
  colnames(workshop) = c('healthcare_access')
  workshop = cbind(antiviral_target_individuals_run,workshop)
  
  antiviral_target_individuals_run = workshop %>%
    filter(healthcare_access < 6) %>% #retain those who access care within 5 days
    mutate(min_date_access = date + healthcare_access,
           max_date_access = date + 5) %>% 
    select(-healthcare_access) 
  #____________________________________________________________________________
  
  
  ### PATHWAY TO CARE STEP THREE: Is the individual allocated antivirals?#######
  #ASSUMPTION: allocated at random
  antiviral_delivery_length = as.numeric(max(incidence_log_tidy$date) - toggle_antiviral_start_date)
  antiviral_delivery_tracker = data.frame()
  
  for (day in 0:antiviral_delivery_length){
    this_date =  toggle_antiviral_start_date + day
    
    #include all presenting within 
    presentations_this_date = antiviral_target_individuals_run %>% 
      filter(this_date>=min_date_access & this_date<=max_date_access) %>%
      filter(!ID %in% antiviral_delivery_tracker$ID) #make sure previous recipients removed from future decisions
    
    num_to_sample = min(nrow(presentations_this_date),toggle_antiviral_delivery_capacity) #making sure number of doses delivered <= number of available recipients
    
    antiviral_recipients_this_date = data.frame(ID = sample(presentations_this_date$ID,num_to_sample,replace=FALSE))
    antiviral_recipients_this_date = antiviral_recipients_this_date %>% mutate(date = this_date)
    antiviral_delivery_tracker = rbind(antiviral_delivery_tracker,antiviral_recipients_this_date)
  }
  
  antiviral_target_individuals_run = antiviral_delivery_tracker %>% 
    select(-date) %>%
    left_join(antiviral_target_individuals_run, by = 'ID') #remove all not selected for antivirals 
  #____________________________________________________________________________
  
  
  ### PATHWAY TO CARE STEP FOUR: How many cases of severe disease are prevented?#######
  workshop = antiviral_target_individuals_run %>%
    left_join(likelihood_severe_outcome, by = c("date", "risk_group", "age_group", "dose", "vaccine_type")) %>%
    mutate(percentage = percentage*toggle_antiviral_effectiveness) 
  
  prevented_by_antivirals = workshop %>%
    group_by(outcome) %>%
    summarise(n = sum(percentage)) 
  
  this_scenario_tracker = rbind(this_scenario_tracker,prevented_by_antivirals)
  #____________________________________________________________________________
}

summary_over_runs <- 
  this_scenario_tracker %>%
  group_by(outcome) %>%
  dplyr::summarise(average = mean(n), 
                   sd = sd(n),
                   UCI = average - qnorm(0.975)*sd,
                   LCI = average - qnorm(0.023)*sd) %>%
  left_join(outcomes_without_antivirals) %>%
  mutate(percentage = average/overall *100,
         UCI_percentage = UCI/overall *100,
         LCI_percentage = LCI/overall *100) %>%
  select(outcome,average,UCI,LCI,percentage,UCI_percentage,LCI_percentage)

summary_over_runs 

time.end.AntiviralModel=proc.time()[[3]]
time.end.AntiviralModel-time.start.AntiviralModel

#100 runs takes (15/09)
#1000 runs takes (16/09)


### This program sets the state of the system to our study location
###
### COMEBACK count = 5


###### (1/4) Vaccination
#(A/B) Coverage 
#(i/iv) Delay & Interval ____________________________________
vaxCovDelay = crossing(dose = seq(1,num_vax_doses),delay = 0)
vaxCovDelay = vaxCovDelay %>%
  mutate(delay = case_when(
    dose == 1 ~ 21,#number of days till protection from first dose, COMEBACK - J&J full protection after 14 days?
    TRUE ~ 14 #all other doses
  ))

vaccineDoseInterval <- read.csv("1_inputs/vaccine_dose_intervals.csv", header=TRUE)

#(ii/iv) #Vaccine coverage at end of true history
vaccine_coverage_end_history = vaccination_history_TRUE %>% filter(date == max(vaccination_history_TRUE$date)) %>%
  select(dose,vaccine_type,age_group,risk_group,coverage_this_date)
#_________________________________________________

#(iii/iv) Add hypothetical campaign (if 'on') ____
if (vax_strategy_toggle == "on" & vax_risk_strategy_toggle == "off"){
  vaccination_history_FINAL = 
    vax_strategy(vax_strategy_start_date        = vax_strategy_toggles$vax_strategy_start_date,
                 vax_strategy_num_doses         = vax_strategy_toggles$vax_strategy_num_doses,
                 vax_strategy_roll_out_speed    = vax_strategy_toggles$vax_strategy_roll_out_speed,
                 vax_age_strategy               = vax_strategy_toggles$vax_age_strategy,  
                 vax_delivery_group             = 'universal',
                 vax_dose_strategy              = vax_strategy_toggles$vax_dose_strategy,            
                 vax_strategy_vaccine_type      = vax_strategy_toggles$vax_strategy_vaccine_type,            
                 vax_strategy_vaccine_interval  = vax_strategy_toggles$vax_strategy_vaccine_interval,            
                 vax_strategy_max_expected_cov  = vax_strategy_toggles$vax_strategy_max_expected_cov
    )
  
  #recalculate!
  list_doses = unique(vaccination_history_FINAL$dose)
  list_doses = list_doses[! list_doses %in% c(8)]
  num_vax_doses = D = length(list_doses)  # dose 1, dose 2, COMEBACK no boosters yet in these settings 
  vax_type_list = sort(unique(vaccination_history_FINAL$vaccine_type))
  num_vax_types = T = length(unique(vaccination_history_FINAL$vaccine_type))
  num_vax_classes = num_vax_doses*num_vax_types + 1                 # + 1 for unvaccinated
  
  
} else if (vax_strategy_toggle == "on" & vax_risk_strategy_toggle == "on"){
  
  if(risk_group_acceptability %in% apply_risk_strategy_toggles){
    vaccination_history_FINAL = 
      apply_risk_strategy(vax_risk_strategy     = apply_risk_strategy_toggles$vax_risk_strategy,            
                          vax_risk_proportion   = apply_risk_strategy_toggles$vax_risk_proportion,      
                          vax_doses_general     = apply_risk_strategy_toggles$vax_doses_general,      
                          vax_doses_risk        = apply_risk_strategy_toggles$vax_doses_risk,
                          risk_group_acceptability = apply_risk_strategy_toggles$risk_group_acceptability
      )
  } else{
    vaccination_history_FINAL = 
      apply_risk_strategy(vax_risk_strategy     = apply_risk_strategy_toggles$vax_risk_strategy,            
                          vax_risk_proportion   = apply_risk_strategy_toggles$vax_risk_proportion,      
                          vax_doses_general     = apply_risk_strategy_toggles$vax_doses_general,      
                          vax_doses_risk        = apply_risk_strategy_toggles$vax_doses_risk
      )
  }
  
  #recalculate!
  list_doses = unique(vaccination_history_FINAL$dose)
  list_doses = list_doses[! list_doses %in% c(8)]
  num_vax_doses = D = length(list_doses)  # dose 1, dose 2, COMEBACK no boosters yet in these settings 
  vax_type_list = sort(unique(vaccination_history_FINAL$vaccine_type))
  num_vax_types = T = length(unique(vaccination_history_FINAL$vaccine_type))
  num_vax_classes = num_vax_doses*num_vax_types + 1                 # + 1 for unvaccinated
 
  
  date_complete_at_risk_group = vaccination_history_FINAL %>% 
    filter(risk_group == risk_group_name) %>%
    filter(doses_delivered_this_date > 0)
  date_complete_at_risk_group = max(date_complete_at_risk_group$date)
  
} else {
  vaccination_history_FINAL = vaccination_history_TRUE
}

if(outbreak_post_rollout == "on"){
  date_start = max(vaccination_history_FINAL$date)
  seed_date = date_start
}


#_________________________________________________

#(iv/iv)  Initial coverage _______________________
#Including coverage_this_date for projected doses
vaccination_history_FINAL = vaccination_history_FINAL %>% left_join(pop_risk_group_dn) %>%
  group_by(risk_group,age_group,vaccine_type,dose) %>%
  mutate(coverage_this_date = 100*cumsum(doses_delivered_this_date)/pop) %>%
  select(-pop)
vaccination_history_FINAL$coverage_this_date[is.na(vaccination_history_FINAL$coverage_this_date)] = NA

#calc
vaccine_coverage = crossing(risk_group = risk_group_labels,
                            dose = c(1:num_vax_doses),
                            vaccine_type = unique(vaccination_history_FINAL$vaccine_type),
                            age_group = age_group_labels,
                            cov = c(0)) # CHECKED:vaccine coverage long like E/I/R structure
for (r in 1:num_risk_groups){ # risk group
  for (i in 1:J){             # age
    for (t in 1:T){           # vaccine type
      for (d in 1:D){         # vaccine dose
        workshop_type =  unique(vaccination_history_FINAL$vaccine_type)[t]
        workshop_age  = age_group_labels[i]
        workshop_risk = risk_group_labels[r]
        this_vax_max_date = max(vaccination_history_FINAL$date[vaccination_history_FINAL$vaccine_type == workshop_type])
        
          if ((date_start - vaxCovDelay$delay[vaxCovDelay$dose == d])<= this_vax_max_date &
              (date_start - vaxCovDelay$delay[vaxCovDelay$dose == d])>= min(vaccination_history_FINAL$date)){
            
            workshop_value =  vaccination_history_FINAL$coverage_this_date[
              vaccination_history_FINAL$date == date_start - vaxCovDelay$delay[vaxCovDelay$dose == d]
              & vaccination_history_FINAL$age_group == workshop_age
              & vaccination_history_FINAL$risk_group == workshop_risk
              & vaccination_history_FINAL$dose == d
              & vaccination_history_FINAL$vaccine_type == workshop_type] / 100
            
            vaccine_coverage$cov[
                vaccine_coverage$dose == d &
                vaccine_coverage$vaccine_type == workshop_type &
                vaccine_coverage$age_group == workshop_age &
                vaccine_coverage$risk_group == workshop_risk 
            ] = max(workshop_value,0)
             
          } else if ((date_start -vaxCovDelay$delay[vaxCovDelay$dose == d])> this_vax_max_date){
            workshop_value =
              vaccination_history_FINAL$coverage_this_date[
                vaccination_history_FINAL$date == this_vax_max_date
                & vaccination_history_FINAL$dose == d
                & vaccination_history_FINAL$age_group == workshop_age
                & vaccination_history_FINAL$risk_group == workshop_risk
                & vaccination_history_FINAL$vaccine_type == workshop_type]/100 
            
            vaccine_coverage$cov[
              vaccine_coverage$dose == d &
                vaccine_coverage$vaccine_type == workshop_type &
                vaccine_coverage$age_group == workshop_age &
                vaccine_coverage$risk_group == workshop_risk 
              ] =  max(workshop_value,0)
          } 
      }
    }
  }
}
vaccine_coverage$cov[is.na(vaccine_coverage$cov)] = 0



#(B/B) Vaccine Effectiveness (VE)
#load( file = '1_inputs/VE_waning_distribution.Rdata')
load( file = '1_inputs/VE_waning_distribution.Rdata')
VE_waning_distribution = VE_waning_distribution[VE_waning_distribution$waning == waning_toggle_acqusition,] %>%
  mutate(outcome = 'any_infection')

# part_one = VE_waning_distribution[VE_waning_distribution$outcome %in% c('any_infection','symptomatic_disease') &
#                                     VE_waning_distribution$waning == waning_toggle_acqusition,]
# part_two = VE_waning_distribution[VE_waning_distribution$outcome %in% c('severe_disease','death') &
#                                     VE_waning_distribution$waning == waning_toggle_severe_outcome,]
# VE_waning_distribution = rbind(part_one,part_two)
# rm(part_one,part_two)

VE =  crossing(dose = c(1:num_vax_doses),
               vaccine_type = unique(vaccination_history_FINAL$vaccine_type),
               age_group = age_group_labels,
               VE = c(0)) 
if ((date_start - vaxCovDelay$delay[vaxCovDelay$dose == d])>= min(vaccination_history_POP$date)){
  VE = VE_inital = VE_time_step(strain_inital,date_start,'any_infection')
  #VE_onwards_inital <- VE_time_step(strain_inital,date_start,'transmission')
}
#___________________________________________________________________



###### (2/4) Seroprevalence
load(file = "1_inputs/seroprev.Rdata")
seroprev = seroprev[seroprev$setting == setting & seroprev$year == 
                      as.numeric(format(date_start, format="%Y")),]
if (as.numeric(format(date_start, format="%Y")) > 2022){
  load(file = "1_inputs/seroprev.Rdata")
  seroprev = seroprev[seroprev$setting == setting & seroprev$year ==  2022,]
}
#___________________________________________________________________



###### (3/4) NPI
if (NPI_toggle == 'stringency'){ NPI_estimates = NPI_estimates_full[,-c(3)]
} else if (NPI_toggle == 'contain_health'){ NPI_estimates = NPI_estimates_full[,-c(2)]}
colnames(NPI_estimates) <- c('date','NPI')

if(date_start <=max(NPI_estimates$date)){
  NPI_inital = NPI_estimates$NPI[NPI_estimates$date==date_start]
} else {
  if (NPI_outbreak_toggle == "final"){
    NPI_inital = NPI_estimates$NPI[NPI_estimates$date == max(NPI_estimates$date)] # peak is 40.8
  } 
  if (NPI_outbreak_toggle == "delta_peaks"){
    #NPI_inital = NPI_estimates$NPI[NPI_estimates$date == as.Date('2021-07-07')] #peak is 62.3
    NPI_inital = mean(NPI_estimates$NPI[NPI_estimates$date > as.Date('2021-01-01')&NPI_estimates$date<as.Date('2021-08-01')])
    #average of two delta peaks in 2021
  }
}
NPI = NPI_inital = as.numeric(NPI_inital)/100
#________________________________________________________________



###### (4/4) Hence, initial state
#(A/F): intialise classes
J=num_age_groups
T=num_vax_types
D=num_vax_doses
RISK=num_risk_groups
count=J*(T*D+1)*RISK # +1 is unvax

S_inital=E_inital=I_inital=R_inital=(rep(0,count)) 

#(B/F): number of active infected/recovered cases
#NB: difference in Recovered where age dn when using seroprev data

if (date_start <= max(case_history$date)){
  initialInfected = case_history %>%
    filter(date == date_start) %>%
    select(active) %>%
    as.numeric()
  initialExposed = case_history %>%
    filter(date == date_start+14) %>%
    select(active) %>%
    as.numeric()  
  
  initialRecovered = case_history %>%
    filter(date == date_start) %>%
    select(recovered) %>%
    as.numeric()
  initialRecovered = pop_risk_group_dn %>% ungroup() %>% 
    mutate(R = initialRecovered*pop/sum(pop)) %>%
    select(risk_group,age_group,R)
  
  hist_cases = case_history %>% mutate(daily_cases = new * underascertainment_est) %>%
    select(date,daily_cases)
}
if (seed>0 & seed_date == date_start) { #overwrite
  initialInfected = seed*AverageSymptomaticPeriod/(AverageSymptomaticPeriod+AverageLatentPeriod) 
  initialExposed  = seed*AverageLatentPeriod/(AverageSymptomaticPeriod+AverageLatentPeriod) 
}
if (date_start > max(case_history$date)){
  
  initialRecovered = seroprev %>% left_join(pop_setting) %>% mutate(R = seroprev*pop/100) %>%
    select(age_group,R)
  
  date = seq(1,lengthInfectionDerivedImmunity)
  date = date_start - date
  workshop = as.data.frame(date)
  workshop$daily_cases = sum(initialRecovered$R)/lengthInfectionDerivedImmunity
  hist_cases = workshop
  
  recovered_risk = initialRecovered %>% left_join(risk_dn) %>%
    mutate(risk_group = risk_group_name,
           R = R*prop) %>% 
    select(risk_group,age_group,R)
  recovered_general_public   = initialRecovered %>% left_join(risk_dn) %>%
    mutate(risk_group = 'general_public',
           R = R*(1-prop)) %>% 
    select(risk_group,age_group,R)
  initialRecovered = rbind(recovered_general_public,recovered_risk)
  
}

#(C/F): age distribution of cases
#COMEBACK: no data, so assuming uniform across age groups
initialClasses = pop_risk_group_dn %>% ungroup() %>% 
  mutate(I = initialInfected*pop/sum(pop),
         E = initialExposed*pop/sum(pop)) %>% 
  left_join(initialRecovered) %>%
  mutate(S = pop - E - I - R) %>%
  select(-pop) %>%
  pivot_longer(
    cols = I:S,
    names_to = 'class',
    values_to = 'state_inital'
  ) 
if (round(sum(initialClasses$state_inital)) != sum(pop)){stop('(2) inital state line 248')}  


  # Step Four B: distribute across vaccine classes 
  # COMEBACK, assumption infections are spread equally across vax classes
disease_class_list = c('S','E','I','R')

empty_unvaccinated = crossing(class = disease_class_list,
                              risk_group = risk_group_labels,
                              dose = 0,
                              vaccine_type = 'unvaccinated',
                              age_group = age_group_labels,
                              state_inital = 0)
empty_vaccinated = crossing(class = disease_class_list,
                            risk_group = risk_group_labels,
                            dose = seq(1,num_vax_doses),
                            vaccine_type = vax_type_list,
                            age_group = age_group_labels,
                            state_inital = 0)
state_tidy = rbind(empty_unvaccinated,empty_vaccinated)

for (num in 1:num_disease_classes){
  
  workshop = initialClasses %>% filter(class ==  disease_class_list[num] )
  
  for (r in 1:RISK){
    for (i in 1:J){ # age
      #pop*(1-cov1A-cov1B-cov1C)
      #unvaccinated
      state_tidy$state_inital[state_tidy$dose == 0 & 
                                state_tidy$age_group == age_group_labels[i] &
                                state_tidy$risk_group == risk_group_labels[r] &
                                state_tidy$class == disease_class_list[num]] = 
        workshop$state_inital[workshop$risk_group ==  risk_group_labels[r] & workshop$age_group == age_group_labels[i]]*
        (1-sum(vaccine_coverage$cov[vaccine_coverage$dose == 1 & 
                                      vaccine_coverage$age_group == age_group_labels[i] & 
                                      vaccine_coverage$risk_group == risk_group_labels[r]]))
      
      for (t in 1:T){  # vaccine type
        for (d in 1:D){ # vaccine dose
  
          if (d != D){
            #pop*(cov1A-cov2A)
            state_tidy$state_inital[state_tidy$dose == d  & state_tidy$vaccine_type == vax_type_list[t]& 
                                      state_tidy$age_group == age_group_labels[i] &
                                      state_tidy$risk_group == risk_group_labels[r] &
                                      state_tidy$class == disease_class_list[num]] = 
              workshop$state_inital[workshop$risk_group ==  risk_group_labels[r] & workshop$age_group == age_group_labels[i]]*
              (vaccine_coverage$cov[vaccine_coverage$dose == d &
                                      vaccine_coverage$vaccine_type == vax_type_list[t] &
                                      vaccine_coverage$age_group == age_group_labels[i] & 
                                      vaccine_coverage$risk_group == risk_group_labels[r]] -
                 vaccine_coverage$cov[vaccine_coverage$dose == d+1 &
                                        vaccine_coverage$vaccine_type == vax_type_list[t] &
                                        vaccine_coverage$age_group == age_group_labels[i] & 
                                        vaccine_coverage$risk_group == risk_group_labels[r]])
          }
          if (d == D){
            #pop*cov2A
            state_tidy$state_inital[state_tidy$dose == d & state_tidy$vaccine_type == vax_type_list[t] & 
                                      state_tidy$age_group == age_group_labels[i] & 
                                      state_tidy$risk_group == risk_group_labels[r] &
                                      state_tidy$class == disease_class_list[num]] = 
              workshop$state_inital[workshop$risk_group ==  risk_group_labels[r] & workshop$age_group == age_group_labels[i]]*
              (vaccine_coverage$cov[vaccine_coverage$dose == d &
                                      vaccine_coverage$vaccine_type == vax_type_list[t]&
                                      vaccine_coverage$age_group == age_group_labels[i]& 
                                      vaccine_coverage$risk_group == risk_group_labels[r]])
          }
        }
      }
    }
  }
}    

state_tidy$class = factor(state_tidy$class, levels = disease_class_list)
state_tidy$risk_group = factor(state_tidy$risk_group, levels = risk_group_labels)
state_tidy$age_group = factor(state_tidy$age_group, levels = age_group_labels)
  
state_tidy = state_tidy %>% arrange(class,risk_group,dose,vaccine_type,age_group)
state_tidy$state_inital[is.na(state_tidy$state_inital)] = 0
  
#CHECKED - unique(state_tidy$vaccine_type)[-c(1)] == vax_type_list

#Step Five: construct silly array that ODE solver requires
Incidence_inital=(rep(0,J*(T*D+1)*RISK)) 
Exposed_incidence_inital = rep(0,J*2)

state=c(state_tidy$state_inital,Incidence_inital,Exposed_incidence_inital) 

if (round(sum(state)) != sum(pop)){stop('(2) inital state doesnt align with population size!')}



### Use historical cases to estimate rho and R0/Reff (->beta)
if (waning_toggle_rho_acqusition == TRUE ){
  rho_inital = rho_time_step('symptomatic_disease',date_start)
} else{
  rho_inital = 0.95 #Chemaitelly et al. 2 week estimate
}
if (rho_inital > 1){stop('rho is > 1')}

#LIMITATION WARNING: no age-specific susceptibility to infection is included (no delta data available)
source(paste(getwd(),"/(function)_calculate_R0_Reff.R",sep=""))
beta = beta_optimised

R0_beta; beta_check; beta_optimised; beta




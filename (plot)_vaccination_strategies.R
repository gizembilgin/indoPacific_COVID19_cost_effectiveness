
###### Coding vaccine prioritisation strategies

#####(1/3) Toggles #############################################################

vax_strategy = "oldest"
#options: "oldest", "youngest","dose 1", OTHER?

vax_strategy_vaccine_type = "AstraZeneca" 
#options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"  

vax_strategy_num_doses = 200000
#COMEBACK - should have % option

vax_strategy_delivery_timeframe = 180 #(days)
#COMEBACK - should have max # per day option

vax_strategy_start_date = as.Date('2022-01-20')

vax_strategy_max_expected_cov = 0.8 #between 0-1 % of total age group expected to be vaccinated
#_______________________________________________________________________________



#####(2/3) Enact strategy ######################################################
###(A/C) Calculate the eligible population
# = % max poss coverage * pop_size - existing vaccine coverage
eligible_pop = data.frame(pop_setting)

#set max poss coverage
eligible_pop$pop = round(eligible_pop$pop * vax_strategy_max_expected_cov)
colnames(eligible_pop) = c('age_group','eligible_individuals')

#make long by dose
workshop = data.frame()
for (i in 1:num_vax_doses){
  this_dose = as.data.frame(eligible_pop) %>% mutate(dose = i)
  workshop = rbind(workshop,this_dose)
}
eligible_pop= workshop

#remove already vaccinated individuals
existing_coverage = data.frame(eligible_pop$age_group,eligible_pop$dose,rep(0,num_age_groups*num_vax_doses))
colnames(existing_coverage) = c('age_group','dose','cov_to_date')

for (t in 1:num_vax_types){
  for (d in 1:num_vax_doses){
    #need to sum across vaccine_coverage (as this is vaccination_history_FINAL split across age groups)
    existing_coverage$cov_to_date[existing_coverage$dose == d] =
      existing_coverage$cov_to_date[existing_coverage$dose == d] + vaccine_coverage_end_history[(J*(t+(d-1)*T) - J+1):(J*(t+(d-1)*T))]
  }
}

eligible_pop <- eligible_pop %>% left_join(existing_coverage) %>%
  mutate(eligible_individuals = round(eligible_individuals *(1-cov_to_date))) %>%
  select(age_group,dose,eligible_individuals)

eligible_pop



###(B/C) Place priority # on age group by strategy
#lots of {} strategy
#want table with columns: age_group, priority


###(C/C) Distribute doses by priority
#separate by 1/2 dose strategy
#want table with columns: age_group, priority, dose 1, dose 2
#_______________________________________________________________________________



#####(3/3) Distribute between days #############################################

#_______________________________________________________________________________

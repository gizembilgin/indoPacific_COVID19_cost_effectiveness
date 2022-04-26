### This program sets the state of the system to our study location
###
### COMEBACK count = 5


###### (1/4) Vaccination
#(A/B) Coverage 
#(i/iv) Delay
vaccine_coverage_delay_1 = 21 #number of days till protection from first dose, COMEBACK - J&J full protection after 14 days? (single dose vaccine)
vaccine_coverage_delay_2 = 14 #number of days till protection from second dose
vaccine_coverage_delay = c(vaccine_coverage_delay_1,vaccine_coverage_delay_2)


#(ii/iv) #Vaccine coverage at end of true history
multiplier =  sum(pop)/sum(pop[3:num_age_groups])
multiplier = c(0,0,rep(multiplier,J-2)) #COMEBACK - arbitrary uniform distribution of vaccines into age classes 18+ years old
vaccine_coverage_end_history = crossing(dose = c(1:num_vax_doses),
                                        vaccine_type = unique(vaccination_history_TRUE$vaccine_type),
                                        age_group_num = c(1:num_age_groups),
                                        cov = c(0)) 
for (i in 1:J){ # age
  for (t in 1:T){  # vaccine type
    for (d in 1:D){ # vaccine dose
      C = i + J*(t+(d-1)*T) - J
      workshop_type =  unique(vaccination_history_POP$vaccine_type)[t]
      
      if (workshop_type == "Johnson & Johnson" & d == 2){#avoid J&J dose 2, otherwise NA and stuffs up vax_type order
      } else{
        workshop_value =
          vaccination_history_POP$coverage_this_date[
            vaccination_history_POP$date == max(vaccination_history_POP$date) 
            & vaccination_history_POP$dose == d
            & vaccination_history_POP$vaccine_type == workshop_type]/100 * multiplier[i]
        
        vaccine_coverage_end_history$cov[
          vaccine_coverage_end_history$dose == d &
            vaccine_coverage_end_history$vaccine_type == workshop_type &
            vaccine_coverage_end_history$age_group_num == i
        ] = max(workshop_value,0)
      }
    }
  }
}
#COMEBACK - need elegant
vaccine_coverage_end_history = vaccine_coverage_end_history %>% left_join(age_group_order)


#(iii/iv) Add hypothetical campaign (if 'on')
if (vax_strategy_plot == "on"){
  vaccination_history_FINAL = 
    vax_strategy(vax_strategy_start_date        = vax_strategy_toggles$vax_strategy_start_date,
                 vax_strategy_num_doses         = vax_strategy_toggles$vax_strategy_num_doses,
                 vax_strategy_roll_out_speed    = vax_strategy_toggles$vax_strategy_roll_out_speed,
                 vax_age_strategy               = vax_strategy_toggles$vax_age_strategy,            
                 vax_dose_strategy              = vax_strategy_toggles$vax_dose_strategy,            
                 vax_strategy_vaccine_type      = vax_strategy_toggles$vax_strategy_vaccine_type,            
                 vax_strategy_vaccine_interval  = vax_strategy_toggles$vax_strategy_vaccine_interval,            
                 vax_strategy_max_expected_cov  = vax_strategy_toggles$vax_strategy_max_expected_cov
    )

  #recalculate!
  num_vax_doses = D = length(unique(vaccination_history_FINAL$dose))  # dose 1, dose 2, COMEBACK no boosters yet in these settings 
  vax_type_list = sort(unique(vaccination_history_FINAL$vaccine_type))
  num_vax_types = T = length(unique(vaccination_history_FINAL$vaccine_type))
  num_vax_classes = num_vax_doses*num_vax_types + 1                 # + 1 for unvaccinated
  num_total_classes = (num_disease_classes+1)*(num_age_groups*num_vax_classes) #+1 for incidence tracker
  
} else {
  vaccination_history_FINAL = vaccination_history_TRUE
}
if(outbreak_post_rollout == "on"){
  date_start = max(vaccination_history_FINAL$date)
  seed_date = date_start
}




#(iv/iv)  Initial coverage
#<intermission> correcting coverage_this_date variable (previously = total doses delivered/sum(pop))
workshop = pop_setting
colnames(workshop) = c('age_group','pop')

vaccination_history_FINAL = vaccination_history_FINAL %>% left_join(workshop) %>%
    group_by(age_group,vaccine_type,dose) %>%
    mutate(coverage_this_date = 100*cumsum(doses_delivered_this_date)/pop) %>%
    select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date,age_group)

# workshop = vaccination_history_FINAL %>% left_join(workshop) %>%
#   group_by(age_group,vaccine_type,dose) %>%
#   mutate(total = cumsum(doses_delivered_this_date),
#          cov = 100*cumsum(doses_delivered_this_date)/pop)
#workshop = na.omit(workshop)
#workshop = workshop[workshop$age_group == '5-17' & workshop$dose == 2,]
# workshop = workshop[workshop$age_group == '70-100' & workshop$dose == 2,]
# ggplot(workshop) + geom_point(aes(x=date, y=coverage_this_date,color=vaccine_type)) + 
#   geom_line(aes(x=date,y=cov,color=vaccine_type))
#CHECKED: new cov values are higher since doses(age_group)/pop(age_group) > doses(all)/pop(all) since some age groups not vaccinated



#calc
vaccine_coverage = crossing(dose = c(1:num_vax_doses),
                            vaccine_type = unique(vaccination_history_FINAL$vaccine_type),
                            age_group = age_group_labels,
                            cov = c(0)) # CHECKED:vaccine coverage long like E/I/R structure

for (i in 1:J){ # age
  for (t in 1:T){  # vaccine type
    for (d in 1:D){ # vaccine dose
      workshop_type =  unique(vaccination_history_FINAL$vaccine_type)[t]
      workshop_age = age_group_labels[i]
      this_vax_max_date = max(vaccination_history_FINAL$date[vaccination_history_FINAL$vaccine_type == workshop_type])
      
      
      if (workshop_type == "Johnson & Johnson" & d == 2){ #avoid J&J dose 2, otherwise NA and stuffs up vax_type order
      } else{
        if ((date_start - vaccine_coverage_delay[d])<= this_vax_max_date &
            (date_start - vaccine_coverage_delay[d])>= min(vaccination_history_FINAL$date)){
          
          workshop_value =  vaccination_history_FINAL$coverage_this_date[
            vaccination_history_FINAL$date == date_start - vaccine_coverage_delay[d] 
            & vaccination_history_FINAL$age_group == workshop_age
            & vaccination_history_FINAL$dose == d
            & vaccination_history_FINAL$vaccine_type == workshop_type] / 100
          
          vaccine_coverage$cov[
              vaccine_coverage$dose == d &
              vaccine_coverage$vaccine_type == workshop_type &
              vaccine_coverage$age_group == workshop_age
          ] = max(workshop_value,0)
           
        } else if ((date_start - vaccine_coverage_delay[d])> this_vax_max_date){
          workshop_value =
            vaccination_history_FINAL$coverage_this_date[
              vaccination_history_FINAL$date == this_vax_max_date
              & vaccination_history_FINAL$dose == d
              & vaccination_history_FINAL$age_group == workshop_age
              & vaccination_history_FINAL$vaccine_type == workshop_type]/100 
          
          vaccine_coverage$cov[
            vaccine_coverage$dose == d &
              vaccine_coverage$vaccine_type == workshop_type &
              vaccine_coverage$age_group == workshop_age
            ] =  max(workshop_value,0)
        } 
      }
    }
  }
}









#(B/B) Vaccine Effectiveness (VE)
load( file = '1_inputs/VE_waning_distribution.Rdata')
part_one = VE_waning_distribution[VE_waning_distribution$outcome %in% c('any_infection','symptomatic_disease') &
                                    VE_waning_distribution$waning == waning_toggle_acqusition,]
part_two = VE_waning_distribution[VE_waning_distribution$outcome %in% c('severe_disease','death') &
                                    VE_waning_distribution$waning == waning_toggle_severe_outcome,]
VE_waning_distribution = rbind(part_one,part_two)
rm(part_one,part_two)

VE =  crossing(dose = c(1:num_vax_doses),
               vaccine_type = unique(vaccination_history_FINAL$vaccine_type),
               age_group = age_group_labels,
               VE = c(0)) 
if ((date_start - vaccine_coverage_delay[d])>= min(vaccination_history_POP$date)){
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
A=J*(T*D+1) # +1 is unvax

S_inital=E_inital=I_inital=R_inital=Incidence_inital=(rep(0,A))


#(B/F): number of active infected/recovered cases
if (date_start <= max(case_history$date)){
  initialRecovered = case_history %>%
    filter(date == date_start) %>%
    select(recovered) %>%
    as.numeric()
  initialInfected = case_history %>%
    filter(date == date_start) %>%
    select(active) %>%
    as.numeric()
  initialExposed = case_history %>%
    filter(date == date_start+14) %>%
    select(active) %>%
    as.numeric()
}
if (seed>0 & seed_date == date_start) { #overwrite
  initialInfected = seed*AverageSymptomaticPeriod/(AverageSymptomaticPeriod+AverageLatentPeriod) 
  initialExposed  = seed*AverageLatentPeriod/(AverageSymptomaticPeriod+AverageLatentPeriod) 
}
if (date_start > max(case_history$date)){
  initialRecovered = round(pop*seroprev$seroprev/100) #checked- yes age_groups in correct order (untidy)
}

#(C/F): age distribution of cases
#COMEBACK: no data, so assuming uniform across age groups
initialRecovered = round(initialRecovered * pop/sum(pop))
initialInfected = round(initialInfected * pop/sum(pop))
initialExposed = round(initialExposed * pop/sum(pop))



#Step Four: distribute infections among disease classes
for (i in 1:num_age_groups){ #across age classes

  # Step Four A:  split into infected and susceptible
      S_inital[i]  = pop[i]-initialInfected[i] - initialRecovered[i] - initialExposed[i]
      E_inital[i] = initialExposed[i]      
      I_inital[i] = initialInfected[i]  
      R_inital[i] = initialRecovered[i]
}
  
  # Step Four B: distribute across vaccine classes 
  # COMEBACK, assumption infections are spread equally across vax classes
state_TIDY = data.frame()
disease_class_list = list('S','E','I','R')
inital_list = list(S_inital,E_inital,I_inital,R_inital)

for (num in 1:num_disease_classes){
  
  disease_class = inital_list[[num]]
  
  state_tidy = data.frame(disease_class)
  colnames(state_tidy) = c('state_inital')
  state_tidy$class = disease_class_list[[num]]
  state_tidy$temp = seq(1,(num_age_groups*num_vax_classes))
  state_tidy$age_group = rep(age_group_labels,num_vax_classes)
  state_tidy$dose = 0
  state_tidy$vaccine_type = "unvaccinated"
  for (d in 1:num_vax_doses){
    state_tidy$dose[state_tidy$temp %in% c((T*(d-1)+1)*J+1):((T*d+1)*J)] = d
    for (t in 1:num_vax_types){
      state_tidy$vaccine_type[state_tidy$temp %in% c((((t-1)+(d-1)*T+1)*J+1):(((t-1)+(d-1)*T+2)*J))] = vax_type_list[t]
    }
  }
  
  workshop = state_tidy
  
  for (i in 1:J){ # age
    
    #pop*(1-cov1A-cov1B-cov1C)
    state_tidy$state_inital[state_tidy$dose == 0 & state_tidy$age_group == age_group_labels[i]] = 
      workshop$state_inital[workshop$dose == 0 & workshop$age_group == age_group_labels[i]]*
      (1-sum(vaccine_coverage$cov[vaccine_coverage$dose == 1 & vaccine_coverage$age_group == age_group_labels[i]]))
    
    for (t in 1:T){  # vaccine type
      for (d in 1:D){ # vaccine dose

        if (d != D){
          #pop*(cov1A-cov2A)
          state_tidy$state_inital[state_tidy$dose == d & state_tidy$age_group == age_group_labels[i] & state_tidy$vaccine_type == vax_type_list[t]] = 
            workshop$state_inital[workshop$dose == 0 & workshop$age_group == age_group_labels[i]]*
            (vaccine_coverage$cov[vaccine_coverage$dose == d &
                                    vaccine_coverage$vaccine_type == vax_type_list[t] &
                                    vaccine_coverage$age_group == age_group_labels[i]] -
               vaccine_coverage$cov[vaccine_coverage$dose == d+1 &
                                      vaccine_coverage$vaccine_type == vax_type_list[t] &
                                      vaccine_coverage$age_group == age_group_labels[i]])
        }
        if (d == D){
          #pop*cov2A
          state_tidy$state_inital[state_tidy$dose == d & state_tidy$age_group == age_group_labels[i] & state_tidy$vaccine_type == vax_type_list[t]] = 
            workshop$state_inital[workshop$dose == 0 & workshop$age_group == age_group_labels[i]]*
            (vaccine_coverage$cov[vaccine_coverage$dose == d &
                                    vaccine_coverage$vaccine_type == vax_type_list[t]&
                                    vaccine_coverage$age_group == age_group_labels[i]])
        }
      }
    }
  }
  state_TIDY = rbind(state_TIDY,state_tidy)
}    

#Step Five: construct silly array that ODE solver requires
state=c(state_TIDY$state_inital,Incidence_inital) 
sum(state); sum(pop) #CHECK = confirmed equal



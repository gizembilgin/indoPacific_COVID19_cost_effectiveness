### This program sets the state of the system to our study location
###
### COMEBACK count = 5


###### (1/3) Vaccination
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
#Vaccine_effectiveness = VE[age_group,dose_number]
#Load NG day-by-day waning (UK data on AZ and Pfizer)
#distribution for viral (AZ reference) and mRNA (Pfizer reference) for dose 1,2, and 3 (booster is mRNA only)
load(file = "1_inputs/NG_VE_processed.Rdata")
VE_waning_distribution <- VE_together %>%
  mutate(vaccine_type = gsub("AZ", "AstraZeneca", vaccine_type)) %>%
  mutate(vaccine_mode = case_when(
    vaccine_type == 'Pfizer' ~ 'mRNA',
    vaccine_type == 'AstraZeneca' ~ 'viral'
  ))
#Create VE_internal as metric of % max VE for this type
VE_waning_distribution <- VE_waning_distribution%>%
  group_by(outcome,strain,vaccine_mode,dose) %>%
  mutate(VE_internal = ve_predict_mean / max(ve_predict_mean))
VE_waning_distribution <- VE_waning_distribution[,-c(3)] #remove vaccine_type

#Load VE for all vaccines we are interested in  
VE_full_vaccine_type = read.csv("1_inputs/vaccine_effectiveness.csv",header=TRUE)
VE_full_vaccine_type <- VE_full_vaccine_type %>%
  mutate(vaccine_mode = case_when(
    vaccine_type == 'Pfizer' ~ 'mRNA',
    vaccine_type == 'Moderna' ~ 'mRNA',
    vaccine_type == 'AstraZeneca' ~ 'viral',
    vaccine_type == 'Sinopharm' ~ 'viral',
    vaccine_type == 'Sinovac' ~ 'viral',
    vaccine_type == 'Johnson & Johnson' ~ 'viral'
  ))

#COMEBACK - cheeky update
# VE_estimates <- read.csv("1_inputs/VE_WHO_forest_plot.csv",header=TRUE)
# VE_estimates = VE_estimates  %>% select(strain, vaccine_type, dose, outcome,VE)
# sinopharm = VE_estimates[VE_estimates$]



VE_waning_distribution <- VE_full_vaccine_type %>%
  left_join(VE_waning_distribution, by = c("outcome","dose","vaccine_mode")) %>%
  select('strain','outcome','vaccine_mode','vaccine_type','dose','days','VE','VE_internal') %>%
  mutate(VE_days = VE * VE_internal/100)

VE =  crossing(dose = c(1:num_vax_doses),
               vaccine_type = unique(vaccination_history_FINAL$vaccine_type),
               age_group = age_group_labels,
               VE = c(0)) 
if ((date_start - vaccine_coverage_delay[d])>= min(vaccination_history_POP$date)){
  VE = VE_inital = VE_time_step(strain_inital,date_start,'acquisition')
  VE_onwards_inital <- VE_time_step(strain_inital,date_start,'transmission')
}
#___________________________________________________________________



###### (2/3) Seroprevalence
load(file = "1_inputs/seroprev.Rdata")
seroprev = seroprev[seroprev$setting == setting & seroprev$year == 
                      as.numeric(format(date_start, format="%Y")),]
if (as.numeric(format(date_start, format="%Y")) > 2022){
  load(file = "1_inputs/seroprev.Rdata")
  seroprev = seroprev[seroprev$setting == setting & seroprev$year ==  2022,]
}
#___________________________________________________________________



###### (2/3) Hence, initial state
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
S_tidy = data.frame(S_inital)
S_tidy$temp = seq(1,(num_age_groups*num_vax_classes))
S_tidy$age_group = rep(age_group_labels,num_vax_classes)
S_tidy$dose = 0
S_tidy$vaccine_type = "unvaccinated"
for (d in 1:num_vax_doses){
  S_tidy$dose[S_tidy$temp %in% c((T*(d-1)+1)*J+1):((T*d+1)*J)] = d
  for (t in 1:num_vax_types){
    S_tidy$vaccine_type[S_tidy$temp %in% c((((t-1)+(d-1)*T+1)*J+1):(((t-1)+(d-1)*T+2)*J))] = vax_type_list[t]
  }
}

for (i in 1:J){ # age
  for (t in 1:T){  # vaccine type
    for (d in 1:D){ # vaccine dose
      workshop_cov = vaccine_coverage$cov[vaccine_coverage$dose == d &
                                            vaccine_coverage$vaccine_type == vax_type_list[t] &
                                            vaccine_coverage$age_group == age_group_labels[i]]
      
      S_tidy$S_inital[S_tidy$dose == d & S_tidy$age_group == age_group_labels[i] & S_tidy$vaccine_type == vax_type_list[t]] = 
        S_tidy$S_inital[S_tidy$dose == 0 & S_tidy$age_group == age_group_labels[i]] * workshop_cov
      
      S_tidy$S_inital[S_tidy$dose == 0 & S_tidy$age_group == age_group_labels[i]] = 
        S_tidy$S_inital[S_tidy$dose == 0 & S_tidy$age_group == age_group_labels[i]] * (1-workshop_cov)
      }
    }
}
    

#Step Five: construct silly array that ODE solver requires
state=c(S_tidy$S_inital,E_inital,I_inital,R_inital,Incidence_inital) 
sum(state); sum(pop) #CHECK = confirmed equal



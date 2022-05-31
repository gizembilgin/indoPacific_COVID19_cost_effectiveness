### This program runs the model iteratively over weekly periods
### Its purpose is to update the weekly vaccination rates in the population

time_step = 1
num_time_steps = model_weeks *7


parameters = c(
  suscept = suscept,
  behaviour_mod=behaviour_mod,
  uniform_mod = uniform_mod,
  beta=beta,
  NPI=NPI_inital,
  contact_matrix=contact_matrix,
  lota=lota,
  gamma=gamma,
  lambda=lambda,
  delta=delta,
  omega=omega,
  rho=rho_inital,
  age_group_labels=age_group_labels,
  vax_type_list=vax_type_list,
  risk_group_labels = risk_group_labels,
  VE=VE,
  # VE_onwards=VE_onwards,
  num_age_groups=num_age_groups,
  num_risk_groups = num_risk_groups,
  num_disease_classes = num_disease_classes,
  num_vax_types=num_vax_types,
  num_vax_doses=num_vax_doses)


Reff_tracker = data.frame()
rho_tracker_dataframe = data.frame()
VE_tracker_dataframe = data.frame()

for (increments_number in 1:num_time_steps){
#for (increments_number in 1:48){ 
  
  if (increments_number == 1){
    
    sol = as.data.frame(ode(y=state,times=(seq(0,time_step,by=1)),func=covidODE,parms=parameters))
    sol_log <- sol
    sol_log_unedited <- sol
    
    Reff <- NA
    Reff_tracker = rbind(Reff_tracker,Reff)
    colnames(Reff_tracker) <- c('Reff')
    
  } else{
    
    date_now = date_start + increments_number*time_step
    
    if (date_now <= max(NPI_estimates$date)){
      NPI_this_step <- NPI_estimates$NPI[NPI_estimates$date == date_now]/100
      parameters$NPI = NPI_this_step
    } #i.e. assume after end date that NPI constant
    
    if ((date_now - min(vaxCovDelay$delay))>= min(vaccination_history_FINAL$date)){
      parameters$VE = VE_time_step(strain,date_now,'any_infection')
    }
    if (waning_toggle_rho_acqusition == TRUE ){
      parameters$rho = rho_time_step('symptomatic_disease',date_now)
      rho = parameters$rho
    }
    
    # selecting bottom row of solution which is time = 7 (one week)
    state_working=tail.matrix(sol,1)
    state_working=select(state_working,-time) #remove column with time
    state_working=as.vector(state_working)
    
    # lets reconstruct our matrix (easier to work with)
    A=RISK*J*(T*D+1) # +1 is unvax
    
    S = as.matrix(state_working[1:A])
    E = as.matrix(state_working[(A+1):(2*A)])
    I = as.matrix(state_working[(2*A+1):(3*A)])
    R = as.matrix(state_working[(3*A+1):(4*A)])
    Incid = as.matrix(state_working[(4*A+1):(5*A)]) 
    
    
    #back to tidy form!
    prev_state = data.frame()
    class_list = list(S,E,I,R)
    class_name_list = c('S','E','I','R')
    #HERE!!!
    for (i in 1:num_disease_classes){
      workshop = data.frame(t(class_list[[i]]))
      colnames(workshop) = c('pop')
      workshop$class = class_name_list[i]
      workshop$temp = rep(seq(1,(num_age_groups*num_vax_classes)),RISK)
      workshop$age_group = rep(age_group_labels,num_vax_classes*RISK)
      workshop$dose = 0
      workshop$vaccine_type = "unvaccinated"
      for (d in 1:num_vax_doses){
        workshop$dose[workshop$temp %in% c((T*(d-1)+1)*J+1):((T*d+1)*J)] = d
        for (t in 1:num_vax_types){
          workshop$vaccine_type[workshop$temp %in% c((((t-1)+(d-1)*T+1)*J+1):(((t-1)+(d-1)*T+2)*J))] = vax_type_list[t]
        }
      }
      workshop$risk_group = 'general_public'
      if (RISK>1){
        workshop$risk_group[(num_age_groups*num_vax_classes+1):(num_age_groups*num_vax_classes*2)] = risk_group_name
      }
      prev_state = rbind(prev_state,workshop)
    }
    if (round(sum(prev_state$pop))!= sum(pop)){stop('prev state not equal to pop size! (~line 100 in time step)')}
    
    next_state=prev_state # initialise next state
    
    
    ### VACCINATION
for (r in 1:RISK){
  this_risk_group = risk_group_labels[r]   
   for (t in 1:num_vax_types){ #iterating over vaccine types
     this_vax = vax_type_list[t]
     
     this_vax_history = vaccination_history_FINAL[vaccination_history_FINAL$vaccine_type == this_vax & vaccination_history_FINAL$risk_group == this_risk_group,]
     
     # (1/3) recorded vax
     #COMEBACK delay of J&J first does is 21 days, is this right?
     
     VR_this_step = crossing(dose = seq(1:D),
                             age_group = age_group_labels,
                             doses = 0)
     for (d in 1:D){
       for (i in 2:J){ #COMEBACK - could be faster with less for loop, assumption that don't vaccinate 0-4  
         if (nrow(this_vax_history[this_vax_history$date == as.Date(date_now) - vaxCovDelay$delay[vaxCovDelay$dose == d],]) >0){
           VR_this_step$doses[VR_this_step$dose == d & VR_this_step$age_group == age_group_labels[i]] =
             this_vax_history$doses_delivered_this_date[this_vax_history$date ==  as.Date(date_now) - vaxCovDelay$delay[vaxCovDelay$dose == d] & 
                                                                  this_vax_history$dose==d &
                                                                  this_vax_history$age_group == age_group_labels[i]]
         }
       }
     }
     
      for (i in 1:num_age_groups){ # across age groups
        increase = rep(0,num_vax_doses)
        for (d in 1:D){
          increase[d] = VR_this_step$doses[VR_this_step$dose == d & VR_this_step$age_group == age_group_labels[i]] 
        }
        
       for (j in 1:4){ #let's assume all SEIR vaccinated
         class=class_name_list[j]
         
         prop = rep(0,num_vax_doses) #prop in S,E,I or R in vaccine groups
         for (d in 0:D){
           if (d==0){
             prop[d+1] = prev_state$pop[prev_state$class == class & prev_state$risk_group == this_risk_group & prev_state$vaccine_type == "unvaccinated" & prev_state$age_group == age_group_labels[i]]/
               sum(prev_state$pop[prev_state$vaccine_type == "unvaccinated" & prev_state$risk_group == this_risk_group & prev_state$age_group == age_group_labels[i]])
           } else{
             prop[d+1] = prev_state$pop[prev_state$class == class & prev_state$risk_group == this_risk_group & prev_state$vaccine_type == this_vax &  prev_state$dose == d & prev_state$age_group == age_group_labels[i]]/
               sum(prev_state$pop[prev_state$risk_group == this_risk_group & prev_state$vaccine_type == this_vax & prev_state$dose == d & prev_state$age_group == age_group_labels[i]])
           }
           if (is.nan(prop[d+1]) == TRUE){prop[d+1]=0}
         }

         next_state$pop[next_state$class == class & next_state$risk_group == this_risk_group & next_state$vaccine_type == "unvaccinated" & next_state$age_group == age_group_labels[i]] =
           next_state$pop[next_state$class == class & next_state$risk_group == this_risk_group & next_state$vaccine_type == "unvaccinated" & next_state$age_group == age_group_labels[i]] - increase[1]* prop[1]
         
         for (d in 1:(D-1)){
           next_state$pop[next_state$class == class & next_state$risk_group == this_risk_group & next_state$vaccine_type == this_vax & next_state$dose == d & next_state$age_group == age_group_labels[i]] =
             next_state$pop[ next_state$class == class & next_state$risk_group == this_risk_group & next_state$vaccine_type == this_vax & next_state$dose == d & next_state$age_group == age_group_labels[i]] + increase[d]*prop[d]-increase[d+1]*prop[d+1]
         }
         for (d in D){
           next_state$pop[next_state$class == class & next_state$risk_group == this_risk_group & next_state$vaccine_type == this_vax & next_state$dose == D & next_state$age_group == age_group_labels[i]] =
             next_state$pop[next_state$class == class & next_state$risk_group == this_risk_group & next_state$vaccine_type == this_vax & next_state$dose == D & next_state$age_group == age_group_labels[i]] + increase[D] * prop[D]
         }
       }
     }
   }
}
    
    if (seed_date != date_start & seed_date == date_now){
      seed.Infected = seed*AverageSymptomaticPeriod/(AverageSymptomaticPeriod+AverageLatentPeriod)
      seed.Exposed  = seed*AverageLatentPeriod/(AverageSymptomaticPeriod+AverageLatentPeriod)

      #assuming uniform across age groups
      seed.Infected = round(seed.Infected * pop/sum(pop))
      seed.Exposed = round(seed.Exposed * pop/sum(pop))

      for (i in 1:num_age_groups){ # across age groups
        infect = seed.Infected[i]
        expose = seed.Exposed[i]
        
        infect = round(infect * next_state$pop[next_state$class == 'S'  & next_state$age_group == age_group_labels[i]] /sum(next_state$pop[next_state$class == 'S'  & next_state$age_group == age_group_labels[i]]))
        expose = round(expose * next_state$pop[next_state$class == 'S'  & next_state$age_group == age_group_labels[i]] /sum(next_state$pop[next_state$class == 'S'  & next_state$age_group == age_group_labels[i]]))
        
        next_state$pop[next_state$class == 'S'  & next_state$age_group == age_group_labels[i]] = next_state$pop[next_state$class == 'S'  & next_state$age_group == age_group_labels[i]] - infect - expose
        
        next_state$pop[next_state$class == 'I'  & next_state$age_group == age_group_labels[i]] = next_state$pop[next_state$class == 'I'  & next_state$age_group == age_group_labels[i]] + infect
        next_state$pop[next_state$class == 'E'  & next_state$age_group == age_group_labels[i]] = next_state$pop[next_state$class == 'E'  & next_state$age_group == age_group_labels[i]] + expose
        
      }
    }

    
    # if (importation_toggle == "on"){
    #   #Let's assume all importations from lowest vax class
    #    imported_this_date = 3/7
    #    
    #   rand_age = round(runif(1,min=1,max=num_age_groups))
    #   
    #   if (length(imported_this_date) > 0 ){
    #   x=2 #import to exposed or to infected?
    #     if (next_state[1,rand_age] >= imported_this_date ){ #if someone who is Sunvax
    #       next_state[1,rand_age] = next_state[1,rand_age] - imported_this_date
    #       next_state[1+x*4,rand_age] = next_state[1+x*4,rand_age] + imported_this_date
    #     } else if (next_state[2,rand_age] >= imported_this_date ){ #if someone who is Sv1
    #       next_state[2,rand_age] = next_state[2,rand_age] - imported_this_date
    #       next_state[2+x*4,rand_age] = next_state[2+x*4,rand_age] + imported_this_date
    #     } else if (next_state[3,rand_age] >= imported_this_date ){ #if someone who is Sv2
    #       next_state[3,rand_age] = next_state[3,rand_age] - imported_this_date
    #       next_state[3+x*4,rand_age] = next_state[3+x*4,rand_age] + imported_this_date
    #     }
    #   }
    #}
    
    if (round(sum(next_state$pop))!= round(sum(prev_state$pop))){stop('pop not retained between next_state and prev_state!')}
    if (round(sum(next_state$pop))!= sum(pop)) {stop('pop in next_state not equal to setting population')}
    if(nrow(next_state[round(next_state$pop)<0,])>0){ stop('(4)_time_step line 224')}
    
    
    # convert back into silly vector form for ODE solver
    workshop = next_state
    workshop$class = factor(workshop$class, levels = disease_class_list)
    workshop$risk_group = factor(workshop$risk_group, levels = risk_group_labels)
    workshop$vaccine_type = factor(workshop$vaccine_type, levels = vax_type_list)
    workshop$age_group = factor(workshop$age_group, levels = age_group_labels)
    
    next_state = workshop %>% arrange(class,risk_group,dose,vaccine_type,age_group)
    
    S_next=next_state$pop[next_state$class == 'S']
    E_next=next_state$pop[next_state$class == 'E']
    I_next=next_state$pop[next_state$class == 'I']
    R_next=next_state$pop[next_state$class == 'R']
    
    Reff <- Reff_time_step(parameters,next_state)
    Reff_tracker = rbind(Reff_tracker,Reff)

    
    next_state_FINAL=as.numeric(c(S_next,E_next,I_next,R_next,
                                  Incidence_inital,Exposed_incidence_inital)) #setting Incid to repeated 0s
    
    # next week!
    sol <- as.data.frame(ode(y=next_state_FINAL,times=(seq(0,time_step,by=1)),func=covidODE,parms=parameters))
    
    sol[,1]=sol[,1]+time_step*(increments_number-1) #make times correct
    
    sol_log=head(sol_log,-1) #remove last entry from sol_log (overlap of two weekly runs)
    sol_log <- rbind(sol_log,sol)
    sol_log_unedited <- rbind(sol_log_unedited,sol)

    rho_tracker_dataframe = rbind(rho_tracker_dataframe,parameters$rho) 
    
    workshop = parameters$VE
    workshop = workshop[workshop$VE>0,]
    workshop = aggregate(workshop$VE, by=list(category=workshop$dose), FUN=mean)
    colnames(workshop) = c('dose','VE')
    workshop$date = date_now
    VE_tracker_dataframe = rbind(VE_tracker_dataframe,workshop)

### INCIDENCE CALCULATIONS 
J=num_age_groups
T=num_vax_types
D=num_vax_doses
RISK=num_risk_groups
A=RISK*J*(T*D+1) # +1 is unvax

#WEEKLY
#Incid by age and vax class
incidence_log_unedited <- sol_log_unedited[, c(1,(A*num_disease_classes+2):(A*(num_disease_classes+1)+1))]

# select weekly end points 
incidence_log_unedited <- incidence_log_unedited %>% filter (time %% time_step == 0, rowSums(incidence_log_unedited) != time)
incidence_log_unedited <- distinct(round(incidence_log_unedited,digits=2))
incidence_log_unedited$date <- date_start + incidence_log_unedited$time
incidence_log_unedited$daily_cases  <- rowSums(incidence_log_unedited[,2:(A+1)])


incidence_log <- incidence_log_unedited %>% 
  select(date,daily_cases) %>%
  mutate(rolling_average = (daily_cases + lag(daily_cases) + lag(daily_cases,n=2)+lag(daily_cases,n=3)
                            +lag(daily_cases,n=4)+lag(daily_cases,n=5)+lag(daily_cases,n=6))/7) %>%
  mutate(rolling_average_percentage = 100*rolling_average/sum(pop)) %>%
  mutate(cumulative_incidence = cumsum(daily_cases)) %>%
  mutate(cumulative_incidence_percentage = 100*cumsum(daily_cases)/sum(pop))

incidence_log <- cbind(incidence_log,Reff_tracker)

  }
}
#incidence log moved within loop to allow rho_time_step to access

check <- sol_log_unedited
check$Incid = rowSums(sol_log_unedited[,(A*4+2):(A*5+1)])
check$R = rowSums(sol_log_unedited[,(A*3+2):(A*4+1)])
check$I = rowSums(sol_log_unedited[,(A*2+2):(A*3+1)])
check$E = rowSums(sol_log_unedited[,(A+2):(A*2+1)])
check$S = rowSums(sol_log_unedited[,2:(A+1)])

check = check %>% select(S,E,I,R,Incid) %>%
  mutate(pop = S + E + I + R)
if (round(check$pop[1]) !=sum(pop)){stop('ERROR: see line 276 of (4) timestep')}


### INCIDENCE LOG TIDY 
workshop = subset(incidence_log_unedited, select=-c(time,daily_cases))


workshop = pivot_longer(
  workshop,
  cols = paste((num_disease_classes)*(num_age_groups*num_vax_classes)*RISK+1):paste((num_disease_classes+1)*(num_age_groups*num_vax_classes)*RISK),
  names_to = 'temp',
  values_to = 'incidence'
)
workshop$temp = as.numeric(workshop$temp) - (num_disease_classes)*(num_age_groups*num_vax_classes)*RISK


#HERE - ERROR need to align!
workshop2=as.data.frame(unique(workshop$temp)); colnames(workshop2)=c('temp')
if (RISK == 1){
  workshop2 = workshop2 %>%   mutate(temp_risk = temp, risk_group = risk_group_labels[[1]])
} else {
  workshop2 = workshop2 %>%
    mutate(temp_risk = case_when(
      temp <= max(workshop$temp)/2 ~ temp,
      temp > max(workshop$temp)/2  ~ temp - max(workshop$temp)/2 
    ),
    risk_group = case_when(
      temp <= max(workshop$temp)/2 ~ risk_group_labels[[1]],
      temp > max(workshop$temp)/2  ~ risk_group_labels[[2]]
    ))
}
workshop2$age_group = rep(age_group_labels,num_vax_classes) #smallest subdivision is age
workshop2$dose = 0                                          #then dose
workshop2$vaccine_type = "unvaccinated"                     #then vaccine type
for (d in 1:num_vax_doses){
  workshop2$dose[workshop2$temp_risk %in% c((T*(d-1)+1)*J+1):((T*d+1)*J)] = d
  for (t in 1:num_vax_types){
    workshop2$vaccine_type[workshop2$temp_risk %in% c((((t-1)+(d-1)*T+1)*J+1):(((t-1)+(d-1)*T+2)*J))] = vax_type_list[t]
  }
}
#View(workshop2)
#CHECKED: yes aligns as expected


colnames(rho_tracker_dataframe) = c('rho')
rho_tracker_dataframe = cbind(rho = rho_tracker_dataframe, date = incidence_log$date[2:nrow(incidence_log)])


incidence_log_tidy = workshop %>% left_join(workshop2)
incidence_log_tidy = subset(incidence_log_tidy,select=-c(temp))



### EXPOSED LOG TIDY
skip = (num_disease_classes+1)*(num_age_groups*num_vax_classes)
exposed_log = sol_log_unedited %>% 
  select(1, (skip + 2):(skip + 2*J + 1))
exposed_log = exposed_log %>%
  filter (time %% time_step == 0, rowSums(exposed_log) != time) %>%
  mutate(date =  date_start + time)

workshop = subset(exposed_log, select = -c(time))
workshop = pivot_longer(
  workshop,
  cols = colnames(workshop)[1]:colnames(workshop)[2*J],
  names_to = 'temp',
  values_to = 'exposed'
)
workshop$temp = as.numeric(workshop$temp) - skip

workshop2=as.data.frame(unique(workshop$temp)); colnames(workshop2)=c('temp')
workshop2$age_group = rep(age_group_labels,2) #smallest subdivision is age
workshop2 = workshop2 %>% mutate(infection_type = case_when(
  temp <= num_age_groups ~ "new_infection",
  temp > num_age_groups ~ "reinfection"))
#View(workshop2) #CHECKED: yes aligns as expected

exposed_log = workshop %>% left_join(workshop2)
exposed_log_tidy = subset(exposed_log,select=-c(temp))

#reinfection ratio
exposed_log = exposed_log_tidy %>% ungroup() %>% pivot_wider(
  id_cols = c(date,age_group),
  names_from = infection_type,
  values_from = exposed)

exposed_log = exposed_log %>% mutate(reinfection_ratio = reinfection/(new_infection+reinfection))

ggplot(exposed_log) + geom_line(aes(x=date,y=reinfection_ratio,color=as.factor(age_group)))




# rm(workshop2,workshop,check,
#    increase_one,increase_two,
#    sol,
#    S,E,I,R,
#    this_vax_history,this_vax,
#    state_working)  

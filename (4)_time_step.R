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
  rho=rho,
  age_group_labels=age_group_labels,
  vax_type_list=vax_type_list,
  VE=VE,
# VE_onwards=VE_onwards,
  num_age_groups=num_age_groups,
  num_vax_types=num_vax_types,
  num_vax_doses=num_vax_doses)


Reff_tracker = data.frame()
for (increments_number in 1:num_time_steps){
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
  
  if ((date_now - min(vaccine_coverage_delay))>= min(vaccination_history_FINAL$date)){
    parameters$VE = VE_time_step(strain,date_now,'acquisition')
  }
 

  # selecting bottom row of solution which is time = 7 (one week)
  state_working=tail.matrix(sol,1)
  state_working=select(state_working,-time) #remove column with time
  state_working=as.vector(state_working)
  
  # lets reconstruct our matrix (easier to work with)
  A=J*(T*D+1) # +1 is unvax
  
  S = as.matrix(state_working[1:A])
  E = as.matrix(state_working[(A+1):(2*A)])
  I = as.matrix(state_working[(2*A+1):(3*A)])
  R = as.matrix(state_working[(3*A+1):(4*A)])
  Incid = as.matrix(state_working[(4*A+1):(5*A)]) 

  
  prev_state <- as.data.frame(rbind(S,E,I,R))
  row.names(prev_state) <- c("S","E","I","R")
  next_state=prev_state # initialise next state
  
  
  ### VACCINATION
  
  #COMEBACK append hypoth vaccine strategy
  #if (vaccine_strategy_toggle == "on"){
  #  if (date_now >= min(vaccine_strategy$date) & date_now <= max(vaccine_strategy$date)){
  #    
  #  }
  #}
  
  for (t in 1:num_vax_types){ #iterating over vaccine types
    this_vax = vax_type_list[t]
    
    this_vax_history = vaccination_history_FINAL[vaccination_history_FINAL$vaccine_type == this_vax,]
    
    # (1/3) recorded vax
    #COMEBACK delay of J&J first does is 21 days, is this right?
    if (nrow(this_vax_history[this_vax_history$date == as.Date(date_now) - vaccine_coverage_delay_1,]) >0){
      dose_one <- as.numeric(this_vax_history$doses_delivered_this_date[this_vax_history$date==as.Date(date_now) - vaccine_coverage_delay_1 & this_vax_history$dose==1])
    }else { dose_one = rep(0,num_age_groups)}
    if (nrow(this_vax_history[this_vax_history$date == as.Date(date_now) - vaccine_coverage_delay_2,]) >0){
      dose_two <- as.numeric(this_vax_history$doses_delivered_this_date[this_vax_history$date==as.Date(date_now) - vaccine_coverage_delay_2 & this_vax_history$dose==2])
    }  else { dose_two = rep(0,num_age_groups)}
    if (this_vax == "Johnson & Johnson") {dose_two = rep(0,num_age_groups)}

    #NB: no booster dose yet!

    VR_this_step <- cbind(dose_one,dose_two)

    for (i in 1:num_age_groups){ # across age groups
      increase_one = VR_this_step[i,1] 
      increase_two = VR_this_step[i,2] 

      for (j in 1:4){ #let's assume all SEIR vaccinated
        #for (d in 1:D){ #COMEBACK could shorten code with dose  B = i + J*(t+(d-1)*T)

            prop1 = prev_state[j,i]     / sum(prev_state[,i])       # prop unvax in SEIR
            prop2 = prev_state[j,i+J*t] / sum(prev_state[,i+J*t]) # prop dose one

            if (is.nan(prop1) == TRUE){prop1=0}
            if (is.nan(prop2) == TRUE){prop2=0}

            next_state[j,i]         = next_state[j,i] - increase_one*prop1
            next_state[j,i+J*t]     = next_state[j,i+J*t] + increase_one*prop1-increase_two*prop2
            next_state[j,i+J*(t+T)] = next_state[j,i+J*(t+T)] + increase_two * prop2
        #}
      }
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
  #next_state <- round(next_state,digits=0)
  sum(next_state); sum(prev_state); sum(pop) # CHECK = confirmed equal
  
  next_state[next_state<0] <- 0
  
  sum(next_state); sum(prev_state) # CHECK = NOT equal
  
  # convert back into silly vector form for ODE solver
  S_next=next_state[1,]
  E_next=next_state[2,]
  I_next=next_state[3,]
  R_next=next_state[4,]
  
  Reff <- Reff_time_step(parameters,next_state)
  Reff_tracker = rbind(Reff_tracker,Reff)

  next_state_FINAL=as.numeric(c(S_next,E_next,I_next,R_next,
                                Incidence_inital)) #setting Incid to repeated 0s
  
  # next week!
  sol <- as.data.frame(ode(y=next_state_FINAL,times=(seq(0,time_step,by=1)),func=covidODE,parms=parameters))
  
  sol[,1]=sol[,1]+time_step*(increments_number-1) #make times correct
  
  sol_log=head(sol_log,-1) #remove last entry from sol_log (overlap of two weekly runs)
  sol_log <- rbind(sol_log,sol)
  sol_log_unedited <- rbind(sol_log_unedited,sol)
  }
}


### INCIDENCE CALCULATIONS 
J=num_age_groups
T=num_vax_types
D=num_vax_doses
A=J*(T*D+1) # +1 is unvax

  #WEEKLY
  #Incid by age and vax class
  incidence_log_unedited <- sol_log_unedited[, c(1,(A*4+2):(A*5+1))]

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
  
  
  check <- sol_log_unedited
  check$Incid = rowSums(sol_log_unedited[,(A*4+2):(A*5+1)])
  check$R = rowSums(sol_log_unedited[,(A*3+2):(A*4+1)])
  check$I = rowSums(sol_log_unedited[,(A*2+2):(A*3+1)])
  check$E = rowSums(sol_log_unedited[,(A+2):(A*2+1)])
  check$S = rowSums(sol_log_unedited[,2:(A+1)])
  
  check = check %>% select(S,E,I,R,Incid) %>%
    mutate(pop = S + E + I + R)

  #check=check[check$Incid != 0,]
  #check$date = incidence_log$date
  
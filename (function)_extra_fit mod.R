
#### This program fits the COVID-19 transmission model to a specific sero-prevalence over time
#### Note: make sure you've run CommandDeck with the correct STRAIN first!

### Toggles
outbreak_start_date = as.Date('2020-04-01')
seroprev_measured_date = as.Date('2021-03-01')
seroprev_measured_value = 2.6

mod_factor_type = 'behavioural'   
#options: 'uniform', 'behavioural'(proportional to NPI)

interval =c(0.2,0.5) #search range


### Application of toggles
date_start = outbreak_start_date
model_weeks =  round(as.numeric(difftime(seroprev_measured_date,outbreak_start_date,units = "weeks")))+1


### Optimize
time.start=proc.time()[[3]] #let's see how long this runs for
minimise_this <- function(mod_value) {
  
  if (mod_factor_type == 'behavioural'){ #NPI = NPI*(1+behaviour_mod)
    behaviour_mod = mod_value 
    uniform_mod = 1
  } else if (mod_factor_type == 'uniform'){ #beta = beta*behaviour_mod
    behaviour_mod = 0
    uniform_mod = mod_value
  } else {
    warning('no mod_factor')
    behaviour_mod = 0
    uniform_mod = 1
  }
  
  source(paste(getwd(),"/(4)_time_step.R",sep=""))
  
  #COMEBACK: by age group?
  fit = abs(incidence_log$cumulative_incidence_percentage[incidence_log$date == seroprev_measured_date] 
            - seroprev_measured_value)
  
  fit;incidence_log$cumulative_incidence_percentage[incidence_log$date == seroprev_measured_date]
  
  #comparison <- proj_log %>%
  #  left_join(true_log)  %>%
  #  mutate(diff=reported_rolling_average-proj_rolling_average)
  #  fit = sum(comparison$diff^2,na.rm=TRUE)
  
  return(fit)
}

save_optim = optimize(minimise_this,interval)
save_optim

time.end=proc.time()[[3]]
time.end-time.start

#TIME 02/01  - approx. 10 mins (for a single run!)

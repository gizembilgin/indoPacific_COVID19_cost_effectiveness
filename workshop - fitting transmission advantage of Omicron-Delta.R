
#transmission advantage = Reff / Reff

#original beta AT TIME WHEN OMICRON INTRODUCED
# model_weeks = as.numeric(floor((seed_date[2]-date_start)/7))
Reff_delta = Reff_time_step(parameters,next_state) #0.8433571

save = list(parameters$VE$VE,parameters$rho,parameters$beta )

#back to save

  target = 1.56
  
  strain_now = 'omicron'
  parameters$VE = VE_time_step('omicron',date_now,'any_infection')
  parameters$rho = rho_time_step('symptomatic_disease',date_now,'omicron')

  
  Reff_time_step(parameters,next_state)/Reff_delta
  
  fit = abs(TA - target)
  






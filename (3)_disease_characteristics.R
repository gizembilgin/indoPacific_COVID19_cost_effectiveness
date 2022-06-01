### This program sets the disease parameters to the currently circulating strain of COVID-19
### Delta is the strain currently informing these parameters, there is insufficient info on Omicron
### COMEBACK count = 1

strain = strain_inital

### (A/E) Transmission
load(file = "1_inputs/param_age.Rdata")
#COMEBACK uncertainty?
#COMEBACK gamma seems too low!

suscept = param_age$value[param_age$param == 'susceptibility'] # (i) age-specific susceptibility to infection
gamma =param_age$value[param_age$param == 'prop_sympt']        # (ii) proportion of cases symptomatic
lota = 0.5                                                     # (iii) modification factor on infectiousness of asymptomatic cases


### (B/E) Latent period 
if (run_type == "point"){  
  if (strain_inital == 'delta' | strain_inital == 'WT'){
    AverageLatentPeriod = 3.71
  } else if (strain_inital == 'omicron'){
    AverageLatentPeriod = 2.22
  }
}else if (run_type == "rand"){AverageLatentPeriod = rlnorm(1,meanlog = 1.3, sd=0.2)} 

lambda = 1/AverageLatentPeriod


### (C/E) Symptomatic period
if (run_type == "point"){
  if (strain_inital == 'delta' | strain_inital == 'WT'){
    AverageSymptomaticPeriod = 10.9
  } else if (strain_inital == 'omicron'){
    AverageSymptomaticPeriod = 9.87
  }
}else if (run_type == "rand"){AverageSymptomaticPeriod = runif(1,min=7,max=14)} #taken from Zachreson et al., 2021, COMEBACK use ACT bounds

delta = 1/AverageSymptomaticPeriod


### (D/E) Waning of infection-derived immunity
lengthInfectionDerivedImmunity = 365 #days #COMEBACK - need actual value!
omega = 1/lengthInfectionDerivedImmunity

if (waning_toggle_rho_acqusition == TRUE ){
  rho_inital = rho_time_step('symptomatic_disease',date_start)
} else{
  rho_inital = 0.95 #Chemaitelly et al. 2 week estimate
}
if (rho_inital > 1){stop('rho is > 1')}



### (E/E) Beta - fitting parameter


#LIMITATION WARNING: no age-specific susceptibility to infection is included (no delta data available)
source(paste(getwd(),"/(function)_calculate_R0_Reff.R",sep=""))
beta = beta_optimised

R0_beta; beta_check; beta_optimised; beta



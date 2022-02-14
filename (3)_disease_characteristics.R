### This program sets the disease parameters to the currently circulating strain of COVID-19
### Delta is the strain currently informing these parameters, there is insufficient info on Omicron
### COMEBACK count = 1

strain = strain_inital

### (A/E) Transmission
# (i) Contact matrix
#imported in (1)_simulate_setting

# (ii) proportion of cases symptomatic
if (run_type == "point"){gamma=1-0.133
}else if (run_type == "rand"){gamma=1-rnorm(1,mean=0.133,sd=0.0102)}     #confidence interval around Meru's ACT specific analysis
gamma = rep(gamma,num_age_groups)

suscept <- read.csv("1_inputs/susceptibility.csv",header=TRUE)
suscept = suscept$susceptibility
#COME BACK: hard coded for num age groups

# gamma_orig <- read.csv('1_inputs/ACT_probab_sympt.csv')
# gamma = gamma_orig$probab_sympt


#(iii) modification factor on infectiousness of asymptomatic cases
lota = 0.5 #taken from Doherty Institute estimate


### (B/E) Latent period 
if (run_type == "point"){AverageLatentPeriod = 3.71  #Li et al., 2021 (reference used by the Burnett Institute and Zachreson et al., 2021)
}else if (run_type == "rand"){AverageLatentPeriod = rlnorm(1,meanlog = 1.3, sd=0.2)} 

lambda = 1/AverageLatentPeriod


### (C/E) Symptomatic period
if (run_type == "point"){AverageSymptomaticPeriod = 10.5
}else if (run_type == "rand"){AverageSymptomaticPeriod = runif(1,min=7,max=14)} #taken from Zachreson et al., 2021, COMEBACK use ACT bounds

delta = 1/AverageSymptomaticPeriod


### (D/E) Waning of infection-derived immunity
lengthInfectionDerivedImmunity = 180 #days
omega = 1/lengthInfectionDerivedImmunity

load(file = "1_inputs/NG_rho.Rdata")
if (strain_inital == 'delta' | strain_inital == 'WT'){
  rho = mean(immunity_from_infection$ve_predict_mean[immunity_from_infection$outcome == "acquisition" &
                                                       immunity_from_infection$strain == 'delta' & immunity_from_infection$days<=lengthInfectionDerivedImmunity])
} else if (strain_inital == 'omicron'){
  rho = mean(immunity_from_infection$ve_predict_mean[immunity_from_infection$outcome == "acquisition" &
                                                       immunity_from_infection$strain == 'omicron' & immunity_from_infection$days<=lengthInfectionDerivedImmunity])
}
#rho = mean(immunity_from_infection$ve_predict_mean[immunity_from_infection$outcome == "acquisition" &
#                                                    immunity_from_infection$strain == 'delta' & immunity_from_infection$days == 1])

### (E/E) Beta - fitting parameter
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

#LIMITATION WARNING: no age-specific susceptibility to infection is included (no delta data available)
source(paste(getwd(),"/(function)_calculate_R0_Reff.R",sep=""))
beta = beta_optimised

R0_beta; beta_check; beta_optimised; beta



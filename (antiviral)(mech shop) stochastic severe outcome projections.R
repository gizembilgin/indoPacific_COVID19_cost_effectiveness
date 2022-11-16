### This program fits distributions to confidence intervals from literature

### MINIMISE FUNCTIONS #########################################################
fit_normal <- function(mean,LB,UB){
  
  minimise_this_normal <- function(sd) {
    LB_estimate = mean - qnorm(0.975) * sd
    UB_estimate = mean - qnorm(0.025) * sd
    return((LB - LB_estimate)^2 + (UB - UB_estimate)^2) #squared residuals
  }
  
  sd_est = optimize(minimise_this_normal, interval=c(0,mean*2))
  return(sd_est$minimum)
}

fit_lognormal <- function(mean,LB,UB){
  
  minimise_this_lognormal <- function(sd) {
    mean_sq = mean^2
    sd_sq = sd^2
    a = log(mean_sq/sqrt(mean_sq+sd_sq))
    b = log(1+sd_sq/mean_sq)
    
    Z= rlnorm(10000000, meanlog = a,sdlog = b)
    LB_estimate <- as.numeric(quantile(Z,.025))
    UB_estimate <- as.numeric(quantile(Z,.975))
    
    return((LB - LB_estimate)^2 + (UB - UB_estimate)^2) #squared residuals
  }
  
  sd_est = optimize(minimise_this_lognormal, interval=c(0,mean*2))
  sd_est = sd_est$minimum
  
  mean_sq = mean^2
  sd_sq = sd_est^2
  a = log(mean_sq/sqrt(mean_sq+sd_sq))
  b = log(1+sd_sq/mean_sq)

  return(data.frame(lognorm_a=a,lognorm_b=b))
}

fit_beta <- function(mean,LB,UB){
  
  minimise_this_beta <- function(X) {
    a = mean * X
    b = (1-mean) * X
    
    Z= rbeta(10000000, a, b)
    LB_estimate <- as.numeric(quantile(Z,.025,na.rm=TRUE))
    UB_estimate <- as.numeric(quantile(Z,.975,na.rm=TRUE))
    
    return((LB - LB_estimate)^2 + (UB - UB_estimate)^2) #squared residuals
  }
  
  X_estimate = optimize(minimise_this_beta, interval=c(0,100))
  X = X_estimate$minimum
  
  a = mean * X
  b = (1-mean) * X

  return(data.frame(beta_a=a,beta_b=b))
}

fit_gamma <- function(mean,LB,UB){
  
  minimise_this_gamma <- function(param) {
    shape=param[1]
    scale=param[2]
    
    Z= rgamma(10000000, shape, scale=scale)
    return((LB - quantile(Z,.025,na.rm=TRUE))^2 + (mean(Z))^2 + (UB - as.numeric(quantile(Z,.975,na.rm=TRUE))^2))
  }
  
  X_estimate = optim(c(1,1),minimise_this_gamma)
  shape_estimate = X_estimate$par[1]
  scale_estimate = X_estimate$par[1]
  
  return(data.frame(gamma_shape=shape_estimate,gamma_scale=scale_estimate))
}
#_______________________________________________________________________________



### PART ONE: severe_outcome_age_distribution_RAW ______________________________
workshop = read.csv('1_inputs/severe_outcome_age_distribution_RAW.csv')

#remove dodgy 80+ group (see Seedat et al. 2021's discussion)
workshop = workshop %>% 
  rename(mean = rate) %>%
  filter(agegroup != '80+') #remove dodgy age group!
workshop$agegroup[workshop$agegroup == '70 to 79'] = '70 to 100'

#manually overwrite zero values
row_replacement =  workshop[workshop$agegroup == '10 to 19' & workshop$outcome =='critical_disease',] %>% mutate(mean = mean * 1.5, LB = LB * 1.5, UB = UB * 1.5, agegroup = '0 to 9')
workshop[workshop$agegroup == '0 to 9' & workshop$outcome=='critical_disease',] = row_replacement
row_replacement = workshop[workshop$agegroup == '20 to 29' & workshop$outcome=='death',]  %>% mutate(agegroup = '10 to 19')
workshop[workshop$agegroup == '10 to 19' & workshop$outcome=='death',] = row_replacement
row_replacement = workshop[workshop$agegroup == '10 to 19' & workshop$outcome=='death',] %>% mutate(mean = mean * 1.5, LB = LB * 1.5, UB = UB * 1.5, agegroup = '0 to 9')
workshop[workshop$agegroup == '0 to 9' & workshop$outcome=='death',] = row_replacement
#workshop[workshop$rate == 0,] #n=0

lognorm_param = mapply(fit_lognormal, workshop$mean, workshop$LB, workshop$UB)
severe_outcome_age_distribution_RAW_v2 = cbind(workshop,t(lognorm_param)) 
save(severe_outcome_age_distribution_RAW_v2, file = '1_inputs/severe_outcome_age_distribution_RAW_v2.Rdata' )

rm(row_replacement,workshop,lognorm_param)

# check
# sampled_value = mapply(rlnorm,10000000,severe_outcome_age_distribution_RAW_v2$lognorm_a, severe_outcome_age_distribution_RAW_v2$lognorm_b)
# plot(density(sampled_value[,1])); mean(sampled_value[,1]); min(sampled_value[,1])
# plot(density(sampled_value[,5])); mean(sampled_value[,5])
# plot(density(sampled_value[,10])); mean(sampled_value[,10])
# plot(density(sampled_value[,25])); mean(sampled_value[,25])
# plot(density(sampled_value[,26])); mean(sampled_value[,26])
#_______________________________________________________________________________


### PART TWO: severe outcome setting-specific value (severe_outcome_country_level)
# severe_outcome_country_level <- read.csv('1_inputs/severe_outcome_country_level.csv')
# severe_outcome_country_level %>% mutate(diff = (UB-LB)/mean) 
# Let's  use a uniform distribution because CI tight
#_______________________________________________________________________________



### PART THREE: variant-specific multipliers ___________________________________
#WT to Delta multiplier
workshop <- read.csv('1_inputs/severe_outcome_variant_multiplier.csv')
workshop = workshop %>% filter(variant == 'delta')
lognorm_param = mapply(fit_lognormal, workshop$multiplier, workshop$lower_est, workshop$upper_est)
delta_multiplier = cbind(workshop,t(lognorm_param)) 

# sampled_value = mapply(rlnorm,10000000,delta_multiplier$lognorm_a, delta_multiplier$lognorm_b)
# plot(density(sampled_value[,1])); mean(sampled_value[,1]); min(sampled_value[,1])
# plot(density(sampled_value[,2])); mean(sampled_value[,2]); min(sampled_value[,2])
# plot(density(sampled_value[,3])); mean(sampled_value[,3]); min(sampled_value[,3])
# 

#Delta to Omicron multiplier
omicron_multiplier <- read.csv('1_inputs/severe_outcome_variant_multiplier_complex.csv') #omicron vs delta
omicron_multiplier = omicron_multiplier %>% rename(LB=lower_est, UB=upper_est)
# Let's  use a uniform distribution because CI tight

save(delta_multiplier, file = '1_inputs/delta_multiplier.Rdata' )
save(omicron_multiplier, file = '1_inputs/omicron_multiplier.Rdata' )
#_______________________________________________________________________________



### PART FOUR: YLL _____________________________________________________________
#"The average number of remaining years of life expected by a hypothetical cohort of individuals alive at age x 
# who would be subject during the remaining of their lives to the mortality rates of a given period."
# https://population.un.org/wpp/Download/Standard/Mortality/
lifeExpect <- read.csv('1_inputs/UN_life_expectancy_est_v2.csv') #updated 20/10/2022
YLL_FINAL = lifeExpect %>%
  filter(setting == setting,
         year == '2022') %>%
  rename(life_expectancy = medium_variant) %>%
  left_join(pop_estimates, by = 'age') %>%
  select(age,life_expectancy,population) %>%
  mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
  group_by(age_group) %>%
  mutate(group_percent = population/sum(population),
         interim = life_expectancy * group_percent) %>%
  summarise(YLL = sum(interim)) 
save(YLL_FINAL, file = '1_inputs/YLL_FINAL.Rdata' )
#_______________________________________________________________________________



### PART FIVE: RR ______________________________________________________________
RR_sample = data.frame(risk_group = c('pregnant_women','adults_with_comorbidities'),
                       RR = c(2.40,1.95),
                       LB = c(2.25,0.99),
                       UB = c(2.57,3.82))
param_est = mapply(fit_normal, RR_sample$RR, RR_sample$LB, RR_sample$UB)
RR_sample = cbind(RR_sample,sd = param_est) 

# sampled_value = mapply(rnorm,10000000,RR_sample$RR, RR_sample$sd)
# plot(density(sampled_value[,1])); mean(sampled_value[,1]); min(sampled_value[,1])
# plot(density(sampled_value[,2])); mean(sampled_value[,2]); min(sampled_value[,2])
#save(RR_sample,file = '1_inputs/RR_sample.Rdata') #VERIFIED 04/11/2022
#_______________________________________________________________________________



### PART SIX: infection-derived protection against severe outcomes _____________
mean = 0.878
LB = 0.475
UB = 0.971

rho_SO_sample = fit_beta(mean,LB,UB)
# sampled_value = rbeta(10000000,rho_SO_sample$beta_a, rho_SO_sample$beta_b)
# plot(density(sampled_value)); mean(sampled_value); min(sampled_value);max(sampled_value)
# save(rho_SO_sample,file = '1_inputs/rho_SO_sample.Rdata') #VERIFIED 04/11/2022
#_______________________________________________________________________________



### PART SEVEN: Antiviral effectiveness ________________________________________
workshop = read.csv('1_inputs/antiviral_effectiveness.csv')
beta_param = mapply(fit_beta, workshop$mean, workshop$LB, workshop$UB)
antiviral_effectiveness = cbind(workshop,t(beta_param)) 
save(antiviral_effectiveness, file = '1_inputs/antiviral_effectiveness.Rdata' )

rm(workshop,beta_param)

# check
# sampled_value = mapply(rbeta,10000000,antiviral_effectiveness$beta_a, antiviral_effectiveness$beta_b)
# plot(density(sampled_value[,1])); mean(sampled_value[,1]); min(sampled_value[,1])
# plot(density(sampled_value[,2])); mean(sampled_value[,2])
# plot(density(sampled_value[,3])); mean(sampled_value[,3])
# plot(density(sampled_value[,4])); mean(sampled_value[,4])
# plot(density(sampled_value[,5])); mean(sampled_value[,5])
#_______________________________________________________________________________



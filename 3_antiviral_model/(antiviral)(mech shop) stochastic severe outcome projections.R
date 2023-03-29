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
# Let's  use a uniform distribution because CI so tight (LB == mean and UB == mean for some rows)
#_______________________________________________________________________________



### PART THREE: variant-specific multipliers ___________________________________
#WT to Delta multiplier
workshop <- read.csv('1_inputs/severe_outcome_variant_multiplier.csv')
workshop = workshop %>% filter(variant == 'delta')
norm_param = mapply(fit_normal, workshop$multiplier, workshop$lower_est, workshop$upper_est)
delta_multiplier = cbind(workshop,sd = norm_param) 

sampled_value = mapply(rnorm,10000000,delta_multiplier$multiplier, delta_multiplier$sd)
plot(density(sampled_value[,1])); mean(sampled_value[,1]); min(sampled_value[,1])
plot(density(sampled_value[,2])); mean(sampled_value[,2]); min(sampled_value[,2])
plot(density(sampled_value[,3])); mean(sampled_value[,3]); min(sampled_value[,3])

#Delta to Omicron multiplier
omicron_multiplier <- read.csv('1_inputs/severe_outcome_variant_multiplier_complex.csv') #omicron vs delta
workshop = omicron_multiplier
norm_param = mapply(fit_normal, workshop$multiplier, workshop$lower_est, workshop$upper_est)
omicron_multiplier = cbind(workshop,sd = norm_param) 

sampled_value = mapply(rnorm,10000000,omicron_multiplier$multiplier, omicron_multiplier$sd)
sampled_value2 =  mapply(runif,10000000,omicron_multiplier$lower_est, omicron_multiplier$upper_est)
plot(density(sampled_value[,1]));lines(density(sampled_value2[,1]))
plot(density(sampled_value[,2]));lines(density(sampled_value2[,2]))

save(delta_multiplier, file = '1_inputs/delta_multiplier.Rdata' )
save(omicron_multiplier, file = '1_inputs/omicron_multiplier.Rdata' )
#_______________________________________________________________________________



### PART FOUR: YLL _____________________________________________________________
#"The average number of remaining years of life expected by a hypothetical cohort of individuals alive at age x 
# who would be subject during the remaining of their lives to the mortality rates of a given period."
# https://population.un.org/wpp/Download/Standard/Mortality/
load(file = "1_inputs/UN_world_population_prospects/UN_pop_est.Rdata")
load(file = "1_inputs/UN_world_population_prospects/UN_lifeExpect_est.Rdata")
YLL_FINAL = UN_lifeExpect_est %>%
  rename(life_expectancy = ex) %>%
  left_join(UN_pop_est, by = c('AgeGrp','ISO3_code')) %>%
  select(ISO3_code,AgeGrp,life_expectancy,PopTotal) %>%
  mutate(age_group = cut(AgeGrp,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
  group_by(ISO3_code,age_group) %>%
  mutate(group_percent = PopTotal/sum(PopTotal),
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
save(RR_sample,file = '1_inputs/RR_sample.Rdata') #VERIFIED 04/11/2022
#_______________________________________________________________________________



### PART SIX: infection-derived protection against severe outcomes _____________
mean = 0.878
LB = 0.475
UB = 0.971

rho_SO_sample = fit_beta(mean,LB,UB)
# sampled_value = rbeta(10000000,rho_SO_sample$beta_a, rho_SO_sample$beta_b)
# plot(density(sampled_value)); mean(sampled_value); min(sampled_value);max(sampled_value)
save(rho_SO_sample,file = '1_inputs/rho_SO_sample.Rdata') #VERIFIED 04/11/2022
#_______________________________________________________________________________



### PART SEVEN: raw_VE_point_est (dose 1 and 2) ________________________________
raw = read.csv("1_inputs/VE_WHO_forest_plot.csv",header=TRUE)
raw = raw %>% mutate(
  VE = VE/100,
  lower_est = lower_est/100,
  upper_est = upper_est/100
)
workshop = raw[is.na(raw$VE) == FALSE,]
# beta_param = mapply(fit_beta, workshop$VE, workshop$lower_est, workshop$upper_est)
# VE_WHO_est_norm = cbind(workshop,t(beta_param))

norm_param = mapply(fit_normal, workshop$VE, workshop$lower_est, workshop$upper_est)
VE_WHO_est = cbind(workshop,sd = norm_param) 

VE_WHO_est = VE_WHO_est %>% select(strain,vaccine_type,primary_if_booster,dose,outcome,actual.measure,reference,sd)
VE_WHO_est = raw %>% left_join(VE_WHO_est)

save(VE_WHO_est,file = '1_inputs/VE_WHO_est.Rdata')

# check
# sampled_value_beta = mapply(rbeta,100,VE_WHO_est_norm $beta_a, VE_WHO_est_norm$beta_b)
 sampled_value_norm = mapply(rnorm,100,VE_WHO_est$VE, VE_WHO_est$sd)
 sampled_value_unif = mapply(runif,100,min=VE_WHO_est$lower_est, max=VE_WHO_est$upper_est)
# 
 plot(density(sampled_value_norm[,1]));  lines(density(sampled_value_unif[,1]));mean(sampled_value_beta[,1]); mean(sampled_value_norm[,1])
 plot(density(sampled_value_norm[,2]));  lines(density(sampled_value_unif[,2]));mean(sampled_value_unif[,2]); mean(sampled_value_norm[,2])# ; lines(density(sampled_value_beta[,2]));
 plot(density(sampled_value_norm[,3])); lines(density(sampled_value_unif[,3]));mean(sampled_value_unif[,3]); mean(sampled_value_norm[,3]) # ; lines(density(sampled_value_beta[,3]))
# plot(density(sampled_value_norm[,4])); lines(density(sampled_value_beta[,4])); lines(density(sampled_value_unif[,4]));mean(sampled_value_beta[,4]); mean(sampled_value_norm[,4])
# plot(density(sampled_value_norm[,5])); lines(density(sampled_value_beta[,5])); lines(density(sampled_value_unif[,5]));mean(sampled_value_beta[,5]); mean(sampled_value_norm[,5])
#_______________________________________________________________________________



### PART EIGHT: booster_VE_point_est (dose 3) __________________________________
#included in VE_WHO_est
#_______________________________________________________________________________



### PART NINE: raw_VE_severe_outcomes (dn to fit) ______________________________
workshop = read.csv(file = '1_inputs/VE_severe_outcomes.csv',header=TRUE)

# beta_param = mapply(fit_beta, workshop$VE, workshop$LB, workshop$UB)
# SO_pt_est_beta = cbind(workshop,t(beta_param))

norm_param = mapply(fit_normal, workshop$VE, workshop$LB, workshop$UB)
SO_pt_est_norm = cbind(workshop,sd = norm_param) 

VE_severe_outcomes_waning_pt_est = SO_pt_est_norm
save(VE_severe_outcomes_waning_pt_est,file = '1_inputs/VE_severe_outcomes_waning_pt_est.Rdata')

# sampled_value_beta = mapply(rbeta,100000,SO_pt_est_beta $beta_a, SO_pt_est_beta$beta_b)
# sampled_value_norm = mapply(rnorm,100000,SO_pt_est_norm$VE, SO_pt_est_norm$sd)
# sampled_value_unif = mapply(runif,100000,min=SO_pt_est_norm$LB, max=SO_pt_est_norm$UB)
# 
# plot(density(sampled_value_norm[,1])); lines(density(sampled_value_beta[,1])); lines(density(sampled_value_unif[,1]));mean(sampled_value_beta[,1]); mean(sampled_value_norm[,1])
# plot(density(sampled_value_norm[,2])); lines(density(sampled_value_beta[,2])); lines(density(sampled_value_unif[,2]));mean(sampled_value_beta[,2]); mean(sampled_value_norm[,2])
# plot(density(sampled_value_norm[,3])); lines(density(sampled_value_beta[,3])); lines(density(sampled_value_unif[,3]));mean(sampled_value_beta[,3]); mean(sampled_value_norm[,3])
# plot(density(sampled_value_norm[,4])); lines(density(sampled_value_beta[,4])); lines(density(sampled_value_unif[,4]));mean(sampled_value_beta[,4]); mean(sampled_value_norm[,4])
# plot(density(sampled_value_norm[,5])); lines(density(sampled_value_beta[,5])); lines(density(sampled_value_unif[,5]));mean(sampled_value_beta[,5]); mean(sampled_value_norm[,5])
#_______________________________________________________________________________



### PART TEN: Antiviral effectiveness ________________________________________
workshop = read.csv('1_inputs/antiviral_effectiveness.csv')
beta_param = mapply(fit_beta, workshop$mean, workshop$LB, workshop$UB)
antiviral_effectiveness = cbind(workshop,t(beta_param)) 
save(antiviral_effectiveness, file = '1_inputs/antiviral_effectiveness.Rdata' )

rm(workshop,beta_param)

# check
# sampled_value = mapply(rbeta,10000000,antiviral_effectiveness$beta_a, antiviral_effectiveness$beta_b)
# sampled_value2 = mapply(rbeta,10000000,antiviral_effectiveness_prev $beta_a, antiviral_effectiveness_prev $beta_b)
# plot(density(sampled_value[,1])); lines(density(sampled_value2[,1])); mean(sampled_value[,1]); mean(sampled_value2[,1])
# plot(density(sampled_value[,2])); lines(density(sampled_value2[,2])); mean(sampled_value[,2]); mean(sampled_value2[,2])
# plot(density(sampled_value[,3])); lines(density(sampled_value2[,3])); mean(sampled_value[,3]); mean(sampled_value2[,3])
# plot(density(sampled_value[,4])); lines(density(sampled_value2[,4])); mean(sampled_value[,4]); mean(sampled_value2[,4])
# plot(density(sampled_value[,5])); lines(density(sampled_value2[,5])); mean(sampled_value[,5]); mean(sampled_value2[,5])
#_______________________________________________________________________________



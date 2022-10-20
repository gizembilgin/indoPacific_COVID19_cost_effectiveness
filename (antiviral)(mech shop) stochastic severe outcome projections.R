### This program creates all the distributions to sample from



### MINIMISE FUNCTIONS
minimise_this_lognormal <- function(sd) {
  
  mean_sq = mean^2
  sd_sq = sd^2
  
  a = log(mean_sq/sqrt(mean_sq+sd_sq))
  b = log(1+sd_sq/mean_sq)
  
  Z= rlnorm(10000000, meanlog = a,sdlog = b)
  
  LB_estimate <- as.numeric(quantile(Z,.025))
  UB_estimate <- as.numeric(quantile(Z,.975))
  
  (LB - LB_estimate)^2 + (UB - UB_estimate)^2 #squared residuals
}
fit_lognormal <- function(mean,LB,UB){
  
  sd_est = optimize(minimise_this_lognormal, interval=c(0,mean*2))
  sd_est = sd_est$minimum
  
  mean_sq = mean^2
  sd_sq = sd^2
  
  a = log(mean_sq/sqrt(mean_sq+sd_sq))
  b = log(1+sd_sq/mean_sq)
  
  # Z= rlnorm(10000000, meanlog = a,sdlog = b)
  # plot(density(Z))
  # mean(Z)
  # quantile(Z)
  
  result = data.frame(lognorm_a=a,lognorm_b=b)
  return(result)
}

minimise_this_beta <- function(X) {

  a = mean * X
  b = (1-mean) * X
  
  Z= rbeta(10000000, a, b)
  
  LB_estimate <- as.numeric(quantile(Z,.025))
  UB_estimate <- as.numeric(quantile(Z,.975))
  
  (LB - LB_estimate)^2 + (UB - UB_estimate)^2 #squared residuals

}
fit_beta <- function(mean,LB,UB){
  
  X_estimate = optimize(minimise_this_beta, interval=c(0,100))
  X = X_estimate$minimum
  
  a = mean * X
  b = (1-mean) * X
  
  # Z= rbeta(10000000, a,b)
  # plot(density(Z))
  # mean(Z)
  # quantile(Z); quantile(Z,c(.025,.975))
  
  result = data.frame(beta_a=a,beta_b=b)
  return(result)
}
#______________________________________________________________________________________________________________________________________



### PART ONE: severe outcome age distribution (severe_outcome_age_distribution_RAW)_____________________________________________________
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
#____________________________________________________________________________________________________________________________________



### PART TWO: severe outcome setting-specific value (severe_outcome_country_level)___________________________________________________
# severe_outcome_country_level <- read.csv('1_inputs/severe_outcome_country_level.csv')
# severe_outcome_country_level %>% mutate(diff = (UB-LB)/mean) 
# Let's  use a uniform distribution because CI tight
#____________________________________________________________________________________________________________________________________


### PART X: Antiviral effectiveness _________________________________________________________________________________________________
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
#____________________________________________________________________________________________________________________________________



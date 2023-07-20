### This program fits distributions to:
### (1) Operational costs for vaccine dose delivery
### (2) Operational costs for antiviral delivery / cost per outpatient visit
### (3) Reduced length of stay of patients who recieve oral antivirals

fitted_distributions = data.frame()


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
    a = log(mean^2/sqrt(mean^2+sd^2))
    b = sqrt(log(1+(sd^2/mean^2)))
    
    Z= rlnorm(10000000, meanlog = a,sdlog = b)
    LB_estimate <- as.numeric(quantile(Z,.025,na.rm=TRUE))
    UB_estimate <- as.numeric(quantile(Z,.975,na.rm=TRUE))
    mean_estimate <-  mean(Z)
    
    return((LB - LB_estimate)^2 + 
             (mean - mean_estimate)^2 + 
             (UB - UB_estimate)^2)
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
    mean_estimate <-  mean(Z)
    
    return((LB - LB_estimate)^2 + 
             (mean - mean_estimate)^2 + 
             (UB - UB_estimate)^2)
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
    LB_estimate <- as.numeric(quantile(Z,.025,na.rm=TRUE))
    UB_estimate <- as.numeric(quantile(Z,.975,na.rm=TRUE))
    mean_estimate <-  mean(Z)
    
    return((LB - LB_estimate)^2 + 
             (mean - mean_estimate)^2 + 
             (UB - UB_estimate)^2)
  }
  
  X_estimate = optim(c(1,1),minimise_this_gamma)
  shape_estimate = X_estimate$par[1]
  scale_estimate = X_estimate$par[2]
  
  return(data.frame(gamma_shape=shape_estimate,gamma_scale=scale_estimate))
}
#_______________________________________________________________________________



### PART ONE: Operational costs for vaccine dose delivery
mean = 2.85
sd = 2.26
LB = 0.21
UB = 13.04

##norm = mean, sd

##lognormal
mean_sq = mean^2
sd_sq = sd^2
a = log(mean^2/sqrt(mean^2+sd^2))
b = sqrt(log(1+(sd^2/mean^2)))

##gamma
# > shape_estimate
# [1] 0.6223589
# > scale_estimate
# [1] 4.60195

sampled_norm = mapply(rnorm,10000000,mean, sd)
sampled_lognorm = mapply(rlnorm,10000000,a , b)
sampled_gamma = mapply(rgamma,10000000,shape_estimate, 1/scale_estimate)


### check outpatient cost
plot(density(sampled_norm)); mean(sampled_norm); max(sampled_norm); min(sampled_norm)
plot(density(sampled_lognorm[sampled_lognorm<20]));mean(sampled_lognorm); max(sampled_lognorm); min(sampled_lognorm); quantile(sampled_lognorm,probs=c(0.05,0.5,0.95))
plot(density(sampled_gamma[sampled_gamma<20]));mean(sampled_gamma); max(sampled_gamma); min(sampled_gamma); quantile(sampled_gamma,probs=c(0.05,0.5,0.95))
#let's use the lognormal

this_row = data.frame(parameter = "operation_cost_vaccine",
                 setting = NA,
                 mean = mean,
                 distribution = "lognorm",
                 param1 = a,
                 param2 = b,
                 param1_name = "meanlog",
                 param2_name = "sdlog")
fitted_distributions = bind_rows(fitted_distributions,this_row)
#_______________________________________________________________________________



### PART TWO: Operational costs for antiviral delivery / cost per outpatient visit (WHO CHOICE estimates)
load(file = "2_inputs/WHO_CHOICE_2022.Rdata")
workshop = WHO_CHOICE_2022 %>% 
  filter(currency_short == "USD" &
           patient_type == "outpatient" &
           care_setting == "Health centre (no beds)") %>%
  filter(statistic %in% c("model_prediction","UB","LB","SD")) %>%
  pivot_wider(names_from = statistic,values_from=value) 
  
lognorm_param = mapply(fit_lognormal, workshop$model_prediction, workshop$LB, workshop$UB)
workshop = cbind(workshop,t(lognorm_param)) 

gamma_param = mapply(fit_gamma, workshop$model_prediction, workshop$LB, workshop$UB)
workshop = cbind(workshop,t(gamma_param))

#analytical lognorm
a = log(workshop$model_prediction^2/sqrt(workshop$model_prediction^2+workshop$SD^2))
b = sqrt(log(1+(workshop$SD^2/workshop$model_prediction^2)))
workshop = cbind(workshop,a,b)

sampled_norm = mapply(rnorm,10000000,workshop$model_prediction, workshop$SD)
sampled_lognorm = mapply(rlnorm,10000000,workshop$lognorm_a , workshop$lognorm_b)
sampled_gamma = mapply(rgamma,10000000,workshop$gamma_shape, 1/data.frame(workshop$gamma_scale))
sampled_analyatical_lognorm =  mapply(rlnorm,10000000,a , b)


### check outpatient cost
num = 2
workshop[num,c("model_prediction","LB","UB")]
#plot(density(sampled_norm[,num])); quantile(sampled_norm[,num],probs=c(0.05,0.5,0.95)); mean(sampled_norm[,num])
#plot(density(sampled_lognorm[,num])); quantile(sampled_lognorm[,num],probs=c(0.05,0.5,0.95)); mean(sampled_lognorm[,num])
plot(density(sampled_analyatical_lognorm[,num]),xlim = c(0,20)); quantile(sampled_analyatical_lognorm[,num],probs=c(0.05,0.5,0.95)); mean(sampled_analyatical_lognorm[,num])
plot(density(sampled_gamma[,num]),xlim = c(0,20)); quantile(sampled_gamma[,num],probs=c(0.05,0.5,0.95)); mean(sampled_gamma[,num])
#plots so similar! Let's use lognormal again since this is analytically derived and not fitted

rows = workshop %>% 
  rename(setting = ISO3_code,
         mean = model_prediction,
         param1 = a,
         param2= b) %>%
  mutate(parameter = "outpatient_visit_cost",
         distribution = "lognorm",
         param1_name = "meanlog",
         param2_name = "sdlog") %>%
  select(parameter,setting,mean,distribution,param1,param2,param1_name,param2_name)
fitted_distributions = bind_rows(fitted_distributions,rows)
#_______________________________________________________________________________


### PART THREE: Reduced LOS 
mean = 0.784
LB = 0.027
UB = 1.542

norm_est = fit_normal(mean, LB, UB)
gamma_est = fit_gamma(mean,LB,UB)
lognorm_est = fit_lognormal(mean,LB,UB)

sampled_norm =rnorm(10000000,mean, norm_est)
sampled_lognorm = rlnorm(10000000,lognorm_est$lognorm_a , lognorm_est$lognorm_b)
sampled_gamma = rgamma(10000000,gamma_est$gamma_shape, scale = gamma_est$gamma_scale)

plot(density(sampled_norm)); mean(sampled_norm); max(sampled_norm); min(sampled_norm); quantile(sampled_norm,probs=c(0.05,0.5,0.95))
plot(density(sampled_lognorm));mean(sampled_lognorm); max(sampled_lognorm); min(sampled_lognorm); quantile(sampled_lognorm,probs=c(0.05,0.5,0.95))
plot(density(sampled_gamma));mean(sampled_gamma); max(sampled_gamma); min(sampled_gamma); quantile(sampled_gamma,probs=c(0.05,0.5,0.95))

#95% CI of rnorm fits the best
this_row = data.frame(parameter = "reduced_LOS_days",
                 setting = NA,
                 mean = mean,
                 distribution = "norm",
                 param1 = mean,
                 param2 = norm_est,
                 param1_name = "mean",
                 param2_name = "sd")
fitted_distributions = bind_rows(fitted_distributions,this_row)
#_______________________________________________________________________________

save(fitted_distributions,file = "2_inputs/fitted_distributions.Rdata")

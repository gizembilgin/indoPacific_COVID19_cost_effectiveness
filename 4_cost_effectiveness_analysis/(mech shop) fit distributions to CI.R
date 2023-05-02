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



### PART ONE: WHO CHOICE estimates _____________________________________________
load(file = "2_inputs/WHO_CHOICE_2022.Rdata")
workshop = WHO_CHOICE_2022 %>% 
  filter(statistic %in% c("model_prediction","UB","LB","SD")) %>%
  pivot_wider(names_from = statistic,values_from=value) %>%
  filter(currency_short == "USD")
  
lognorm_param = mapply(fit_lognormal, workshop$model_prediction, workshop$LB, workshop$UB)
workshop = cbind(workshop,t(lognorm_param)) 

gamma_param = mapply(fit_gamma, workshop$model_prediction, workshop$LB, workshop$UB)
workshop = cbind(workshop,t(gamma_param))

sampled_norm = mapply(rnorm,10000000,workshop$model_prediction, workshop$SD)
sampled_lognorm = mapply(rlnorm,10000000,workshop$lognorm_a , workshop$lognorm_b)
sampled_gamma = mapply(rgamma,10000000,workshop$gamma_shape, workshop$gamma_scale)


### check outpatient cost
num = 4 #4,12,20,28
workshop[num,c("model_prediction","LB","UB")]
plot(density(sampled_norm[,num])); quantile(sampled_norm[,num],probs=c(0.05,0.5,0.95))
plot(density(sampled_lognorm[,num])); quantile(sampled_lognorm[,num],probs=c(0.05,0.5,0.95))
plot(density(sampled_gamma[,num])); quantile(sampled_gamma[,num],probs=c(0.05,0.5,0.95))

#_______________________________________________________________________________


### PART TWO: Reduced LOS 
mean = 0.784
LB = 0.027
UB = 1.542

norm_est = fit_normal(mean, LB, UB)
gamma_est = fit_gamma(mean,LB,UB)
lognorm_est = fit_lognormal(mean,LB,UB)
#_______________________________________________________________________________

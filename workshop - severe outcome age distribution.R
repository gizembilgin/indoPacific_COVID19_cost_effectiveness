
### Pop distribution
#(A) by 5 year age bands
age_groups = c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,110)
age_group_labels = c('0-4','5 to 9','10 to 14','15 to 19','20 to 24','24 to 29','30 to 34','35 to 39','40 to 44','45 to 49',
                     '50 to 54','55 to 59','60 to 64','65 to 69','70 to 74','75 to 79','80 to 84','85 to 89','90 to 94','95 to 100')

pop_5_bands <- read.csv("C:/Users/gizem/Documents/PhD/Research/2_scarce_COVID_vaccine_supply/4_code/inputs/pop_estimates.csv", header=TRUE)
pop_5_bands <- pop_5_bands[pop_5_bands$country == setting,]
pop_5_bands <- pop_5_bands %>%
  mutate(agegroup = cut(age,breaks = age_groups, include.lowest = T,labels = age_group_labels)) 
pop_5_bands <- aggregate(pop_5_bands$population, by=list(category=pop_5_bands$agegroup), FUN=sum)
colnames(pop_5_bands) <-c('agegroup','pop')

pop_5_bands <- pop_5_bands %>% mutate(pop_percentage = pop/sum(pop_5_bands$pop))  
  
#(B) by model age bands
age_groups = c(0,4,19,29,39,49,59,110)
age_group_labels = c('0-4','5-19','20-29','30-39','40-49','50-59','60-100')

pop_model <- read.csv("C:/Users/gizem/Documents/PhD/Research/2_scarce_COVID_vaccine_supply/4_code/inputs/pop_estimates.csv", header=TRUE)
pop_model <- pop_model[pop_model$country == setting,]
pop_model <- pop_model %>%
  mutate(agegroup = cut(age,breaks = age_groups, include.lowest = T,labels = age_group_labels)) 
pop_model <- aggregate(pop_model$population, by=list(category=pop_model$agegroup), FUN=sum)
colnames(pop_model) <-c('agegroup','pop')

pop_model <- pop_model %>% mutate(pop_percentage = pop/sum(pop_model$pop))  
######


### Shape of risk
# risk_df <- data.frame(x=seq(from =5,to=100,by=5))
# risk_df <- risk_df %>% mutate(y=0.0009*exp(0.1139*x)) #fit from excel
# risk_df = rbind(c(0,0.01),risk_df) #add back 0-4 which doesn't fit in trend



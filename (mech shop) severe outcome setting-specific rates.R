#doco explanation:
# We used country-specific severity estimates to project from cumulative incidence to 
# incidence of severe disease, acute-care bed hospitalisation and deaths [39]. 
# We then adjusted these wild-type estimates by variant- and age-specific multipliers [40-42].
# Further, we included vaccine effectiveness against severe outcomes.

# COMEBACK - could include d'n in both country-specific estimate, variant multiplier, VE, age distribution
# COMEBACK - what parts of this script should be in the function?

# NOTE - this will be incidence of hosp NOT occupancy, the WHO defines severe disease as 
# "a patient with severe acute respiratory illness (fever and at least one sign or symptom of respiratory disease), AND requiring hospitalization
# Hence, hosp - severe = unmet need!
time.start=proc.time()[[3]]


VOC = strain_inital


##### (1/7) Load population-level wild-type estimate of severe outcomes
severe_outcome_0 <- read.csv('1_inputs/severe_outcome_country_level.csv')
severe_outcome_0$percentage = severe_outcome_0$percentage/100 #make it between 0-1
severe_outcome_0 <- severe_outcome_0[severe_outcome_0$outcome %in% c('death','severe_disease','hosp') &
                                       severe_outcome_0$country == setting
                                     ,-c(1,5)] #dropping ICU and ICR as we won't use them, removing source and country column
#_______________________________________________________________________________


if (VOC != 'WT'){
  #####(2/7) Load variant-specific multipliers
  #these multipliers are WT to VOC
  
  workshop <- read.csv('1_inputs/severe_outcome_variant_multiplier.csv')
  ## if stochastic then multiplier = rand in uniform(lower_est,upper_est)
  
  #<interlude for omicron>
  workshop2 <- read.csv('1_inputs/severe_outcome_variant_multiplier_complex.csv') #omicron vs delta
  omicron_basis = workshop[workshop$variant == 'delta',]
  omicron_basis$variant = 'omicron'
  omicron_basis$source = paste(omicron_basis$source,'/',workshop2$source)
  omicron_basis <- omicron_basis %>%
    mutate(multiplier = case_when(
      outcome == 'hosp' ~ multiplier*workshop2$multiplier[workshop2$outcome == 'hosp'],
      outcome %in% c('ICU','death') ~ multiplier*workshop2$multiplier[workshop2$outcome == 'hosp_long']))
  #NOTE: assumption here that hosp_long proportional to ICU and death
  #NOTE: upper and lower limits not adjusted as assumed used above
  workshop = rbind(workshop,omicron_basis)
  #<fin>
  
  workshop = workshop[workshop$variant == VOC,c('outcome','multiplier')]
  #_______________________________________________________________________________
  
  
  
  #####(3/7) Calculating population-level variant-specific estimate of severe outcomes
  #could be made faster, but assumptions less obvious, by including a var that is var_proxy to join on
  severe_outcome_1 <- severe_outcome_0 %>%
    mutate(percentage = case_when(
      outcome == 'death' ~ percentage * workshop$multiplier[workshop$outcome == 'death'],
      outcome == 'severe_disease' ~ percentage * workshop$multiplier[workshop$outcome == 'ICU'], #assumption
      outcome == 'hosp' ~ percentage * workshop$multiplier[workshop$outcome == 'hosp']
    ),variant=VOC)
  rm (omicron_basis,workshop2)
  
} else if (VOC == 'WT'){
  severe_outcome_1 = severe_outcome_0 %>% mutate(variant = VOC)
}
#_______________________________________________________________________________



#####(4/7) Calculating age-specific estimates of severe outcomes
load(file = '1_inputs/severe_outcome_age_distribution.Rdata') #adjusted values from Qatar
workshop = age_dn_severe_outcomes
workshop = workshop[workshop$setting == setting,]

#dummy values based on log-linear relationship
#workshop <- read.csv('1_inputs/severe_outcome_age_distribution.csv')
#workshop <- workshop[,c(1,2,3)] #remove source and explanation columns

severe_outcome_2 <- severe_outcome_1 %>%  left_join(workshop)
severe_outcome_2 <- severe_outcome_2 %>% mutate(percentage=percentage*RR)
severe_outcome_FINAL <- severe_outcome_2 %>%
  select(outcome,outcome_long,age_group,percentage) 

rm(severe_outcome_0,severe_outcome_2)
#_______________________________________________________________________________



#####(5/7) Calculating YLL from death
#requires average age in age-group and life_expectancy of this study setting
workshop <- pop_setting_orig %>%
  mutate(agegroup = cut(age,breaks = age_groups, include.lowest = T,labels = age_group_labels)) 

workshop_sum <- aggregate(workshop$population, by=list(category=workshop$agegroup), FUN=sum)
colnames(workshop_sum) <-c('agegroup','pop')

workshop <- workshop %>% left_join(workshop_sum)
workshop <- workshop %>% mutate(weight=population/pop,
                                age_weight = (age+0.5)*weight)

workshop <- aggregate(workshop$age_weight, by=list(category=workshop$agegroup), FUN=sum)
colnames(workshop) <-c('agegroup','average_age')

lifeExpect <- read.csv('1_inputs/UN_life_expectancy_est.csv')
lifeExpect = lifeExpect[lifeExpect$setting == setting,]

closest_age = data.frame()
for (i in 1:nrow(workshop)){
  closest_age = rbind(closest_age,lifeExpect$age[which.min(abs(lifeExpect$age-workshop$average_age[i]))])
}
colnames(closest_age) = c('age')

workshop = cbind(workshop,closest_age)

workshop <- workshop %>%
  left_join(lifeExpect) %>%
  select(agegroup,average_age,age,life_expectancy)
#"The average number of remaining years of life expected by a hypothetical cohort of individuals alive at age x 
# who would be subject during the remaining of their lives to the mortality rates of a given period."
# https://population.un.org/wpp/Download/Standard/Mortality/

YLL_FINAL = workshop %>%
  select(agegroup,life_expectancy) 
colnames(YLL_FINAL) = c('age_group','YLL')

#apply discounting using continuous approach, as per larson et al.
if (discounting_rate >0){YLL_FINAL$life_expectancy = (1/discounting_rate)*(1-exp(-discounting_rate*YLL_FINAL$life_expectancy ))}


YLL_row = severe_outcome_FINAL[severe_outcome_FINAL$outcome == 'death',]
YLL_row$outcome = 'YLL'
YLL_row$outcome_long = 'YLL per death in this age_group multiplied by death rate'
YLL_row <- YLL_row %>% left_join(YLL_FINAL) %>%
  mutate(percentage = percentage*YLL)
YLL_row = YLL_row[,c(1:4)]

severe_outcome_FINAL = rbind(severe_outcome_FINAL,YLL_row)

severe_outcome_FINAL = severe_outcome_FINAL %>%
  mutate(outcome_VE = case_when(
    outcome %in% c('death','YLL') ~ 'death',
    outcome %in% c('hosp','severe_disease') ~ 'severe_disease'
  ))

ggplot() + 
  geom_point(data=severe_outcome_FINAL[severe_outcome_FINAL$outcome != 'YLL',],
             aes(x=factor(age_group,level=age_group_labels),
                                           y=percentage,color=as.factor(outcome)),na.rm=TRUE) +
  xlab('age group') +
  labs(color='outcome') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))
#_______________________________________________________________________________
time.end=proc.time()[[3]]
time.end-time.start 


save(severe_outcome_FINAL, file = "1_inputs/severe_outcome_FINAL.Rdata")
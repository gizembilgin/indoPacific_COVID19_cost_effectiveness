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
# COMEBACK - these are dummy values, and NOT country-specific
workshop <- read.csv('1_inputs/severe_outcome_age_distribution.csv')
workshop <- workshop[,c(1,2,3)] #remove source and explanation columns

severe_outcome_2 <- severe_outcome_1 %>%  left_join(workshop)
severe_outcome_2 <- severe_outcome_2 %>% mutate(percentage=percentage*RR)
severe_outcome_FINAL <- severe_outcome_2 %>%
  select(outcome,outcome_long,age_group,percentage) %>%
  mutate(age_group = gsub(" to ", "-", age_group)) 
severe_outcome_FINAL$age_group[severe_outcome_FINAL$age_group == '60+'] = '60-100'

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
    outcome %in% c('hosp','severe_disease') ~ 'hospitalisation'
  ))

#_______________________________________________________________________________



#####(6/7) Multiplying severe outcomes by VE
#(A/B) calculate VE against severe outcomes by day
VE_tracker = data.frame()
for (outcome in c('death','hospitalisation')){
  for (day in 1:(model_weeks*7) ){
    workshop = VE_time_step(strain_inital,date_start+day,outcome)
    workshop = workshop %>% mutate(date=day,
                                   outcome_VE=outcome)
    VE_tracker = rbind(VE_tracker,workshop)
  }
}
VE_tracker$date = date_start + VE_tracker$date 

workshop = crossing(dose = 0,
                    vaccine_type = "unvaccinated",
                    age_group = age_group_labels,
                    VE = 0,
                    date = unique(VE_tracker$date),
                    outcome_VE=unique(VE_tracker$outcome))
VE_tracker = rbind(VE_tracker,workshop)

#(B/B) calculate severe outcome incidence by vax_status
severe_outcome_FINAL = severe_outcome_FINAL %>% left_join(VE_tracker) %>%
  mutate(percentage = percentage*(1-VE)) %>%
  select(date,outcome,outcome_long,age_group,vaccine_type,dose,percentage)
#_______________________________________________________________________________



#####(7/7) Genuine projection from incidence!
severe_outcome_proj <- function(incidence_log_unedited){
  
  #(A/D) Join incidence_log_tidy with severe outcome incidence by vax status
  workshop = incidence_log_tidy %>%
    left_join(severe_outcome_FINAL) %>%
    mutate(proj = incidence*percentage) #calculate incidence -> severe outcome
  if(!nrow(severe_outcome_FINAL) == nrow(workshop)){stop('something has gone amiss')}
  
  #(B/D) Sum across age groups, doses and vaccination status to get overall severe incidence per day
  workshop = workshop %>%
    group_by(date,outcome) %>%
    summarise(proj=sum(proj))
  
  #(C/D) Add cases as an outcome
  workshop_incid =  incidence_log_unedited[,c('date','daily_cases')] %>%
    mutate(outcome ='cases',proj = daily_cases) %>%
    select(date,outcome,proj)
  workshop = rbind(workshop,workshop_incid)
  
  #(D/D) Calculate cumulative severe outcomes by outcome type
  severe_outcome_projections = workshop %>%
    group_by(outcome) %>%
    mutate(proj_cum = cumsum(proj))
  

  plot1 <- 
    ggplot() + 
    geom_line(data=severe_outcome_projections[severe_outcome_projections$outcome != 'cases',],aes(x=date,y=proj,color=as.factor(outcome)),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    #ylim(0,40) +
    ylab("incidence") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  
  plot2 <- ggplot() + 
    geom_line(data=severe_outcome_projections[severe_outcome_projections$outcome != 'cases',],aes(x=date,y=proj_cum,color=as.factor(outcome)),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    ylab("cumulative incidence") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  
  grid.arrange(plot1, plot2)
  
  #create row for table comparing vaccine strategies
  row = severe_outcome_projections %>% 
    filter(date == max(severe_outcome_projections$date)) %>%
    select(-c(proj,date)) %>%
    pivot_wider(names_from=outcome,
                values_from=proj_cum) 
  
   return(row) #COMEBACK - do we need to return severe_outcome_projections instead?
}





  

  
  
  
  
  


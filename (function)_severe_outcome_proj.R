#doco explanation:
# We used country-specific severity estimates to project from cumulative incidence to 
# incidence of severe disease, acute-care bed hospitalisation and deaths [39]. 
# We then adjusted these wild-type estimates by variant- and age-specific multipliers [40-42].

# COMEBACK - could include d'n in both pop estimate and variant multiplier
# COMEBACK - need to apply VE against severe outcome
# NOTE - this will be incidence of hosp NOT occupancy

VOC = strain_inital


##### (1/6) Load population-level wild-type estimate of severe outcomes
severe_outcome_0 <- read.csv('1_inputs/severe_outcome_country_level.csv')
severe_outcome_0 <- severe_outcome_0[severe_outcome_0$outcome %in% c('death','severe_disease','hosp') &
                                       severe_outcome_0$country == setting
                                     ,-c(1,5)]
#dropping ICU and ICR as we won't use them, removing source and country column
#_______________________________________________________________________________


if (VOC != 'WT'){
#####(2/6) Load variant-specific multipliers
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



#####(3/6) Calculating population-level variant-specific estimate of severe outcomes
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



#####(4/6) Calculating age-specific estimates of severe outcomes
# COMEBACK - these are dummy values, and NOT country-specific
workshop <- read.csv('1_inputs/severe_outcome_age_distribution.csv')
workshop <- workshop[,c(1,2,3)] #remove source and explanation columns

severe_outcome_2 <- severe_outcome_1 %>%  left_join(workshop)
severe_outcome_2 <- severe_outcome_2 %>% mutate(percentage=percentage*RR)
severe_outcome_FINAL <- severe_outcome_2 %>%
  select(outcome,outcome_long,age_group,percentage)

outcomes_list <- unique(severe_outcome_FINAL$outcome)

rm(severe_outcome_0,severe_outcome_2)
#_______________________________________________________________________________



#####(5/6) Calculating YLL from death
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

workshop <- workshop %>% mutate(
  closest = min(abs(rep(average_age,nrow(lifeExpect))-lifeExpect$age))
)

#_______________________________________________________________________________



#####(6/6) Geunine proj from incidence!

severe_outcome_proj <- function(incidence_log_unedited){
  outcome_proj = incidence_log_unedited[,c('date','daily_cases')]
  
  workshop = subset(incidence_log_unedited, select=-c(time,date,daily_cases))
  
  for (i in 1:length(outcomes_list)){ #do for every outcome
    outcome = outcomes_list[i]
    workshop_temp = workshop
    this_health_outcome = severe_outcome_FINAL[severe_outcome_FINAL$outcome == outcome,c('percentage')]
    this_health_outcome = this_health_outcome/100 #from % -> between 0-1
    
    #COMEBACK - would apply VE but for now
    this_health_outcome = rep(this_health_outcome,num_vax_classes)
    
    for (i in 1:nrow(workshop)){
      #COMEBACK - could be faster!
      workshop_temp[i,] = workshop[i,] * this_health_outcome
    }
    
    outcome_result = as.data.frame(rowSums(workshop_temp))
    colnames(outcome_result) = outcome
    
    outcome_proj = cbind(outcome_proj,outcome_result)
  }
  
  outcome_proj_cum = outcome_proj
  for (i in 2:ncol(outcome_proj)){
    outcome_proj_cum[,i] = round(cumsum(outcome_proj_cum[,i]),digits=4)
  }
  
  outcome_proj_cum_long = data.frame()
  for (i in 2:ncol(outcome_proj_cum)){
    interim = outcome_proj_cum[,c(1,i)]
    colnames(interim) = c('date','proj')
    interim$outcome = colnames(outcome_proj_cum)[i]
    outcome_proj_cum_long = rbind(outcome_proj_cum_long,interim)
  }
  
  outcome_proj_long = data.frame()
  for (i in 2:ncol(outcome_proj)){
    interim = outcome_proj[,c(1,i)]
    colnames(interim) = c('date','proj')
    interim$outcome = colnames(outcome_proj)[i]
    outcome_proj_long = rbind(outcome_proj_long,interim)
  }
  
  plot1 <- 
    ggplot() + 
    geom_line(data=outcome_proj_long,aes(x=date,y=proj,color=as.factor(outcome)),na.rm=TRUE) +
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
    geom_line(data=outcome_proj_cum_long,aes(x=date,y=proj,color=as.factor(outcome)),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    ylab("cumulative incidence") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  
  grid.arrange(plot1, plot2)
  
  plot1 <- 
    ggplot() + 
    geom_line(data=outcome_proj_long[outcome_proj_long$outcome != 'daily_cases',],aes(x=date,y=proj,color=as.factor(outcome)),na.rm=TRUE) +
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
    geom_line(data=outcome_proj_cum_long[outcome_proj_cum_long$outcome != 'daily_cases',],aes(x=date,y=proj,color=as.factor(outcome)),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    ylab("cumulative incidence") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  
  grid.arrange(plot1, plot2)

}


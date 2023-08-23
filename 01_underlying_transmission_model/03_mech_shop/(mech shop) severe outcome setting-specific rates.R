### This (mech shop) creates setting-specific estimates of severe outcomes. 
# COMEBACK - could include uncertainty in both country-specific estimate and variant multiplier.
# NOTE - this includes the incidence of hospital admission NOT occupancy, the WHO defines severe disease as - 
# "a patient with severe acute respiratory illness (fever and at least one sign or symptom of respiratory disease), AND requiring hospitalization"
# Hence, hosp - severe = unmet need!

discounting_rate = 0

##### (1/7) Load population-level wild-type estimate of severe outcomes
severe_outcome_country_level <- read.csv('01_inputs/severe_outcome_country_level.csv')

#Note, the paper includes no estimates for nations < 1 million, 
#hence estimating Fiji <-> Indonesia and Solomon Islands <-> PNG as closest in pop over 65 and HAQ Index in the region
severe_outcome_country_level = severe_outcome_country_level %>% filter(! country %in% c('FJI','SLB')) #remove NAs
workshop = severe_outcome_country_level %>% 
  filter(country %in% c('PNG','IDN')) %>%
  mutate(country = case_when (country == 'PNG' ~ 'SLB', country == 'IDN' ~ 'FJI'),
         country_long  = case_when (country == 'SLB' ~ 'Solomon Islands', country == 'FJI' ~ 'Fiji'))
severe_outcome_country_level = rbind(severe_outcome_country_level,workshop)

severe_outcome_country_level$percentage = severe_outcome_country_level$percentage/100 #make it between 0-1
save(severe_outcome_country_level,file = "01_inputs/severe_outcome_country_level.Rdata")

severe_outcome_0 = severe_outcome_country_level
#_______________________________________________________________________________


#####(2/7) Load variant-specific multipliers
workshop <- read.csv('01_inputs/severe_outcome_variant_multiplier.csv')
#<interlude for omicron>
workshop2 <- read.csv('01_inputs/severe_outcome_variant_multiplier_complex.csv') #omicron vs delta
omicron_basis = workshop[workshop$variant == 'delta',]
omicron_basis$variant = 'omicron'
omicron_basis$source = paste(omicron_basis$source,'/',workshop2$source)
omicron_basis <- omicron_basis %>%
  mutate(multiplier = case_when(
    outcome == 'hosp' ~ multiplier*workshop2$multiplier[workshop2$outcome == 'hosp'],
    outcome %in% c('ICU','death') ~ multiplier*workshop2$multiplier[workshop2$outcome == 'hosp_long']))
#ASSUMPTION: hosp_long proportional to ICU and death
variant_multiplier = rbind(workshop,omicron_basis)
#<fin>

severe_outcome_FINAL = data.frame()

for (VOC in c('omicron')){ #since we are only considering severe outcomes during the circulation of Omicron
  if (VOC != 'WT'){

    workshop = variant_multiplier[variant_multiplier$variant == VOC,c('outcome','multiplier')]
    #_______________________________________________________________________________
    
    #####(3/7) Calculating population-level variant-specific estimate of severe outcomes
    #could be made faster, but the assumptions were are making would be less obvious
    severe_outcome_1 <- severe_outcome_0 %>%
      mutate(percentage = case_when(
        outcome == 'death' ~ percentage * workshop$multiplier[workshop$outcome == 'death'],
        outcome %in% c('severe_disease','ICU','critical_disease') ~ percentage * workshop$multiplier[workshop$outcome == 'ICU'], #ASSUMPTION
        outcome == 'hosp' ~ percentage * workshop$multiplier[workshop$outcome == 'hosp']
      ),variant=VOC)
    
  } else if (VOC == 'WT'){
    severe_outcome_1 = severe_outcome_0 %>% mutate(variant = VOC)
  }
  #_______________________________________________________________________________
  
  
  
  #####(4/7) Calculating age-specific estimates of severe outcomes
  load(file = '01_inputs/severe_outcome_age_distribution.Rdata') #adjusted values from Qatar

  severe_outcome_2 <- severe_outcome_1 %>%  
    left_join(age_dn_severe_outcomes) %>% mutate(percentage=percentage*RR)
  
  severe_outcome_3 <- severe_outcome_2 %>%
    select(country,outcome,outcome_long,age_group,percentage) 
  
  rm(severe_outcome_2)
  #_______________________________________________________________________________
  
  
  
  #####(5/7) Calculating YLL from death
  #"The average number of remaining years of life expected by a hypothetical cohort of individuals alive at age x 
  # who would be subject during the remaining of their lives to the mortality rates of a given period."
  # https://population.un.org/wpp/Download/Standard/Mortality/
  load(file = "01_inputs/UN_world_population_prospects/UN_lifeExpect_est.Rdata")
  YLL_FINAL = UN_lifeExpect_est %>%
   # filter(ISO3_code == setting) %>%
    rename(country = ISO3_code,
           life_expectancy = ex,
           age = AgeGrp) %>%
    left_join(pop_orig, by = c('age','country')) %>%
    select(country,age,life_expectancy,population) %>%
    mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
    group_by(country,age_group) %>%
    mutate(group_percent = population/sum(population),
           interim = life_expectancy * group_percent) %>%
    summarise(YLL = sum(interim)) 
  
  #apply discounting using continuous approach, as per larson et al.
  if (discounting_rate >0){YLL_FINAL$life_expectancy = (1/discounting_rate)*(1-exp(-discounting_rate*YLL_FINAL$life_expectancy ))}
  
  YLL_row = severe_outcome_3 %>%
    filter(outcome == 'death') %>%
    mutate(outcome = 'YLL',
           outcome_long = 'YLL per death in this age_group multiplied by death rate') %>%
    left_join(YLL_FINAL) %>%
    mutate(percentage = percentage*YLL)
  YLL_row = YLL_row %>% select(-YLL)
  
  severe_outcome_3 = rbind(severe_outcome_3,YLL_row)
  severe_outcome_3 = severe_outcome_3 %>% mutate(variant = VOC)
  severe_outcome_FINAL = rbind(severe_outcome_FINAL,severe_outcome_3)
}

severe_outcome_FINAL = severe_outcome_FINAL %>%
  mutate(outcome_VE = case_when(
    outcome %in% c('death','YLL') ~ 'death',
    outcome %in% c('hosp','severe_disease','critical_disease','ICU') ~ 'severe_disease'
  ))

ggplot() + 
  geom_point(data=severe_outcome_FINAL[severe_outcome_FINAL$outcome != 'YLL',],
             aes(x=factor(age_group,level=age_group_labels),
                                           y=percentage,color=as.factor(outcome),shape=as.factor(country)),na.rm=TRUE) +
  xlab('age group') +
  labs(color='outcome') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))
#_______________________________________________________________________________

save(severe_outcome_FINAL, file = "01_inputs/severe_outcome_FINAL.Rdata")
#doco explanation:
# We used country-specific severity estimates to project from cumulative incidence to 
# incidence of severe disease, acute-care bed hospitalisation and deaths [39]. 
# We then adjusted these wild-type estimates by variant- and age-specific multipliers [40-42].
# COMEBACK - could include d'n in both pop estimate and variant multiplier

VOC = 'WT'


##### (1/?) Load population-level wild-type estimate of severe outcomes
severe_outcome_0 <- read.csv('1_inputs/severe_outcome_country_level.csv')
severe_outcome_0 <- severe_outcome_0[severe_outcome_0$outcome %in% c('IFR','ISR','hosp') &
                                       severe_outcome_0$country == setting
                                     ,-c(1,5)]
#dropping ICU and ICR as we won't use them, removing source and country column
#_______________________________________________________________________________


if (VOC != 'WT'){
#####(2/?) Load variant-specific multipliers
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



#####(3/?) Calculating population-level variant-specific estimate of severe outcomes
#could be made faster, but assumptions less obvious, by including a var that is var_proxy to join on
severe_outcome_1 <- severe_outcome_0 %>%
  mutate(percentage = case_when(
    outcome == 'IFR' ~ percentage * workshop$multiplier[workshop$outcome == 'death'],
    outcome == 'ISR' ~ percentage * workshop$multiplier[workshop$outcome == 'ICU'], #assumption
    outcome == 'hosp' ~ percentage * workshop$multiplier[workshop$outcome == 'hosp']
  ),variant=VOC)
  rm (omicron_basis,workshop2)
  
} else if (VOC == 'WT'){
  severe_outcome_1 = severe_outcome_0 %>% mutate(variant = VOC)
}

#rm(severe_outcome_0)

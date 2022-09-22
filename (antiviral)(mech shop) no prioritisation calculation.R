risk_group_list = list('adults_with_comorbidities','pregnant_women')
risk_group_uniform_proportion = list()
risk_group_uniform_booster_proportion = list()

for (group in 1:length(risk_group_list)){
  risk_group_name = risk_group_list[[group]]
  
  #load pop_dn with risk group
  pop_risk_group_dn = pop_setting %>% 
    mutate(risk_group = 'general_public')
  if(risk_group_name %in% c('adults_with_comorbidities')){
    risk_dn = read.csv('1_inputs/risk_group_distribution.csv')
    risk_dn = risk_dn[risk_dn$risk_group_name == risk_group_name,]
  } else if (risk_group_name %in% c('pregnant_women')){
    load(file = "1_inputs/prevalence_pregnancy.Rdata")
    risk_dn = prevalence_pregnancy
  } else {
    stop('risk_group_name not a valid value')
  }
  
  risk_dn = risk_dn %>%
    select(age_group,prop)
  
  pop_high_risk = pop_setting %>% 
    left_join(risk_dn, by = "age_group") %>%
    mutate(risk_group = risk_group_name,
           pop = round(pop*prop),
           pop_booster = pop*2) %>% 
    select(risk_group,age_group,pop,pop_booster)
  
  pop_general_public   = pop_setting %>% 
    left_join(risk_dn, by = "age_group") %>%
    mutate(risk_group = 'general_public',
           pop = round(pop*(1-prop)),
           pop_booster = pop) %>% 
    select(risk_group,age_group,pop,pop_booster)
  
  pop_risk_group_dn = rbind(pop_general_public,pop_high_risk)
  if (round(sum(pop_risk_group_dn$pop)) != sum(pop)){stop('population by risk group group does not match total population!')}
  #__________________________
  
  #calculate proportion of adult population
  age_risk_split =  pop_risk_group_dn %>%
    mutate(adult_pop = case_when(
      age_group %in% c('0 to 4','5 to 9','10 to 17') ~ 0,
      TRUE ~ pop),
      adult_pop_booster = case_when(
        age_group %in% c('0 to 4','5 to 9','10 to 17') ~ 0,
        TRUE ~ pop_booster)) %>%
    mutate(split = adult_pop/sum(adult_pop),
           split_booster = adult_pop_booster/sum(adult_pop_booster)) %>%
    select(-adult_pop)
  if (sum(age_risk_split$split) != 1){stop('(1) simulate setting line 290: dn of doses >1')}
  if (sum(age_risk_split$split_booster) != 1){stop('(1) simulate setting line 290: dn of doses >1')}
  
  #calculate risk group as proportion of adult population
  risk_split = age_risk_split %>% group_by(risk_group) %>% summarise(sum = sum(split))
  booster_risk_split = age_risk_split %>% group_by(risk_group) %>% summarise(sum = sum(split_booster))

  risk_group_uniform_proportion[[group]] = risk_split$sum[risk_split$risk_group == risk_group_name]
  risk_group_uniform_booster_proportion[[group]] = booster_risk_split$sum[booster_risk_split$risk_group == risk_group_name]
}

###CHECK
risk_group_name = 'adults_with_comorbidities'
risk_group_toggle = "on"
vax_strategy_toggle = "on"
vax_risk_strategy_toggle = "on"
num_risk_groups = 2
source(paste(getwd(),"/(1)_simulate_setting.R",sep=""))
vaccine_coverage_end_history = vaccination_history_TRUE %>% 
  filter(date == max(vaccination_history_TRUE$date)) %>%
  select(dose,vaccine_type,age_group,risk_group,coverage_this_date)

vaccination_history_uniform = 
  apply_risk_strategy(vax_risk_strategy     = 'Y',            
                      vax_risk_proportion   = risk_group_uniform_proportion[[1]],      
                      vax_doses_general     = 1,      
                      vax_doses_risk        = 1,
                      risk_group_acceptability = 0.88,
                      risk_group_accessibility = FALSE,
                      risk_group_age_broaden   = FALSE
  )

real_doses = vaccination_history_TRUE %>% 
  filter(! age_group %in% c('0 to 4','5 to 9','10 to 17')) %>%
  group_by(risk_group,age_group,dose) %>%
  summarise(doses = sum(doses_delivered_this_date)) %>%
  left_join(pop_risk_group_dn) %>%
  mutate(cov=doses/pop) %>%
  arrange(dose,age_group)
if (length(unique(na.omit(real_doses$cov[real_doses$dose == 1])))>1 | length(unique(na.omit(round(real_doses$cov[real_doses$dose == 2],digits=2))))>1){
  stop('real doses not equal across risk groups')
}

hypoth_doses = vaccination_history_uniform %>% 
  filter(! age_group %in% c('0 to 4','5 to 9','10 to 17')) %>%
  group_by(risk_group,age_group,dose) %>%
  summarise(doses = sum(doses_delivered_this_date)) %>%
  left_join(pop_risk_group_dn) %>%
  mutate(cov=doses/pop) %>%
  arrange(dose,age_group)
if (length(unique(na.omit(round(hypoth_doses$cov[hypoth_doses$dose == 1],digits=2))))>1){
  stop('hypoth doses not equal across risk groups')
}

if (max(vaccination_history_TRUE$date[vaccination_history_TRUE$risk_group == 'general_public']) !=
    max(vaccination_history_TRUE$date[vaccination_history_TRUE$risk_group == risk_group_name]) | 
    max(vaccination_history_uniform$date[vaccination_history_uniform$risk_group == 'general_public']) !=
    max(vaccination_history_uniform$date[vaccination_history_uniform$risk_group == risk_group_name])){
  warning('max delivery dates dont align between risk groups')
}



vaccination_history_booster = 
  apply_risk_strategy(vax_risk_strategy     = 'Y',            
                      vax_risk_proportion   = risk_group_uniform_booster_proportion[[1]],      
                      vax_doses_general     = 1,      
                      vax_doses_risk        = 2,
                      risk_group_acceptability = 0.88,
                      risk_group_accessibility = FALSE,
                      risk_group_age_broaden   = FALSE
  )

hypoth_doses = vaccination_history_booster %>%
  mutate(dose = case_when(
    vaccine_type == booster_type & dose == booster_dose_number ~ 8,
    TRUE ~ dose
  )) %>%
  filter(! age_group %in% c('0 to 4','5 to 9','10 to 17')) %>%
  group_by(risk_group,age_group,dose) %>%
  summarise(doses = sum(doses_delivered_this_date)) %>%
  left_join(pop_risk_group_dn) %>%
  mutate(cov=doses/pop) %>%
  arrange(dose,age_group)
if (length(unique(na.omit(round(hypoth_doses$cov[hypoth_doses$dose == 1],digits=2))))>1 | #should be equal between risk group and general public
    length(unique(na.omit(round(hypoth_doses$cov[hypoth_doses$dose == 2],digits=2))))>1 | #should be equal between risk group and general public
    length(unique(na.omit(round(hypoth_doses$cov[hypoth_doses$dose == 8],digits=2))))>2){ #should be 0 for general public and 0.88 for risk group
  stop('doses not uniform across risk groups')
}

max(vaccination_history_booster$date[vaccination_history_booster$risk_group == 'general_public' & vaccination_history_booster$doses_delivered_this_date>0]) 
max(vaccination_history_booster$date[vaccination_history_booster$risk_group == risk_group_name & vaccination_history_booster$doses_delivered_this_date>0]) 

check = vaccination_history_booster %>% filter(date>Sys.Date()) %>% group_by(date) %>% summarise(total = sum(doses_delivered_this_date))
ggplot(check) + geom_point(aes(x=date,y=total))



### The aim of this script is to devise a way to include risk groups in the model, without prioritising them for vaccination


###LOAD
risk_group_name = 'adults_with_comorbidities'
risk_group_toggle = "on"
vax_strategy_toggle = "on"
vax_risk_strategy_toggle = "on"
num_risk_groups = 2
source(paste(getwd(),"/(1)_simulate_setting.R",sep=""))
source(paste(getwd(),"/(function)_vax_strategies.R",sep=""))
source(paste(getwd(),"/(function)_vax_strategies_risk.R",sep=""))
vaccine_coverage_end_history = vaccination_history_TRUE %>% 
  filter(date == max(vaccination_history_TRUE$date)) %>%
  select(dose,vaccine_type,age_group,risk_group,coverage_this_date)



### SCENARIO ONE: all adults willing to be vaccinated complete their primary schedule ######################################################
#NOTE: check uniform delivery over time embedded into vax_strategies_risk function
vaccination_history_uniform =  apply_risk_strategy(vax_risk_strategy     = 'N',            
                                                    vax_risk_proportion   = 0,      
                                                    vax_doses_general     = 1,      
                                                    vax_doses_risk        = 1,
                                                    risk_group_acceptability = 0.88,
                                                    risk_group_accessibility = FALSE,
                                                    risk_group_age_broaden   = FALSE
)
#######################################################################################################################################



### SCENARIO TWO: all adults willing to be vaccinated complete their primary schedule, and high-risk adults receive a booster #########
toggle_equal_priority = "individuals" #options: "individuals", "doses"
vaccination_history_uniform =  apply_risk_strategy(vax_risk_strategy     = 'N',            
                                                   vax_risk_proportion   = 0,      
                                                   vax_doses_general     = 1,      
                                                   vax_doses_risk        = 2,
                                                   risk_group_acceptability = 0.88,
                                                   risk_group_accessibility = FALSE,
                                                   risk_group_age_broaden   = FALSE
)
#######################################################################################################################################



### SCENARIO THREE: all adults willing to be vaccinated complete their primary schedule and receive a booster ###########################
#CHECK if scenario one proportions will work

vaccination_history_uniform =  apply_risk_strategy(vax_risk_strategy     = 'N',            
                                                   vax_risk_proportion   = 0,      
                                                   vax_doses_general     = 2,      
                                                   vax_doses_risk        = 2,
                                                   risk_group_acceptability = 0.88,
                                                   risk_group_accessibility = FALSE,
                                                   risk_group_age_broaden   = FALSE
)

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

if (!unique(na.omit(round(hypoth_doses$cov[hypoth_doses$dose == 1],digits=2))) == 0.88){
  warning('not all willing adults vaccinated')
}

if (max(vaccination_history_TRUE$date[vaccination_history_TRUE$risk_group == 'general_public']) !=
    max(vaccination_history_TRUE$date[vaccination_history_TRUE$risk_group == risk_group_name]) | 
    max(vaccination_history_uniform$date[vaccination_history_uniform$risk_group == 'general_public']) !=
    max(vaccination_history_uniform$date[vaccination_history_uniform$risk_group == risk_group_name])){
  warning('max delivery dates dont align between risk groups')
}
#########################################################################################################################################
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



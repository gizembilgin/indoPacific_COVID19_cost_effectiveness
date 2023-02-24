source(paste(getwd(),"/(function)_booster_strategy_informed_prior.R",sep=""))
workshop = booster_strategy_informed_prior(booster_strategy_start_date = booster_toggles$start_date,       # start of hypothetical vaccination program
                                           booster_dose_supply        = booster_toggles$dose_supply,      # num of doses avaliable
                                           #booster_rollout_months     = booster_toggles$rollout_months,   # number of months to complete booster program
                                           
                                           booster_delivery_risk_group = booster_toggles$delivery_risk_group,
                                           booster_prev_dose_floor     = booster_toggles$prev_dose_floor, #down to what dose is willing to be vaccinated?
                                           booster_prev_dose_ceiling   = booster_toggles$prev_dose_ceiling, #up to what dose are we targetting?
                                           booster_age_groups          = booster_toggles$age_groups,      #what model age groups are willing to be vaccinated?
                                           
                                           booster_prioritised_risk   = booster_toggles$prioritised_risk,
                                           booster_prioritised_age    = booster_toggles$prioritised_age,   #what age groups are prioritised
                                           
                                           booster_strategy_vaccine_type = booster_toggles$vaccine_type,   # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"  
                                           
                                           vaccination_history_FINAL_local = vaccination_history_FINAL)

to_plot = workshop %>%
  group_by(date,risk_group) %>%
  summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>%
  group_by(risk_group) %>%
  mutate(cum_doses = cumsum(doses_delivered_this_date))

ggplot(to_plot) + 
  geom_line(aes(x=date,y=cum_doses,color=as.factor(risk_group)))


workshop = booster_strategy_informed_prior(booster_strategy_start_date = booster_toggles$start_date,       # start of hypothetical vaccination program
                                           booster_dose_supply        = booster_toggles$dose_supply,      # num of doses avaliable
                                           #booster_rollout_months     = booster_toggles$rollout_months,   # number of months to complete booster program
                                           
                                           booster_delivery_risk_group = booster_toggles$delivery_risk_group,
                                           booster_prev_dose_floor     = booster_toggles$prev_dose_floor, #down to what dose is willing to be vaccinated?
                                           booster_prev_dose_ceiling   = booster_toggles$prev_dose_ceiling, #up to what dose are we targetting?
                                           booster_age_groups          = booster_toggles$age_groups,      #what model age groups are willing to be vaccinated?
                                           
                                           booster_prioritised_risk   = "Y",
                                           booster_prioritised_age    = booster_toggles$prioritised_age,   #what age groups are prioritised
                                           
                                           booster_strategy_vaccine_type = booster_toggles$vaccine_type,   # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"  
                                           
                                           vaccination_history_FINAL_local = vaccination_history_FINAL)

to_plot = workshop %>%
  group_by(date,risk_group) %>%
  summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>%
  group_by(risk_group) %>%
  mutate(cum_doses = cumsum(doses_delivered_this_date))

ggplot(to_plot) + 
  geom_line(aes(x=date,y=cum_doses,color=as.factor(risk_group)))

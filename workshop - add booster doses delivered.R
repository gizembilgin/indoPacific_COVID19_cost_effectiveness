booster_dose_delivered_log = data.frame()

for (ticket in 1:length(queue)){
  
  commands = queue[[ticket]]
  
  VE_loop = 0
  
  vax_strategy_description = commands$vax_strategy_description
  risk_group_name = commands$risk_group_name
  risk_group_toggle = commands$risk_group_toggle
  booster_toggles = commands$booster_toggles
  if ('risk_group_toggle' %in% names(commands)) { risk_group_toggle = commands$risk_group_toggle}
  if ('vax_strategy_toggles' %in% names(commands)) { vax_strategy_toggles = commands$vax_strategy_toggles}
  if ('vax_risk_strategy_toggle' %in% names(commands)) { vax_risk_strategy_toggle = commands$vax_risk_strategy_toggle}
  if ('apply_risk_strategy_toggles' %in% names(commands)) { apply_risk_strategy_toggles = commands$apply_risk_strategy_toggles}
  if ('vax_strategy_description_long' %in% names(commands)) { vax_strategy_description = paste(vax_strategy_description,commands$vax_strategy_description_long,sep=': ')}
  
  vaccination_history_FINAL = vaccination_history_TRUE

  ### sensitivity analysis - booster doses in 2023
  if (exists("booster_toggles") == FALSE){booster_toggles = "no"}
  if (length(booster_toggles)>1){
    
    if (booster_toggles$function_name == "booster_prioritised_strategies"){
      source(paste(getwd(),"/(function)_booster_dose_delivery.R",sep=""))
      source(paste(getwd(),"/(function)_prioritised_booster_dose_delivery.R",sep=""))
      
      vaccination_history_FINAL =
        booster_strategy_prioritised(
          booster_risk_strategy = booster_prioritised_strategies$strategy,
          booster_risk_proportion = booster_prioritised_strategies$risk_proportion
        )
      rm(booster_strategy_prioritised)
      
    } else if (booster_toggles$function_name == "booster_strategy"){
      source(paste(getwd(),"/(function)_booster_dose_delivery.R",sep=""))
      vaccination_history_FINAL =
        booster_strategy( booster_strategy_start_date = booster_toggles$start_date,       # start of hypothetical vaccination program
                          booster_dose_allocation     = booster_toggles$dose_allocation,  # num of doses avaliable
                          booster_rollout_speed       = booster_toggles$rollout_speed,    # doses delivered per day
                          booster_delivery_risk_group = booster_toggles$delivery_risk_group,
                          booster_delivery_includes_previously_boosted = booster_toggles$delivery_includes_previously_boosted,
                          booster_age_strategy        = booster_toggles$age_strategy,     # options: "oldest", "youngest","50_down","uniform"
                          booster_strategy_vaccine_type = booster_toggles$vaccine_type,   # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
                          booster_strategy_vaccine_interval = booster_toggles$vaccine_interval
        )
      rm(booster_strategy)
    } else if (booster_toggles$function_name == "booster_strategy_informed_prior"){
      source(paste(getwd(),"/(function)_booster_strategy_informed_prior.R",sep=""))
      workshop = booster_strategy_informed_prior(booster_strategy_start_date = booster_toggles$start_date,       # start of hypothetical vaccination program
                                                 booster_dose_supply        = booster_toggles$dose_supply,      # num of doses avaliable
                                                 booster_rollout_months     = booster_toggles$rollout_months,   # number of months to complete booster program
                                                 
                                                 booster_delivery_risk_group = booster_toggles$delivery_risk_group,
                                                 booster_prev_dose_floor     = booster_toggles$prev_dose_floor, #down to what dose is willing to be vaccinated?
                                                 booster_age_groups          = booster_toggles$age_groups,      #what model age groups are willing to be vaccinated?
                                                 
                                                 booster_prioritised_risk   = booster_toggles$prioritised_risk,
                                                 booster_prioritised_age    = booster_toggles$prioritised_age,   #what age groups are prioritised
                                                 
                                                 booster_strategy_vaccine_type = booster_toggles$vaccine_type,   # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"  
                                                 
                                                 vaccination_history_FINAL_local = vaccination_history_FINAL)
      vaccination_history_FINAL = bind_rows(vaccination_history_FINAL,workshop)
    }
    
    #update attributes!
    list_doses = unique(vaccination_history_FINAL$dose)
    list_doses = list_doses[! list_doses %in% c(8,9)]
    num_vax_doses = D = length(list_doses)
    vax_type_list = sort(unique(vaccination_history_FINAL$vaccine_type))
    num_vax_types = T = length(vax_type_list)
    num_vax_classes = num_vax_doses*num_vax_types + 1                 # + 1 for unvaccinated
    parameters$num_vax_types = num_vax_types
    parameters$num_vax_doses = num_vax_doses
    
    #sum across day in case date fitted < date_now
    if ('FROM_vaccine_type' %in% names(vaccination_history_FINAL)){
      vaccination_history_FINAL = vaccination_history_FINAL %>%
        group_by(date,vaccine_type,vaccine_mode,dose,age_group,risk_group,FROM_dose,FROM_vaccine_type) %>%
        summarise(doses_delivered_this_date = sum(doses_delivered_this_date), .groups = 'keep')
    } else{
      vaccination_history_FINAL = vaccination_history_FINAL %>%
        group_by(date,vaccine_type,vaccine_mode,dose,age_group,risk_group) %>%
        summarise(doses_delivered_this_date = sum(doses_delivered_this_date), .groups = 'keep')
    }
  }
  vaccination_history_FINAL = vaccination_history_FINAL %>%
    mutate(schedule = case_when(
      dose > 2 ~ 'booster',
      dose == 2 & vaccine_type == "Johnson & Johnson" ~ 'booster',
      TRUE ~ 'primary'
    ))
  
  vax_TRUE = vaccination_history_TRUE %>%
    mutate(schedule = case_when(
      dose > 2 ~ 'booster',
      dose == 2 & vaccine_type == "Johnson & Johnson" ~ 'booster',
      TRUE ~ 'primary'
    ))%>%
    filter(schedule == "booster") %>%
    group_by(risk_group) %>%
    summarise(true = sum(doses_delivered_this_date))
  
  vax_FINAL = vaccination_history_FINAL %>%
    filter(schedule == "booster") %>%
    group_by(risk_group) %>%
    summarise(final = sum(doses_delivered_this_date))
  
  additional_booster_doses = vax_FINAL %>% 
    left_join(vax_TRUE,by="risk_group") %>%
    mutate(doses = final - true) 
  
  row = data.frame(outcome = "booster_doses_delivered",
                           overall = sum(additional_booster_doses$doses),
                           high_risk = additional_booster_doses$doses[additional_booster_doses$risk_group == risk_group_name],
                           vax_scenario = vax_strategy_description,
                           vax_scenario_risk_group = risk_group_name)
  booster_dose_delivered_log = rbind(booster_dose_delivered_log,row)
  
  
}
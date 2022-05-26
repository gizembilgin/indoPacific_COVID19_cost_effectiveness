
###### Coding vaccine prioritisation strategies
#options(scipen = 100) #removes scientific notation

apply_risk_strategy <- function(
                         vax_risk_strategy,             # options: 'Y','N'
                         vax_risk_proportion,           # value between 0-1 (equivalent to %) of doses prioritised to the at risk group
                         vax_doses_general,             # number of doses delivered to general pop
                         vax_doses_risk,                # number of doses delivered to risk group
                         risk_group_acceptability = vax_strategy_toggles$vax_strategy_max_expected_cov
){
  
  ### WARNINGS 
  if (!vax_risk_strategy %in% c('Y','N')){stop('Is the vax strategy on or off? Y/N')}
  if (vax_risk_proportion<0 | vax_risk_proportion>1){stop('vax_risk_proportion must be between 0-1 (0%-100%)')}
  if (vax_risk_proportion == 0 & vax_risk_strategy == 'Y'){
    warning("Giving 0% priority is not priority! I have overwritten vax_risk_strategy to EQ 'N'")
    vax_risk_strategy = "N"
  }
  #_______________________________________________________________________________
  
  
  
  ### BRANCH ONE: Are we prioritising the at risk group at all?
  if (vax_risk_strategy == "N"){
    vaccination_history_FINAL = 
      vax_strategy(vax_strategy_start_date        = vax_strategy_toggles$vax_strategy_start_date,
                   vax_strategy_num_doses         = vax_strategy_toggles$vax_strategy_num_doses,
                   vax_strategy_roll_out_speed    = vax_strategy_toggles$vax_strategy_roll_out_speed,
                   vax_age_strategy               = vax_strategy_toggles$vax_age_strategy,     
                   vax_delivery_group             = 'universal',
                   vax_dose_strategy              = vax_strategy_toggles$vax_dose_strategy,            
                   vax_strategy_vaccine_type      = vax_strategy_toggles$vax_strategy_vaccine_type,            
                   vax_strategy_vaccine_interval  = vax_strategy_toggles$vax_strategy_vaccine_interval,            
                   vax_strategy_max_expected_cov  = vax_strategy_toggles$vax_strategy_max_expected_cov
      )
    return(vaccination_history_FINAL)
    #this should end the function here
  }
  #___________________________________________________________
  
  
  
  ### BRANCH TWO: What % priority are the at risk group receiving?
  at_risk_delivery_outline = vax_strategy(vax_delivery_group = 'at_risk',
                                          vax_dose_strategy              = vax_doses_risk,       
                                          vax_strategy_roll_out_speed    = vax_strategy_toggles$vax_strategy_roll_out_speed * vax_risk_proportion,            
                                          vax_strategy_max_expected_cov  = risk_group_acceptability,
                                          vax_strategy_start_date        = vax_strategy_toggles$vax_strategy_start_date,
                                          vax_strategy_num_doses         = vax_strategy_toggles$vax_strategy_num_doses,
                                          vax_age_strategy               = vax_strategy_toggles$vax_age_strategy,            
                                          vax_strategy_vaccine_type      = vax_strategy_toggles$vax_strategy_vaccine_type,            
                                          vax_strategy_vaccine_interval  = vax_strategy_toggles$vax_strategy_vaccine_interval
  )
  at_risk_delivery_outline = at_risk_delivery_outline %>% mutate(risk_group = risk_group_name)
  #___________________________________________________________
  
  
  
  ### BRANCH TWO leftover: What % are the general population receiving whilst the at risk group are prioritised?
  #make fit to same amount of days!
  if (vax_risk_proportion<1){
    generalPublic_restricted_outline = vax_strategy(vax_delivery_group = 'general_public',
                                            vax_dose_strategy              = vax_doses_general,       
                                            vax_strategy_roll_out_speed    = vax_strategy_toggles$vax_strategy_roll_out_speed * (1-vax_risk_proportion),
                                            vax_strategy_num_doses         = vax_strategy_toggles$vax_strategy_num_doses- sum(at_risk_delivery_outline$doses_delivered_this_date),
                                            restriction_date = max(at_risk_delivery_outline$date),
                                            
                                            vax_strategy_start_date        = vax_strategy_toggles$vax_strategy_start_date,
                                            vax_age_strategy               = vax_strategy_toggles$vax_age_strategy,            
                                            vax_strategy_vaccine_type      = vax_strategy_toggles$vax_strategy_vaccine_type,            
                                            vax_strategy_vaccine_interval  = vax_strategy_toggles$vax_strategy_vaccine_interval,            
                                            vax_strategy_max_expected_cov  = vax_strategy_toggles$vax_strategy_max_expected_cov
    )
    if (max(generalPublic_restricted_outline$date) != max(at_risk_delivery_outline$date)){stop('error in line 77 of function vax strategies risk')}
    generalPublic_restricted_outline = generalPublic_restricted_outline %>% mutate(risk_group = 'general_public')
  }  
  
  
  
  ### BRANCH THREE: Dose out remaining doses to general population
  #<interim update vaccine_coverage_end_history so eligible pop is correct>
  vaccination_history_MODF = rbind(vaccination_history_TRUE,at_risk_delivery_outline,generalPublic_restricted_outline)
  
  vaccination_history_MODF = vaccination_history_MODF %>% left_join(pop_risk_group_dn) %>%
    group_by(risk_group,age_group,vaccine_type,dose) %>%
    mutate(coverage_this_date = cumsum(doses_delivered_this_date)/pop) 
  
  vaccine_coverage_end_history_UPDATED = data.frame()
  for (t in 1:length(unique(vaccination_history_MODF$vaccine_type))){
    this_vax = vaccination_history_MODF[vaccination_history_MODF$vaccine_type == unique(vaccination_history_MODF$vaccine_type)[t],] 
    this_vax = this_vax %>% filter(date == max(this_vax$date)) %>%
      select(dose,vaccine_type,age_group,risk_group,coverage_this_date)
    vaccine_coverage_end_history_UPDATED = rbind(vaccine_coverage_end_history_UPDATED,this_vax)
  }
  vaccine_coverage_end_history_UPDATED$coverage_this_date[is.na(vaccine_coverage_end_history_UPDATED$coverage_this_date)] = 0
  
  
  
  #<fin>
  
  generalPublic_leftover_outline = vax_strategy(vax_delivery_group = 'general_public',
                                                  vax_dose_strategy              = vax_doses_general,       
                                                  vax_strategy_roll_out_speed    = vax_strategy_toggles$vax_strategy_roll_out_speed,
                                                  vax_strategy_num_doses         = vax_strategy_toggles$vax_strategy_num_doses - 
                                                        sum(at_risk_delivery_outline$doses_delivered_this_date) - sum(generalPublic_restricted_outline$doses_delivered_this_date),
                                                  vax_strategy_start_date        = max(at_risk_delivery_outline$date) + 1,
                                                  
                                                  vax_age_strategy               = vax_strategy_toggles$vax_age_strategy,            
                                                  vax_strategy_vaccine_type      = vax_strategy_toggles$vax_strategy_vaccine_type,            
                                                  vax_strategy_vaccine_interval  = vax_strategy_toggles$vax_strategy_vaccine_interval,            
                                                  vax_strategy_max_expected_cov  = vax_strategy_toggles$vax_strategy_max_expected_cov,
                                                  vax_end_hist   = vaccine_coverage_end_history_UPDATED
  )
  generalPublic_leftover_outline = generalPublic_leftover_outline %>% mutate(risk_group = 'general_public')
  
  vaccination_history_MODF = rbind(vaccination_history_TRUE,at_risk_delivery_outline,generalPublic_restricted_outline,generalPublic_leftover_outline)
  #make sure sorted properly?
  
  return(vaccination_history_MODF)
  #print out end date of complete delivery to at risk group
}
# 
# test = apply_risk_strategy(
#     vax_risk_strategy = 'Y',             # options: 'Y','N'
#     vax_risk_proportion = 0.8,           # value between 0-1 (equivalent to %) of doses prioritised to the at risk group
#     vax_doses_general = 1,               # number of doses delivered to general pop
#     vax_doses_risk = 1                  # number of doses delivered to risk group
# )
# 
# View(test)
# 
# apply_risk_strategy_toggles = list(
#   vax_risk_strategy = 'Y',             # options: 'Y','N'
# vax_risk_proportion = 0.8,           # value between 0-1 (equivalent to %) of doses prioritised to the at risk group
# vax_doses_general = 1,               # number of doses delivered to general pop
# vax_doses_risk = 1                  # number of doses delivered to risk group
# )




#_______________________________________________________________________________

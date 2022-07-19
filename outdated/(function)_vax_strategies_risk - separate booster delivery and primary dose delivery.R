
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
    #if not, let's use a simple function call
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
  ## SUBBRANCH: If the risk group are being prioritised for more doses than the general public
  if (vax_doses_risk>vax_doses_general){ #e.g., if 'at risk' group prioritised for a booster dose
    
    #(1) what proportion of the 'at risk' population are untouched by the existing vaccination program?
    workshop_pop = pop_risk_group_dn[pop_risk_group_dn == risk_group_name,] %>% select(age_group,pop) #at risk pop
    
    workshop_cov = vaccination_history_TRUE %>% # current vaccine coverage in at risk
      filter(date == max(vaccination_history_TRUE$date) &
               risk_group == risk_group_name)
    
    # pop touched by existing vaccination program
    workshop_touched = workshop_cov %>% 
      filter(dose == 1) 
    workshop_touched = aggregate(workshop_touched$coverage_this_date, by = list(category = workshop_touched$age_group), FUN = sum)
    colnames(workshop_touched) = c('age_group','cov')
    
    #consideration of 'uncreachable' % either for vaccine hesitancy or access
    unreachable = 1-risk_group_acceptability 
    workshop_pop_dn = workshop_touched %>% 
      left_join(workshop_pop) %>%
      mutate(pop_touched = pop*cov,
             pop_untouched = pop - pop*unreachable - pop*cov)
    
    # calculate proportion to booster / (booster + primary)
    vax_risk_proportion_booster = (sum(workshop_pop_dn$pop_touched))/(sum(workshop_pop_dn$pop_untouched)+ sum(workshop_pop_dn$pop_touched))   #NOTE: this is standard assumption of basis of population size
    if (is.na(override_vax_risk_proportion) == FALSE){vax_risk_proportion_booster = override_vax_risk_proportion} 
    #COMEBACK - sensitivity analysis, since real 0.54 set to 0.25 and 0.75
    #______________________________
    
    #(2) distribute to at risk population who have not yet been vaccinated at all (primary + booster)
    at_risk_delivery_outline_pt1 = vax_strategy(vax_delivery_group = 'at_risk',
                                                vax_dose_strategy              = vax_doses_risk,       
                                                vax_strategy_roll_out_speed    = vax_strategy_toggles$vax_strategy_roll_out_speed * vax_risk_proportion*(1-vax_risk_proportion_booster),            
                                                vax_strategy_max_expected_cov  = risk_group_acceptability,
                                                vax_strategy_start_date        = vax_strategy_toggles$vax_strategy_start_date,
                                                vax_strategy_num_doses         = vax_strategy_toggles$vax_strategy_num_doses,
                                                vax_age_strategy               = vax_strategy_toggles$vax_age_strategy,            
                                                vax_strategy_vaccine_type      = vax_strategy_toggles$vax_strategy_vaccine_type,            
                                                vax_strategy_vaccine_interval  = vax_strategy_toggles$vax_strategy_vaccine_interval
    )
    #______________________________
    
    #(3) boost at risk population who had already received their primary doses
    at_risk_delivery_outline_pt2 = booster_delivery (vax_strategy_start_date        = vax_strategy_toggles$vax_strategy_start_date,
                                                     vax_strategy_roll_out_speed    = vax_strategy_toggles$vax_strategy_roll_out_speed * vax_risk_proportion*vax_risk_proportion_booster,
                                                     vax_delivery_group             = 'at_risk',
                                                     vax_strategy_num_doses         = vax_strategy_toggles$vax_strategy_num_doses - sum(at_risk_delivery_outline_pt1$doses_delivered_this_date),
                                                     vax_age_strategy               = vax_strategy_toggles$vax_age_strategy,           
                                                     booster_dose_type              = "Johnson & Johnson",
                                                     booster_dose_number            = 2,
                                                     booster_dose_interval          = 3* 30 #COMEBACK - HARD CODED
    )
    #______________________________
    at_risk_delivery_outline = bind_rows(at_risk_delivery_outline_pt1,at_risk_delivery_outline_pt2)
    
  } else{
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
  }
  
  at_risk_delivery_outline = at_risk_delivery_outline %>% mutate(risk_group = risk_group_name)
  #___________________________________________________________
  
  
  
  ### BRANCH THREE: Distribute remaining doses to general population
  #<interim update vaccine_coverage_end_history so eligible pop is correct>
  vaccination_history_MODF = rbind(vaccination_history_TRUE,at_risk_delivery_outline)
  
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
  
  #<interim make doses available per day tracker>
  limiter = at_risk_delivery_outline %>% group_by(date) %>% 
    summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>% 
    filter(doses_delivered_this_date > 0 ) %>%
    mutate(doses_avaliable = vax_strategy_toggles$vax_strategy_roll_out_speed - doses_delivered_this_date,
           day = as.numeric(date - min(limiter$date) + 1)) #see line 315 in function vax strategy
  #<fin>
  
  generalPublic_leftover_outline = vax_strategy(vax_delivery_group = 'general_public',
                                                vax_dose_strategy              = vax_doses_general,       
                                                vax_strategy_roll_out_speed    = vax_strategy_toggles$vax_strategy_roll_out_speed,
                                                vax_roll_out_speed_modifier    = limiter,
                                                vax_strategy_num_doses         = vax_strategy_toggles$vax_strategy_num_doses - sum(at_risk_delivery_outline$doses_delivered_this_date) ,
                                                vax_strategy_start_date        = vax_strategy_toggles$vax_strategy_start_date,
                                                vax_age_strategy               = vax_strategy_toggles$vax_age_strategy,            
                                                vax_strategy_vaccine_type      = vax_strategy_toggles$vax_strategy_vaccine_type,            
                                                vax_strategy_vaccine_interval  = vax_strategy_toggles$vax_strategy_vaccine_interval,            
                                                vax_strategy_max_expected_cov  = vax_strategy_toggles$vax_strategy_max_expected_cov,
                                                vax_end_hist   = vaccine_coverage_end_history_UPDATED
  )
  generalPublic_leftover_outline = generalPublic_leftover_outline %>% mutate(risk_group = 'general_public')
  
  vaccination_history_MODF = rbind(vaccination_history_TRUE,at_risk_delivery_outline,generalPublic_leftover_outline)

  ###CHECKS
  #CHECK 1: total doses delivered <= total doses available
  check_df = bind_rows(at_risk_delivery_outline,generalPublic_leftover_outline)
  if (round(sum(check_df$doses_delivered_this_date)) > vax_strategy_toggles$vax_strategy_num_doses){stop('Total doses delivered > total doses avaliable!')}
  
  check_df_daily = check_df %>% group_by(date) %>% summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>% mutate(label ='total')
  if (nrow(check_df_daily[round(check_df_daily$doses_delivered_this_date)>vax_strategy_toggles$vax_strategy_roll_out_speed,])>0){
    ggplot(check_df_daily) + geom_point(aes(x=date,y=doses_delivered_this_date))
    
    if (vax_doses_risk>vax_doses_general){
      df1_pt1 = at_risk_delivery_outline_pt1 %>% group_by(date) %>% summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>% mutate(label = 'at risk delivery primary')
      df1_pt2 = at_risk_delivery_outline_pt2 %>% group_by(date) %>% summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>% mutate(label = 'at risk delivery booster')
      df1 = rbind(df1_pt1,df1_pt2)
    } else{df1 = at_risk_delivery_outline  %>% group_by(date) %>% summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>% mutate(label = 'at risk delivery')}
    df2 = generalPublic_leftover_outline  %>% group_by(date) %>% summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>% mutate(label = 'general public leftover')
    check_df_daily_comp = bind_rows(df1,df2,check_df_daily)
    ggplot(check_df_daily_comp) + geom_point(aes(x=date,y=doses_delivered_this_date,color=as.factor(label)))
    
    stop('Error line 179 of vax strategies risk - more doses delivered per day than capacity')
  }
  
  ggplot(check_df[check_df$risk_group == 'general_public',]) + geom_point(aes(x=date,y=doses_delivered_this_date,color=as.factor(age_group),shape=as.factor(dose)))
  ggplot(check_df[check_df$risk_group != 'general_public',]) + geom_point(aes(x=date,y=doses_delivered_this_date,color=as.factor(age_group),shape=as.factor(dose)))
  
  
  return(vaccination_history_MODF)
  #print out end date of complete delivery to at risk group
}

#TEST
test = apply_risk_strategy(
    vax_risk_strategy = 'Y',             # options: 'Y','N'
    vax_risk_proportion = 0.8,           # value between 0-1 (equivalent to %) of doses prioritised to the at risk group
    vax_doses_general = 2,             # number of doses delivered to general pop
    vax_doses_risk = 2,                # number of doses delivered to risk group
    risk_group_acceptability = vax_strategy_toggles$vax_strategy_max_expected_cov
)
View(test)
sum(test$doses_delivered_this_date)

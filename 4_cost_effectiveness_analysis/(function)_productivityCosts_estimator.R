require(readr); require(ggplot2); require(tidyverse)

### This function calculates the productivity losses due to illness and premature mortality 

productivityCosts_estimator <- function(
    LIST_CEA_settings,
    MASTER_antiviral_simulations,
    toggle_discounting_rate = 0.03, 
    this_risk_group = "adults_with_comorbidities"
){
  
  # Step One: load inputs
  setting_list = LIST_CEA_settings
  age_groups_num = c(0,4,9,17,29,44,59,69,110)
  age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100')
  load(file = "2_inputs/expected_daily_earnings.Rdata")
  load(file = paste(gsub("4_cost_effectiveness_analysis","",getwd()),
                    "1_inputs/UN_world_population_prospects/UN_pop_est.Rdata",sep=''))
  
  pop_orig <- UN_pop_est %>% 
    rename(population = PopTotal,
           age = AgeGrp,
           setting = ISO3_code) %>%
    filter(setting %in% setting_list) %>%
    select(setting,age,population)
  rm(UN_pop_est)
  
  expected_daily_earnings = expected_daily_earnings %>%
    rename(setting = ISO3_code)
  
  TRANSLATED_antiviral_simulations = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == FALSE) %>%
    
    #created shorten name to describe booster dose eligibility
    mutate(booster_vax_scenario = case_when( 
      vax_scenario == "catchup campaign for high-risk adults: assume booster to high-risk adults who have previously completed their primary schedule but have not recieved a booster"  ~ "booster dose catch-up campaign for high-risk adults",           
      vax_scenario == "catchup campaign for all adults: assume booster to all adults who have previously completed their primary schedule but have not recieved a booster" ~ "booster dose catch-up campaign for all adults",                       
      vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults" ~ "booster to all adults, prioritised to high-risk adults",                                                           
      vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~ "booster to all adults previously willing to be vaccinated",                      
      vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~ "booster to all high-risk adults previously willing to be vaccinated",  
      vax_scenario == "all willing adults vaccinated with a primary schedule" ~ "no booster dose"           
    )) %>%
    filter(is.na(booster_vax_scenario) == FALSE) %>%
    
    #DECISION - CEA for antivirals as of 01/01/2023 
    filter(intervention %in% c('vaccine','antiviral 2023-01-01')) %>%
    mutate(intervention = case_when(
      intervention == "vaccine" ~ "booster dose 2023-03-01",
      antiviral_type == "molunipiravir" ~ "molunipiravir 2023-01-01",
      antiviral_type == "nirmatrelvir_ritonavir" ~ "nirmatrelvir_ritonavir 2023-01-01"
    )) %>%
    
    mutate(intervention_target_group = 
             case_when(
               intervention %in% c("molunipiravir 2023-01-01","nirmatrelvir_ritonavir 2023-01-01") ~ antiviral_target_group,
               booster_vax_scenario %in% c("booster dose catch-up campaign for high-risk adults","booster to all high-risk adults previously willing to be vaccinated") ~ vax_scenario_risk_group,
               booster_vax_scenario %in% c("booster dose catch-up campaign for all adults","booster to all adults, prioritised to high-risk adults", "booster to all adults previously willing to be vaccinated") ~ "all_adults"
             )) %>%
    
    filter(result %in% c("n"))
  #_______________________________________________________________________________
  
  
  # Step Two: productivity losses due to illness
  ###COMEBACK - need to sample for each individual
  duration_symptomatic = 9.87
  productivity_loss = TRANSLATED_antiviral_simulations %>%
    filter(outcome %in% c("critical_disease","severe_disease")) %>%
    left_join(expected_daily_earnings, by = c("setting","age_group")) %>%
    mutate(daily_earning = daily_earning * value * duration_symptomatic) %>%
    group_by(setting,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(cost = sum(daily_earning), .groups = "keep")%>%
    mutate(productivity_loss_category = "illness")
  #_______________________________________________________________________________
  
  # Step Three:Expected remaining lifetime earnings
  #Step One: load life expectancy at age X
  #"The average number of remaining years of life expected by a hypothetical cohort of individuals alive at age x 
  # who would be subject during the remaining of their lives to the mortality rates of a given period."
  # https://population.un.org/wpp/Download/Standard/Mortality/
  load(file = paste(gsub("4_cost_effectiveness_analysis","",getwd()),
                    "1_inputs/UN_world_population_prospects/UN_lifeExpect_est.Rdata",sep=''))
  
  UN_lifeExpect_est = UN_lifeExpect_est %>%
    rename(life_expectancy = ex,
           age = AgeGrp,
           setting = ISO3_code) %>%
    mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels))  %>%
    select(setting,age_group,age,life_expectancy)
  
  # ggplot(UN_lifeExpect_est[UN_lifeExpect_est$setting %in% setting_list,]) +
  #   geom_point(aes(x=age,y=life_expectancy,color=as.factor(setting)),position="dodge")
   
  remaining_lifetime_earnings = expected_daily_earnings %>%
    left_join(UN_lifeExpect_est, by = c("age_group","setting")) %>%
    mutate(yearly_earning = daily_earning * 365)
  
  workshop = data.frame()
  for (this_setting in unique(remaining_lifetime_earnings$setting)){ # for each setting
    for (this_age in unique(remaining_lifetime_earnings$age)){ # for each age
      
      #how many years of life left at this age in this setting?
      this_workshop = remaining_lifetime_earnings %>% 
        filter(setting == this_setting &
                 age == this_age)
      
      #create window of ages we are looking at
      this_workshop_window = remaining_lifetime_earnings %>% 
        filter(setting == this_setting &
                 age > this_age &
                 age <= (this_age + ceiling(this_workshop$life_expectancy))) 
      
      #apply discounting (if>0) to each subsequent year of life
      if (toggle_discounting_rate>0){
        this_workshop_window = this_workshop_window %>%
          mutate(discounting_year = age - this_age,
                 discounting_multiplier = 1/(1+toggle_discounting_rate)^discounting_year,
                 yearly_earning = yearly_earning*discounting_multiplier) %>%
          select(-discounting_year,-discounting_multiplier)
        
      }
      
      #integer component of years of life left, e.g., if life expectancy 68.8 years -> 68 years
      full = this_workshop_window %>% filter(setting == this_setting &
                                               age > this_age &
                                               age <= (this_age + floor(this_workshop$life_expectancy)))
      full = sum(full$yearly_earning)
      
      #decimal component of years of life left, e.g., if life expectancy 68.8 years -> 0.8 years
      partial = this_workshop_window %>% filter(setting == this_setting &
                                                  age == (this_age + ceiling(this_workshop$life_expectancy)))
      partial = partial$yearly_earning*(this_workshop$life_expectancy - floor(this_workshop$life_expectancy))
      if(length(partial) == 0){partial = 0}
      
      this_row = data.frame(age=this_age,
                            setting = this_setting,
                            life_expectancy = this_workshop$life_expectancy,
                            remaining_lifetime_earnings = full+partial)
      
      workshop = rbind(workshop,this_row)
    }
  }
  # ggplot(workshop) +
  #   geom_line(aes(x=age,y=remaining_lifetime_earnings)) +
  #   facet_grid(setting ~.)
  
  
  # calculate remaining_lifetime_earnings per model age group weighted by pop dn
  remaining_lifetime_earnings = workshop %>%
    left_join(pop_orig, by = c('age','setting')) %>%
    select(setting,age,remaining_lifetime_earnings,population) %>%
    mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
    group_by(setting,age_group) %>%
    mutate(group_percent = population/sum(population),
           interim = remaining_lifetime_earnings * group_percent) %>%
    summarise(remaining_lifetime_earnings = sum(interim),.groups="keep") 
  #ggplot(remaining_lifetime_earnings[remaining_lifetime_earnings$setting %in% setting_list,]) + geom_col(aes(x=age_group,y=remaining_lifetime_earnings,fill=as.factor(setting)),position="dodge")
  
  workshop = TRANSLATED_antiviral_simulations %>%
    filter(outcome %in% c("death")) %>%
    left_join(remaining_lifetime_earnings, by = c("setting","age_group")) %>%
    mutate(remaining_lifetime_earnings = remaining_lifetime_earnings * value) %>%
    group_by(setting,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(cost = sum(remaining_lifetime_earnings), .groups = "keep") %>%
    mutate(productivity_loss_category = "premature death")
  productivity_loss = rbind(productivity_loss,workshop)
  #_______________________________________________________________________________
  
  
  productivity_loss = productivity_loss %>%
    group_by(setting,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(cost = sum(cost), .groups = "keep")
  
  return(productivity_loss)
}
require(readr); require(ggplot2); require(tidyverse)

### This function calculates the mean, median, and sd of total QALYs averted by setting, booster_vax_scenario, intervention, and intervention_target_group
### Italso returns additional outcomes such as death and hosp
#NB: There is no uncertainty in QALY conversion estimates as data sources are expert point estimates of:
#       population (UN), life expectancy (UN), HRQoL (Robinson, Eber & Hammitt), and age_severity_specific_QALYs (Robinson, Eber & Hammitt)

outcomesAverted_estimator <- function(
    LIST_CEA_settings,
    MASTER_antiviral_simulations,
    ARRAY_additional_outcomes = c("death","hosp"),
    toggle_longCOVID = "off",
    toggle_discounting_rate = 0.03, #NB: limitation can only change discounting of YLL of fatal cases, not YLD of critical cases due to restrictions of underlying data
    this_risk_group = "adults_with_comorbidities"
    ){

  setting_list = LIST_CEA_settings
  age_groups_num = c(0,4,9,17,29,44,59,69,110)
  age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100')

  
  
  ### PART ONE: loading QALY estimates############################################
  ## non-fatal QALYS 
  #Step One: load population distribution
  load(file = paste(gsub("4_cost_effectiveness_analysis","",getwd()),
                    "1_inputs/UN_world_population_prospects/UN_pop_est.Rdata",sep=''))
  
  pop_orig <- UN_pop_est %>% 
    rename(country_long = Location,
           population = PopTotal,
           population_female = PopFemale,
           age = AgeGrp) %>%
    filter(ISO3_code %in% setting_list)
  rm(UN_pop_est)
  #_______________________________________________________________________________
  
  #Step Two: load non-fatal QALYs by severity and age
  raw <- read.csv("2_inputs/age_severity_specific_QALYs.csv",header=TRUE) 
  #ggplot(raw) + geom_line(aes(x=age,y=QALYs)) + facet_grid(severity ~ ., scale = "free_y")
  #_______________________________________________________________________________
  
  #Step Three: calculate QALYs per model age group weighted by pop dn
  QALYs_nonFatal = raw %>%
    left_join(pop_orig, by = c('age'), relationship = "many-to-many") %>%
    select(severity,ISO3_code,age,QALYs,population) %>%
    mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
    group_by(ISO3_code,severity,age_group) %>%
    mutate(group_percent = population/sum(population),
           interim = QALYs * group_percent) %>%
    summarise(QALYs = sum(interim),.groups="keep") 
  #ggplot(QALYs_nonFatal) + geom_col(aes(x=age_group,y=QALYs,fill=as.factor(ISO3_code)),position="dodge") + facet_grid(severity ~ ., scale = "free_y")
  #______________________________________________________________________________
  
  
  ## fatal QALYS ###########################################################
  #Step One: load life expectancy at age X
  #"The average number of remaining years of life expected by a hypothetical cohort of individuals alive at age x 
  # who would be subject during the remaining of their lives to the mortality rates of a given period."
  # https://population.un.org/wpp/Download/Standard/Mortality/
  load(file = paste(gsub("4_cost_effectiveness_analysis","",getwd()),
                    "1_inputs/UN_world_population_prospects/UN_lifeExpect_est.Rdata",sep=''))
  
  UN_lifeExpect_est = UN_lifeExpect_est %>%
    rename(life_expectancy = ex,
           age = AgeGrp)
  
  # ggplot(UN_lifeExpect_est[UN_lifeExpect_est$ISO3_code %in% setting_list,]) +
  #   geom_point(aes(x=age,y=life_expectancy,color=as.factor(ISO3_code)),position="dodge")
  #_______________________________________________________________________________
  
  #Step Two: convert life expectancy to QALYs using HRQoL estimates
  raw <- read.csv("2_inputs/age_specific_HRQoL_v2.csv",header=TRUE) 
  
  HRQoL = raw %>%
    left_join(UN_lifeExpect_est, by = c("age")) %>%
    select(ISO3_code,age,HRQoL,life_expectancy) %>%
    filter(ISO3_code %in% setting_list)
  
  workshop = data.frame()
  for (this_setting in unique(HRQoL$ISO3_code)){ # for each setting
    for (this_age in unique(HRQoL$age)){ # for each age
      
      #how many years of life left at this age in this setting?
      this_workshop = HRQoL %>% filter(ISO3_code == this_setting &
                                         age == this_age)
      
      #create window of ages we are looking at
      this_workshop_window = HRQoL %>% 
        filter(ISO3_code == this_setting &
                 age > this_age &
                 age <= (this_age + ceiling(this_workshop$life_expectancy))) 
      
      #apply discounting (if>0) to each subsequent year of life
      if (toggle_discounting_rate>0){
        this_workshop_window = this_workshop_window %>%
          mutate(discounting_year = age - this_age,
                 discounting_multiplier = 1/(1+toggle_discounting_rate)^discounting_year,
                 HRQoL = HRQoL*discounting_multiplier) %>%
          select(-discounting_year,-discounting_multiplier)
        
      }
      
      #integer component of years of life left, e.g., if life expectancy 68.8 years -> 68 years
      full = this_workshop_window %>% filter(ISO3_code == this_setting &
                                               age > this_age &
                                               age <= (this_age + floor(this_workshop$life_expectancy)))
      full = sum(full$HRQoL)
      
      #decimal component of years of life left, e.g., if life expectancy 68.8 years -> 0.8 years
      partial = this_workshop_window %>% filter(ISO3_code == this_setting &
                                                  age == (this_age + ceiling(this_workshop$life_expectancy)))
      partial = partial$HRQoL*(this_workshop$life_expectancy - floor(this_workshop$life_expectancy))
      if(length(partial) == 0){partial = 0}
      
      this_row = data.frame(age=this_age,
                            ISO3_code = this_setting,
                            life_expectancy = this_workshop$life_expectancy,
                            QALY = full+partial)
      
      workshop = rbind(workshop,this_row)
    }
  }
  # ggplot(workshop) + geom_line(aes(x=age,y=life_expectancy)) + 
  #   geom_line(aes(x=age,y=QALY)) + 
  #   facet_grid(ISO3_code ~.)
  #_______________________________________________________________________________
  
  
  #Step Three: calculate YLL per model age group weighted by pop dn
  QALYs_fatal = workshop %>%
    left_join(pop_orig, by = c('age','ISO3_code')) %>%
    select(ISO3_code,age,QALY,population) %>%
    mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
    group_by(ISO3_code,age_group) %>%
    mutate(group_percent = population/sum(population),
           interim = QALY * group_percent) %>%
    summarise(QALYs = sum(interim),.groups="keep") %>%
    mutate(severity = "fatal")
  #ggplot(QALYs_fatal[QALYs_fatal$ISO3_code %in% setting_list,]) + geom_col(aes(x=age_group,y=YLL,fill=as.factor(ISO3_code)),position="dodge")
  #_______________________________________________________________________________
  
  
  #QALYs_fatal %>% filter(ISO3_code %in% setting_list & age_group %in% c('0 to 4'))
  #discounting == 0 -> 5.4 times 60 to 69
  # ISO3_code age_group   YLL
  # 1 FJI     0 to 4     67.4
  # 2 IDN     0 to 4     67.3
  # 3 PNG     0 to 4     66.0
  # 4 TLS     0 to 4     69.0
  # ISO3_code age_group QALYs -> 6 times 60 to 69
  # 1 FJI       0 to 4     59.8
  # 2 IDN       0 to 4     59.7
  # 3 PNG       0 to 4     58.7
  # 4 TLS       0 to 4     61.0
  
  #discounting == 0.03 -> 2.5 times 60 to 69
  # ISO3_code age_group   YLL
  # 1 FJI     0 to 4     28.9
  # 2 IDN     0 to 4     28.9
  # 3 PNG     0 to 4     28.7
  # 4 TLS     0 to 4     29.1
  # ISO3_code age_group QALYs -> 3 times 60 to 69
  # 1 FJI       0 to 4     26.3
  # 2 IDN       0 to 4     26.3
  # 3 PNG       0 to 4     26.1
  # 4 TLS       0 to 4     26.4
  
  #discounting == 0.05 -> 1.9 times 60 to 69
  # ISO3_code age_group   YLL
  # 1 FJI     0 to 4     19.3
  # 2 IDN     0 to 4     19.3
  # 3 PNG     0 to 4     19.3
  # 4 TLS     0 to 4     19.4
  # ISO3_code age_group QALYs -> 2 times 60 to 69
  # 1 FJI       0 to 4     17.8
  # 2 IDN       0 to 4     17.8
  # 3 PNG       0 to 4     17.8
  # 4 TLS       0 to 4     17.8
  
  #QALYs_fatal %>% filter(ISO3_code %in% setting_list & age_group %in% c('60 to 69'))
  #discounting == 0 
  # ISO3_code age_group   YLL
  # 1 FJI     60 to 69   13.2
  # 2 IDN     60 to 69   13.1
  # 3 PNG     60 to 69   13.6
  # 4 TLS     60 to 69   14.3
  # ISO3_code age_group QALYs
  # 1 FJI       60 to 69   10.3
  # 2 IDN       60 to 69   10.3
  # 3 PNG       60 to 69   10.6
  # 4 TLS       60 to 69   11.2
  
  #discounting == 0.03
  # ISO3_code age_group   YLL
  # 1 FJI     60 to 69   10.9
  # 2 IDN     60 to 69   10.8
  # 3 PNG     60 to 69   11.1
  # 4 TLS     60 to 69   11.7
  # ISO3_code age_group QALYs
  # 1 FJI       60 to 69   8.41
  # 2 IDN       60 to 69   8.37
  # 3 PNG       60 to 69   8.60
  # 4 TLS       60 to 69   8.96
  
  #discounting == 0.05
  # ISO3_code age_group   YLL
  # 1 FJI     60 to 69   9.66
  # 2 IDN     60 to 69   9.62
  # 3 PNG     60 to 69   9.85
  # 4 TLS     60 to 69  10.2 
  # ISO3_code age_group QALYs
  # 1 FJI       60 to 69   7.42
  # 2 IDN       60 to 69   7.38
  # 3 PNG       60 to 69   7.56
  # 4 TLS       60 to 69   7.83
  ################################################################################
  
  
  ## QALYs longCovid
  if (toggle_longCOVID == "on"){
    prevalence_postSixMonths = 0.027
    prevalence_upToSixMonths = 0.457
    longCOVID_disability_weight = 0.051
    
    QALYs_longCOVID = QALYs_fatal %>%
      #for ongoing long COVID use fatal QALY estimates of YLL weighted by HRQoL and discounted in future years
      mutate(QALYs = QALYs * prevalence_postSixMonths*longCOVID_disability_weight) %>%
      mutate(severity = "total_cases") %>% #ASSUMPTION: all cases of COVID-19 can develop long COVID
      #for short-term long COVID
      mutate(QALYs = QALYs + prevalence_upToSixMonths*longCOVID_disability_weight*0.5)
    
    QALY_estimates = rbind(QALYs_nonFatal,QALYs_fatal,QALYs_longCOVID) 
  } else(
    QALY_estimates = rbind(QALYs_nonFatal,QALYs_fatal)
  )
  

  ## bring together QALYS ######################################################
  QALY_estimates = QALY_estimates %>%
    rename(setting = ISO3_code) %>%
    mutate(outcome = case_when(
      severity == "critical" ~ "critical_disease",
      severity ==  "mild" ~  "mild" ,
      severity == "severe" ~ "severe_disease",
      severity == "fatal"  ~ "death",
      TRUE ~ severity
    )) %>%
    ungroup() %>%
    select(-severity)
  # ggplot(QALY_estimates) +
  #   geom_col(aes(x=age_group,y=QALYs,fill=as.factor(setting)),position="dodge") +
  #   theme_bw() +
  #   labs(fill="")+
  #   facet_grid(outcome ~ ., scale = "free_y")
  ##############################################################################
  ##############################################################################
  
  
  
  
  
  ### PART TWO: load antiviral simulation ######################################
  ## Subset #####################################################################
  # We would like a data set with the following columns:
  # setting, outcome, booster_vax_scenario, intervention, intervention target group, 
  # intervention_doses_delivered, count_outcomes_averted
  
  workshop = MASTER_antiviral_simulations %>%
    filter(is.na(age_group) == FALSE) %>%
    select(-country,-setting_beta) %>%
    
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
    
    #DECISION - CEA of antivirals for a whole year (as of 01/01/2023)
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
    
    filter(result %in% c("n")) %>%
    
    #mutate(doses_per_outcome_averted = intervention_doses_delivered/value) %>%
    rename(count_outcomes_averted = value) %>%
    
    filter(! (outcome %in% c("YLL","neonatal_deaths","booster_doses_delivered","ICU"))) %>%
    
    select(setting, outcome, booster_vax_scenario, intervention, intervention_target_group, age_group,count_outcomes_averted)
  
  if (nrow(workshop) != nrow(na.omit(workshop))){stop("NA introduced")}
  
  #unique(workshop$setting)
  #[1] "FJI" "PNG" "TLS"
  #unique(workshop$outcome)
  # [1] "critical_disease"      "death"                 "hosp"                  "hosp_after_antivirals"
  # [5] "severe_disease"        "mild"                  "total_cases"   
  #unique(workshop$booster_vax_scenario)
  # [1] "booster dose catch-up campaign for high-risk adults"                 "booster dose catch-up campaign for all adults"                      
  # [3] "booster to all adults, prioritised to high-risk adults"              "booster to all adults previously willing to be vaccinated"          
  # [5] "booster to all high-risk adults previously willing to be vaccinated" "no booster dose"                                                   
  #unique(workshop$intervention)
  #[1] "nirmatrelvir_ritonavir 2023-01-01" "molunipiravir 2023-01-01"          "booster dose 2023-03-01"          
  #unique(workshop$intervention_target_group)
  #[1] "adults_with_comorbidities"                         "unvaccinated_adults_AND_adults_with_comorbidities" "all_adults"                                       
  #[4] "unvaccinated_adults"                               "pregnant_women"      
  #_______________________________________________________________________________
  
  
  ## Summarise uncertainty in vax/antiviral model run ###########################
  # DECISION: calculating uncertainty here and propagating through using this summary in case the number of CEA model runs != the number of vax/antiviral model runs (100)
  TRANSLATED_antiviral_simulations = workshop %>%
    group_by(setting,outcome,booster_vax_scenario,intervention,intervention_target_group,age_group) %>%
    summarise(mean = mean(count_outcomes_averted),
              median = median(count_outcomes_averted),
              sd = sd(count_outcomes_averted),
              .groups="keep")
  
  #_______________________________________________________________________________
  ##############################################################################
  
  rm(workshop,MASTER_antiviral_simulations)
  ##############################################################################
  
  
  
  
  
  ### PART THREE: calculating QALYs ############################################
  outcomes_averted = TRANSLATED_antiviral_simulations %>%
    left_join(QALY_estimates, by = c("setting","outcome","age_group")) %>%
    filter(is.na(QALYs) == FALSE) %>%
    mutate(mean = mean * QALYs,
           median = median * QALYs,
           sd = sd * QALYs) %>%
    select(-QALYs) %>%
    mutate(outcome = "QALYs")%>%
    #collapsing outcome and age_group to calculate QALYs
    group_by(setting,outcome,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(mean = sum(mean),
              median = sum(median),
              sd = sum(sd),
              .groups = "keep") 
  
  QALY_breakdown = TRANSLATED_antiviral_simulations %>%
    left_join(QALY_estimates, by = c("setting","outcome","age_group")) %>%
    filter(is.na(QALYs) == FALSE) %>%
    mutate(mean = mean * QALYs,
           median = median * QALYs,
           sd = sd * QALYs) %>%
    select(-QALYs) %>%
    mutate(outcome_source = outcome,
           outcome = "QALYs") %>%
    group_by(setting,outcome_source,booster_vax_scenario,intervention,intervention_target_group) %>%
    summarise(mean = sum(mean),
              median = sum(median),
              sd = sum(sd),
              .groups = "keep") 
  
  ###Plot to breakdown where QALYs from
  # ggplot(QALY_breakdown) + geom_col(aes(x=outcome_source,y=mean)) +
  #   facet_grid(booster_vax_scenario ~.)
  ##############################################################################
  
  #BONUS: add hosp and death outcomes while here
  if (length(ARRAY_additional_outcomes)>0){
    additional_outcomes = TRANSLATED_antiviral_simulations %>%
      filter(outcome %in% ARRAY_additional_outcomes) %>%
      #collapsing outcome and age_group 
      group_by(setting,outcome,booster_vax_scenario,intervention,intervention_target_group) %>%
      summarise(mean = sum(mean),
                median = sum(median),
                sd = sum(sd),
                .groups = "keep")
    
    outcomes_averted = rbind(outcomes_averted,additional_outcomes)
    rm(additional_outcomes)
    
    #CHECK
    # check = outcomes_averted %>%
    #   select(-median,-sd) %>%
    #   pivot_wider(values_from = "mean",names_from = "outcome")
  }
  rm(TRANSLATED_antiviral_simulations)
  ##############################################################################

  result = list(outcomes_averted = outcomes_averted,
                QALY_breakdown = QALY_breakdown)  
  return(result)
}

#test
# outcomes_averted <- outcomes_averted_estimator(LIST_CEA_settings,toggle_discounting_rate = TOGGLE_discounting_rate)
# outcomes_averted %>% arrange(setting,booster_vax_scenario,desc(mean))

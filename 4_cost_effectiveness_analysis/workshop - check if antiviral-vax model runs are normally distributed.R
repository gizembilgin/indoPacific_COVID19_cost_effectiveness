## Step One: load model results
rootpath = str_replace(getwd(), "GitHub_vaxAllocation/4_cost_effectiveness_analysis","")
MASTER_antiviral_simulations = data.frame()

for (i in 1:length(LIST_CEA_settings)){
  this_setting = LIST_CEA_settings[[i]]
  if (this_setting == "PNG"){this_setting = "PNG_low_beta"}
  
  list_poss_Rdata = list.files(
    path = paste(rootpath, "x_results/", sep = ''),
    pattern = paste("AntiviralRun_", this_setting, "_", this_risk_group, "*", sep ="")
  )
  if (length(list_poss_Rdata) > 0) {
    list_poss_Rdata_details = double()
    for (j in 1:length(list_poss_Rdata)) {
      list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                      file.info(paste(rootpath, 'x_results/', list_poss_Rdata[[j]], sep = ''))$mtime)
    }
    latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
    load(file = paste(rootpath, "x_results/", latest_file, sep = ''))
  } else{
    stop(paste("no results for",this_setting,"with",this_risk_group,"see Translator"))
  }
  
  if (this_setting == "PNG_low_beta" & !("PNG_high_beta" %in% settings_to_plot)){this_setting = "PNG"}
  
  df_this_setting = RECORD_antiviral_model_simulations %>% mutate(setting = this_setting)
  MASTER_antiviral_simulations = bind_rows(MASTER_antiviral_simulations,df_this_setting)
}
rm(RECORD_antiviral_model_simulations)
#_______________________________________________________________________________



## Step Two: check age-specific outcomes averted distributed normally
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


#CHECK: normally distributed
#check normally distributed
shapiro_tracker = data.frame()
for (this_setting in unique(workshop$setting)){
  for (this_outcome in unique(workshop$outcome[!(workshop$outcome %in% c("mild","total_cases"))])){
    for (this_intervention in unique(workshop$intervention)){
      for (this_intervention_group in unique(workshop$intervention_target_group[workshop$intervention == this_intervention])){
        for (this_vax_scenario in unique(workshop$booster_vax_scenario[workshop$intervention == this_intervention & workshop$intervention_target_group == this_intervention_group])){
          for (this_age_group in unique(workshop$age_group)){
            this_workshop = workshop %>% 
              filter(setting == this_setting &
                       intervention == this_intervention &
                       intervention_target_group == this_intervention_group &
                       outcome == this_outcome &
                       booster_vax_scenario == this_vax_scenario &
                       age_group == this_age_group) %>%
              filter(count_outcomes_averted != 0 )
            
            if (nrow(this_workshop)>0){
              this_test <- shapiro.test(this_workshop$count_outcomes_averted   )
              
              row = data.frame(test = this_test$method, 
                               statistic = this_test$statistic,
                               p_value = this_test$p.value,
                               setting = this_setting,
                               intervention = this_intervention,
                               intervention_target_group = this_intervention_group,
                               age_group = this_age_group,
                               outcome = this_outcome,
                               booster_vax_scenario = this_vax_scenario,
                               values_tested = "count_outcomes_averted   ")
              shapiro_tracker = rbind(shapiro_tracker,row)
            }
          }
        }
      }
    }
  }
}

shapiro_tracker = shapiro_tracker %>% 
  filter(p_value < 0.05)
if (nrow(shapiro_tracker)>0){warning(paste(nrow(shapiro_tracker),"rows of count outcomes averted are not normally distributed"))}
#______________________________________________________________________________



## Step Three: check interventions doses delivered normally distributed
workshop = MASTER_antiviral_simulations %>%
  filter(is.na(age_group) == TRUE) %>%
  filter(evaluation_group == "pop_level") %>%
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
  
  #ensure one value per simulation
  filter(result == "n" & outcome == "death") %>%
  
  select(setting, booster_vax_scenario, intervention, intervention_target_group,intervention_doses_delivered)


shapiro_tracker = data.frame()
for (this_setting in unique(workshop$setting)){
  for (this_vax_scenario in unique(workshop$booster_vax_scenario)){
    for (this_intervention in unique(workshop$intervention)){
      for (this_intervention_group in unique(workshop$intervention_target_group[workshop$intervention == this_intervention])){
        this_workshop = workshop %>% 
          filter(setting == this_setting &
                   booster_vax_scenario == this_vax_scenario &
                   intervention == this_intervention &
                   intervention_target_group == this_intervention_group)
        
        if(nrow(unique(this_workshop))>1){
          this_test <- shapiro.test(this_workshop$intervention_doses_delivered)
          
          row = data.frame(test = this_test$method, 
                           statistic = this_test$statistic,
                           p_value = this_test$p.value,
                           setting = this_setting,
                           booster_vax_scenario = this_vax_scenario,
                           intervention = this_intervention,
                           intervention_target_group = this_intervention_group,
                           values_tested = "intervention_doses_delivered")
          shapiro_tracker = rbind(shapiro_tracker,row)
        }
      }
    }
  }
}

shapiro_tracker = shapiro_tracker %>% 
  filter(intervention != "booster dose 2023-03-01") %>%
  filter(p_value < 0.05)
if (nrow(shapiro_tracker)>0){warning(paste(nrow(shapiro_tracker),"rows of intervention doses delivered are not normally distributed"))}
#______________________________________________________________________________



## Step Four: healthcare outcomes averted normally distributed
workshop = MASTER_antiviral_simulations %>%
  filter(is.na(age_group) == TRUE) %>%
  filter(evaluation_group == "pop_level") %>%
  
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
  
  filter(result %in% c("n")) %>%
  rename(count_outcomes_averted = value) %>%
  filter(outcome %in% c("hosp","hosp_after_antivirals","mild")) %>%
  
  select(setting, outcome, booster_vax_scenario, intervention, intervention_target_group, count_outcomes_averted)
if (nrow(workshop) != nrow(na.omit(workshop))){stop("NA introduced")}


shapiro_tracker = data.frame()
for (this_setting in unique(workshop$setting)){
  for (this_outcome in unique(workshop$outcome[!(workshop$outcome %in% c("mild","total_cases"))])){
    for (this_intervention in unique(workshop$intervention)){
      for (this_intervention_group in unique(workshop$intervention_target_group[workshop$intervention == this_intervention])){
        for (this_vax_scenario in unique(workshop$booster_vax_scenario[workshop$intervention == this_intervention & workshop$intervention_target_group == this_intervention_group])){
            this_workshop = workshop %>% 
              filter(setting == this_setting &
                       intervention == this_intervention &
                       intervention_target_group == this_intervention_group &
                       outcome == this_outcome &
                       booster_vax_scenario == this_vax_scenario ) %>%
              filter(count_outcomes_averted != 0 )
            
            if (nrow(this_workshop)>0){
              this_test <- shapiro.test(this_workshop$count_outcomes_averted   )
              
              row = data.frame(test = this_test$method, 
                               statistic = this_test$statistic,
                               p_value = this_test$p.value,
                               setting = this_setting,
                               intervention = this_intervention,
                               intervention_target_group = this_intervention_group,
                               outcome = this_outcome,
                               booster_vax_scenario = this_vax_scenario,
                               values_tested = "count_outcomes_averted   ")
              shapiro_tracker = rbind(shapiro_tracker,row)
            }
        }
      }
    }
  }
}

shapiro_tracker = shapiro_tracker %>% 
  filter(p_value < 0.05)
if (nrow(shapiro_tracker)>0){warning(paste(nrow(shapiro_tracker),"rows of count outcomes averted are not normally distributed"))}
plot(density(this_workshop$count_outcomes_averted))

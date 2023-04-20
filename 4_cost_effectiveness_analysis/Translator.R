### This script translates the antiviral/vaccine model results into a useful from for the cost-effectiveness analysis
###
### Fundamentally we need:
### (1) the incidence of all, mild, severe, critical, and fatal disease for calculation of QALYs
### (2) the incidence of fatal disease (deaths)
### (3) the incidence of hospitalisation, and a fun
### (4) the incidence of hospitalisation of individuals who HAVE recieved antivirals
###



### Let's load the antiviral model simulations #################################
rootpath = str_replace(getwd(), "GitHub_vaxAllocation/4_cost_effectiveness_analysis","")
for (r in 1:length(risk_groups_to_plot)){
  this_risk_group = risk_groups_to_plot[r]
  for (i in 1:length(settings_to_plot)){
    this_setting = settings_to_plot[i]
    
    list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = paste("AntiviralRun_",this_setting,"_",this_risk_group,"*",sep=""))
    if (length(list_poss_Rdata)>0){
      list_poss_Rdata_details = double()
      for (j in 1:length(list_poss_Rdata)){
        list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                        file.info(paste(rootpath,'x_results/',list_poss_Rdata[[j]],sep=''))$mtime)
      }
      latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
      load(file = paste(rootpath,"x_results/",latest_file,sep=''))
      
      if (this_setting == "PNG_low_beta" & !("PNG_high_beta" %in% settings_to_plot)){this_setting = "PNG"}
      
      df_this_setting = RECORD_antiviral_model_simulations %>% mutate(setting_beta = this_setting)
      MASTER_RECORD_antiviral_model_simulations = bind_rows(MASTER_RECORD_antiviral_model_simulations,df_this_setting)
    }
  }
}
#_______________________________________________________________________________



### Subset #####################################################################
### We would like a dataset with the following columns:
### setting, outcome, booster_vax_scenario, intervention, intervention target group, 
### intervention_doses_delivered, count_outcomes_averted

workshop = MASTER_RECORD_antiviral_model_simulations %>%
  rename(setting = country) %>%
  
  mutate(booster_vax_scenario = case_when( 
    vax_scenario == "catchup campaign for high-risk adults: assume booster to high-risk adults who have previously completed their primary schedule but have not recieved a booster"  ~ "booster dose catch-up campaign for high-risk adults",           
    vax_scenario == "catchup campaign for all adults: assume booster to all adults who have previously completed their primary schedule but have not recieved a booster" ~ "booster dose catch-up campaign for all adults",                       
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults" ~ "booster to all adults, prioritised to high-risk adults",                                                           
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~ "booster to all adults previously willing to be vaccinated",                      
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~ "booster to all high-risk adults previously willing to be vaccinated",  
    vax_scenario == "all willing adults vaccinated with a primary schedule" ~ "no booster dose"           
  )) %>%
  filter(is.na(booster_vax_scenario) == FALSE) %>%
  
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
    
  mutate(doses_per_outcome_averted = intervention_doses_delivered/value) %>%
  rename(count_outcomes_averted = value) %>%
  
  filter(! (outcome %in% c("YLL","neonatal_deaths","booster_doses_delivered","ICU"))) %>%
  
  select(setting, outcome, booster_vax_scenario, intervention, intervention_target_group, intervention_doses_delivered,count_outcomes_averted,doses_per_outcome_averted)

if (nrow(workshop) != nrow(na.omit(workshop))){stop("NA introduced")}

unique(workshop$setting)
#[1] "FJI" "PNG" "TLS"
unique(workshop$outcome)
#[1] "death"            "hosp"             "severe_disease"   "critical_disease"
unique(workshop$booster_vax_scenario)
# [1] "booster dose catch-up campaign for high-risk adults"                 "booster dose catch-up campaign for all adults"                      
# [3] "booster to all adults, prioritised to high-risk adults"              "booster to all adults previously willing to be vaccinated"          
# [5] "booster to all high-risk adults previously willing to be vaccinated" "no booster dose"                                                   
unique(workshop$intervention)
#[1] "nirmatrelvir_ritonavir 2023-01-01" "molunipiravir 2023-01-01"          "booster dose 2023-03-01"          
unique(workshop$intervention_target_group)
#[1] "adults_with_comorbidities"                         "unvaccinated_adults_AND_adults_with_comorbidities" "all_adults"                                       
#[4] "unvaccinated_adults"                               "pregnant_women"      

### DECISION: do we calculate uncertainty here, and propagate through this measure OR use these simulations going forward?

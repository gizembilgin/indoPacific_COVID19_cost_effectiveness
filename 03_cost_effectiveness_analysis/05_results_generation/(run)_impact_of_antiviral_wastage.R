# This scripts runs the model with varying levels of antiviral wastage for the Supplementary Material

queue = list(
  list(wastage_rate_antiviralSchedule = 0.0),
  list(wastage_rate_antiviralSchedule = 0.2),
  list(wastage_rate_antiviralSchedule = 0.4),
  list(wastage_rate_antiviralSchedule = 0.6)
)

antiviral_wastage_results = data.frame()
CommandDeck_CONTROLS = list()

for (ticket in 1:length(queue)){
  CommandDeck_CONTROLS <- queue[[ticket]]
  CommandDeck_CONTROLS <- append(
    CommandDeck_CONTROLS,
    list(
      LIST_CEA_settings = list("PNG_low_beta", "TLS", "FJI", "IDN"),
      LIST_perspectives = c("healthcare", "societal"),
      LIST_antiviral_cost_scenario = c("low_generic_cost", "middle_income_cost", "high_income_cost"),
      LIST_discounting_rate = 0.03,
      TOGGLE_longCOVID = "off",
      
      LIST_booster_vax_scenarios = list(
        "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule",
        "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule",
        "all willing adults vaccinated with a primary schedule"
      ),
      LIST_antiviral_elig_groups = list("adults_with_comorbidities"),
      LIST_antiviral_types = list("nirmatrelvir_ritonavir"),
      
      TOGGLE_uncertainty = "rand",
      TOGGLE_numberOfRuns = 1000,
      TOGGLE_clusterNumber = 3,
      DECISION_include_net = "N",
      DECISION_sampling_strategy = "single_run"
    )
  )

  source(paste0(getwd(),"/CommandDeck.R"))
  
  rows <- CommandDeck_result_long %>% #CEAC_dataframe
    filter(evaluation_level == "incremental" &
             cost_per_outcome_averted != -Inf &
             cost_per_outcome_averted != Inf) %>%
    group_by(outcome, setting, perspective, discounting_rate, antiviral_cost_scenario,
             booster_vax_scenario, antiviral_type, antiviral_target_group) %>%
    arrange(cost_per_outcome_averted) %>%
    mutate(row_number = row_number(),
           probability = row_number/TOGGLE_numberOfRuns) %>%
    rename(WTP = cost_per_outcome_averted) %>%
    select(outcome, setting, perspective, discounting_rate, antiviral_cost_scenario,
           booster_vax_scenario, antiviral_type, antiviral_target_group, probability, WTP) %>%
    mutate(probability = round(probability,digits=2)) %>%
    group_by(outcome, setting, perspective, discounting_rate, antiviral_cost_scenario,
             booster_vax_scenario, antiviral_type, antiviral_target_group, probability) %>%
    summarise(WTP = mean(WTP),
              .groups = "keep") %>%
    ungroup()
    
  rows <- rows %>%
    mutate(antiviral_wastage_rate = CommandDeck_CONTROLS$wastage_rate_antiviralSchedule) 
  
  antiviral_wastage_results <- rbind(antiviral_wastage_results,rows)
  
}

CommandDeck_CONTROLS <- list()



antiviral_wastage_results <- antiviral_wastage_results %>%
  mutate(
    setting = case_when(
      setting == "FJI" ~ "Fiji",
      setting == "IDN" ~ "Indonesia",
      setting == "PNG" ~ "Papua New Guinea",
      setting == "TLS" ~ "Timor-Leste",
      TRUE ~ setting
    ),
    
    booster_vax_scenario = case_when(
      booster_vax_scenario == "booster to all high-risk adults previously willing to be vaccinated" ~ "high risk adults",
      booster_vax_scenario == "booster to all adults previously willing to be vaccinated" ~ "all adults"    ,
      booster_vax_scenario == "booster dose catch-up campaign for all adults" ~ "all adults who have previously completed their primary schedule but have not recieved a booster"  ,
      booster_vax_scenario == "booster dose catch-up campaign for high-risk adults" ~ "high-risk adults who have previously completed their primary schedule but have not recieved a booster"   ,
      booster_vax_scenario == "no booster dose" ~ "no booster",
      TRUE ~ booster_vax_scenario
    ),
    
    antiviral_type = gsub(" 2023-01-01","",antiviral_type),
    antiviral_target_group = gsub("_"," ",antiviral_target_group),
    antiviral_target_group = case_when(
      antiviral_type == "no antiviral" ~ antiviral_type,
      TRUE ~ antiviral_target_group
    ),
    
    perspective = paste0(perspective," perspective"),
    
    antiviral_cost_scenario = 
           case_when(antiviral_cost_scenario == "high_income_cost" ~ "high-income reference price ($530 USD per schedule)",
                     antiviral_cost_scenario == "middle_income_cost" ~ "middle-income reference price ($250 USD per schedule)",
                     antiviral_cost_scenario == "low_generic_cost" ~ "low generic reference price ($25 USD per schedule)"),
    
    discounting_rate = discounting_rate *100
    )

antiviral_wastage_results$antiviral_cost_scenario <- factor(antiviral_wastage_results$antiviral_cost_scenario, 
                                                            levels = rev(c("low generic reference price ($25 USD per schedule)",
                                                                           "middle-income reference price ($250 USD per schedule)",
                                                                           "high-income reference price ($530 USD per schedule)")))

save(antiviral_wastage_results,file = "07_shiny/x_results/antiviral_wastage_results.Rdata")
#_______________________________________________________________________________
# This scripts runs the model with and without the inclusion of QALYs associated
# with long COVID. The results are presented in the Supplementary Material

queue = list(
  list(TOGGLE_longCOVID = "off"),
  list(TOGGLE_longCOVID = "on")
)

long_COVID_results = data.frame()
CommandDeck_CONTROLS = list()

for (ticket in 1:length(queue)){
  CommandDeck_CONTROLS = queue[[ticket]]
  CommandDeck_CONTROLS = append(CommandDeck_CONTROLS,
                                list(
                                  LIST_CEA_settings = list("PNG_low_beta","TLS","FJI","IDN"),
                                  LIST_perspectives = c("healthcare","societal"),
                                  LIST_antiviral_cost_scenario = c("low_generic_cost","middle_income_cost", "high_income_cost"),
                                  LIST_discounting_rate = 0.03,
                                  
                                  LIST_booster_vax_scenarios = list(
                                    "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule"
                                    ,"all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule"                    
                                    ,"all willing adults vaccinated with a primary schedule"  
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
  
  rows = CommandDeck_result_long  %>%
    mutate(long_COVID = CommandDeck_CONTROLS$TOGGLE_longCOVID) 
  
  long_COVID_results = rbind(long_COVID_results,rows)
  
}

CommandDeck_CONTROLS = list()

long_COVID_results = long_COVID_results %>%
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

long_COVID_results$antiviral_cost_scenario <- factor(long_COVID_results$antiviral_cost_scenario, levels = rev(c("low generic reference price ($25 USD per schedule)",
                                                                                                                              "middle-income reference price ($250 USD per schedule)",
                                                                                                                              "high-income reference price ($530 USD per schedule)")))
save(long_COVID_results,file = "07_shiny/x_results/long_COVID_results.Rdata")
#_______________________________________________________________________________
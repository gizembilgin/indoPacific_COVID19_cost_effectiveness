# This scripts runs one-way sensitivity analysis using the lower and upper estimates
# for each parameter, one at a time. The results of this script are visualised
# in the tornado plot.


queue <- list(
  
  ## (1/3) healthcare costs averted
  list(cost_per_extra_LOS = 0.5, label = "Cost per extra LOS (±50%)",direction = "lower"),
  list(cost_per_extra_LOS = 1.5, label = "Cost per extra LOS (±50%)",direction = "upper"),
  
  list(extra_LOS  = 0.5, label = "Reduced LOS (±50%)",direction = "lower"),
  list(extra_LOS  = 1.5, label = "Reduced LOS (±50%)",direction = "upper"),
  
  list(outpatient = 0.5, label = "Outpatient costs (±50%)",direction = "lower"),
  list(outpatient = 1.5, label = "Outpatient costs (±50%)",direction = "upper"),
  
  list(inpatient = 0.5, label = "Inpatient costs (±50%)",direction = "lower"),
  list(inpatient = 1.5, label = "Inpatient costs (±50%)",direction = "upper"),
  
  
  ## (2/3) intervention costs
  #(A/B) booster program costs
  list(price_per_boosterDose  = 0.5, label = "Booster price ($0.50-$3.00)",direction = "lower"),
  list(price_per_boosterDose  = 3.0, label = "Booster price ($0.50-$3.00)",direction = "upper"),
  
  list(wastage_rate_boosterDose = 0.0, label = "Booster wastage (0-50%)",direction = "lower"),
  list(wastage_rate_boosterDose = 0.5, label = "Booster wastage (0-50%)",direction = "upper"),
  
  list(price_per_injectionEquipmentDose = 0.025, label = "Injection Equipment price ($0.025-$0.050)",direction = "lower"),
  list(price_per_injectionEquipmentDose = 0.050, label = "Injection Equipment price ($0.025-$0.050)",direction = "upper"),
  
  list(wastage_rate_injectionEquipment = 0.0, label = "Injection Equipment wastage (0-50%)",direction = "lower"),
  list(wastage_rate_injectionEquipment = 0.5, label = "Injection Equipment wastage (0-50%)",direction = "upper"),
  
  list(vax_operational_cost = 00.21, label = "Booster operational cost ($0.21-$13.04)",direction = "lower"),
  list(vax_operational_cost = 13.04, label = "Booster operational cost ($0.21-$13.04)",direction = "upper"),
  
  #(B/B) antiviral program costs
  list(price_per_antiviralDose = 25,  label = "Antiviral schedule price ($25-530)",direction = "lower"),
  list(price_per_antiviralDose = 530, label = "Antiviral schedule price ($25-530)",direction = "upper"),
  
  list(wastage_rate_antiviralSchedule = 0.0, label = "Antiviral wastage (0-60%)",direction = "lower"),
  list(wastage_rate_antiviralSchedule = 0.6, label = "Antiviral wastage (0-60%)",direction = "upper"),
  
  list(price_per_RAT = 1, label = "RAT price ($1-5)",direction = "lower"),
  list(price_per_RAT = 5, label = "RAT price ($1-5)",direction = "upper"),
  
  list(wastage_factor_RAT = 3,  label = "RAT wastage factor (3-12)",direction = "lower"),
  list(wastage_factor_RAT = 12, label = "RAT wastage factor (3-12)",direction = "upper"),
  
  list(antiviral_operational_cost = 0.5, label = "Antiviral operational costs (±50%)",direction = "lower"),
  list(antiviral_operational_cost = 1.5, label = "Antiviral operational costs (±50%)",direction = "upper"),
  
  
  ## (3/3) other toggles
  list(LIST_discounting_rate = 0.0, label = "Discounting rate (0-5%)",direction = "lower"),
  list(LIST_discounting_rate = 0.05, label = "Discounting rate (0-5%)",direction = "upper"),
  
  list(TOGGLE_longCOVID = "off", label = "Long COVID (off/on)",direction = "lower"),
  list(TOGGLE_longCOVID = "on", label = "Long COVID (off/on)",direction = "upper"),
  
  list(productivity_loss_illness = 0.5, label = "Productivity loss due to illness (±50%)",direction = "lower"),
  list(productivity_loss_illness = 1.5, label = "Productivity loss due to illness (±50%)",direction = "upper"),
  
  list(productivity_loss_death = 0.5, label = "Productivity loss due to death (±50%)",direction = "lower"),
  list(productivity_loss_death = 1.5, label = "Productivity loss due to death (±50%)",direction = "upper")
  
)

tornado_result = data.frame()
CommandDeck_CONTROLS = list()

for (ticket in 1:length(queue)){
    CommandDeck_CONTROLS <- queue[[ticket]]
    CommandDeck_CONTROLS <- append(CommandDeck_CONTROLS,
                                  list(
                                    LIST_CEA_settings = list("PNG_low_beta","TLS","FJI","IDN"),
                                    LIST_perspectives = c("healthcare","societal"),
                                    LIST_antiviral_cost_scenario = c("middle_income_cost"),
                                    TOGGLE_uncertainty = "fixed",
                                    TOGGLE_numberOfRuns = 1, 
                                    TOGGLE_clusterNumber = 1,
                                    DECISION_include_net = "N",
                                    DECISION_sampling_strategy = "single_run"
                                  )
    )
    
    if (!("LIST_discounting_rate" %in% names(CommandDeck_CONTROLS))) CommandDeck_CONTROLS = append(CommandDeck_CONTROLS, list(LIST_discounting_rate = 0.03))
    if (!("TOGGLE_longCOVID" %in% names(CommandDeck_CONTROLS)))      CommandDeck_CONTROLS = append(CommandDeck_CONTROLS, list(TOGGLE_longCOVID = "off"))
    
    source(paste0(getwd(),"/CommandDeck.R"))
    
    rows <- CommandDeck_result %>%
      filter(variable_type == "ICER") %>%
      mutate(label = CommandDeck_CONTROLS$label,
             direction = CommandDeck_CONTROLS$direction) 
    tornado_result <- rbind(tornado_result,rows)
    
}

CommandDeck_CONTROLS = list()



tornado_result <- tornado_result %>%
  mutate(
    setting = case_when(
      setting == "FJI" ~ "Fiji",
      setting == "IDN" ~ "Indonesia",
      setting == "PNG" ~ "Papua New Guinea",
      setting == "TLS" ~ "Timor-Leste",
      TRUE ~ setting
    ),
    
    booster_vax_scenario = case_when(
      booster_vax_scenario == "booster to all high-risk adults previously willing to be vaccinated" ~ "high-risk adults",
      booster_vax_scenario == "booster to all adults previously willing to be vaccinated" ~ "all adults"    ,
      booster_vax_scenario == "booster dose catch-up campaign for all adults" ~ "all adults (catch-up campaign)"  ,
      booster_vax_scenario == "booster dose catch-up campaign for high-risk adults" ~ "high-risk adults (catch-up campaign)",
      booster_vax_scenario == "no booster dose" ~ "no booster",
      TRUE ~ booster_vax_scenario
    ),
    
    antiviral_type = gsub(" 2023-01-01","",antiviral_type),
    antiviral_target_group = gsub("_"," ",antiviral_target_group),
    antiviral_target_group = case_when(
      antiviral_type == "no antiviral" ~ antiviral_type,
      antiviral_target_group == "adults with comorbidities" ~ "high-risk adults",
      TRUE ~ antiviral_target_group
    ),

    perspective = paste0(perspective," perspective")      
  ) %>%
  filter(!(antiviral_type ==  "molunipiravir" & variable == "cost_per_hosp_averted")) #these results don't make any sense as molnupiravir is not effective against hospitalisation

save(tornado_result,file = "07_shiny/x_results/tornado_result.Rdata")
beep()
#_______________________________________________________________________________
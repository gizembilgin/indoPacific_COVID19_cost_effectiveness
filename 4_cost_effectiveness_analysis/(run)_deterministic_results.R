### RUN SCENARIOS FOR TORNADO PLOT
##Note: will use LIST_CEA_settings from CommandDeck (line 15)


queue = list(
  
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
  list(TOGGLE_discounting_rate = 0.0, label = "Discounting rate (0-5%)",direction = "lower"),
  list(TOGGLE_discounting_rate = 0.05, label = "Discounting rate (0-5%)",direction = "upper"),
  
  list(TOGGLE_longCOVID = "off", label = "Long COVID (off/on)",direction = "lower"),
  list(TOGGLE_longCOVID = "on", label = "Long COVID (off/on)",direction = "upper")
  
)

tornado_result = data.frame()

for (this_perspective in c("healthcare","societal")){
  for (ticket in 1:length(queue)){
    CommandDeck_CONTROLS = queue[[ticket]]
    CommandDeck_CONTROLS = append(CommandDeck_CONTROLS,
                                  list(
                                    LIST_booster_vax_scenarios = list(
                                      "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule"
                                    ),
                                    LIST_antiviral_elig_groups = list("adults_with_comorbidities"),
                                    LIST_antiviral_types = list("nirmatrelvir_ritonavir"),
                                    TOGGLE_uncertainty = "fixed",
                                    TOGGLE_antiviral_cost_scenario = "middle_income_cost",
                                    TOGGLE_perspective = this_perspective
                                  )
    )
    
    if(!("TOGGLE_discounting_rate" %in% names(CommandDeck_CONTROLS))){CommandDeck_CONTROLS = append(CommandDeck_CONTROLS,
                                                                                                    list(TOGGLE_discounting_rate = 0.03))}
    if(!("TOGGLE_longCOVID" %in% names(CommandDeck_CONTROLS))){CommandDeck_CONTROLS = append(CommandDeck_CONTROLS,
                                                                                             list(TOGGLE_longCOVID = "off"))}
    
    source(paste(getwd(),"/CommandDeck.R",sep=""))
    
    rows = CommandDeck_result %>%
      filter(variable_type == "ICER") %>%
      mutate(label = CommandDeck_CONTROLS$label,
             direction = CommandDeck_CONTROLS$direction,
             perspective = this_perspective) 
    tornado_result = rbind(tornado_result,rows)
    
  }
}

CommandDeck_CONTROLS = list()

save(tornado_result,file = "x_results/tornado_result.Rdata")
#_______________________________________________________________________________
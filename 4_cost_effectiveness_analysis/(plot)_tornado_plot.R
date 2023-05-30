
this_antiviral_cost_estimate = "middle_income_cost"# options: low_generic_cost,middle_income_cost, high_income_cost
this_setting_GDP = 4332.71

tornado_plot = list()

### RUN SCENARIOS
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
  list(TOGGLE_discounting_rate = 0.5, label = "Discounting rate (0-5%)",direction = "upper"),
  
  list(TOGGLE_longCOVID = "off", label = "Long COVID (off/on)",direction = "lower"),
  list(TOGGLE_longCOVID = "on", label = "Long COVID (off/on)",direction = "upper")
  
)

tornado_result = data.frame()

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
                                  TOGGLE_antiviral_cost_scenario = this_antiviral_cost_estimate
                                  )
                                )

  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  rows = CommandDeck_result %>%
    mutate(label = CommandDeck_CONTROLS$label,
           direction = CommandDeck_CONTROLS$direction) %>%
    select(-interventionCost,-healthcareCostAverted,-count_outcomes_averted,-netCost)
  tornado_result = rbind(tornado_result,rows)
  
}

CommandDeck_CONTROLS = list()
#_______________________________________________________________________________



### PLOT
to_plot = tornado_result %>%
  filter(antiviral_scenario != "no antiviral" &
           outcome == "QALYs") 

base.value <- to_plot$cost_per_outcome_averted[to_plot$direction == "lower" & 
                                                 to_plot$label == "Long COVID (off/on)" ] # final value was baseline estimates

# width of columns in plot (value between 0 and 1)
width <- 0.95
order_parameters <- to_plot %>%
  select(label,cost_per_outcome_averted,direction) %>%
  group_by(label) %>%
  summarise(LB = min(cost_per_outcome_averted),
            UB = max(cost_per_outcome_averted)) %>%
  mutate(UL_Difference = UB - LB) %>% 
  arrange(UL_Difference) %>%
  mutate(label=factor(x=label, levels=label)) %>%
  select(label) %>% 
  unlist() %>% 
  levels()

# get data frame in shape for ggplot and geom_rect
df_2 <- to_plot %>%
  select(label,cost_per_outcome_averted,direction) %>% 
  rename(value = cost_per_outcome_averted) %>%
  ungroup() %>%
  # create the columns for geom_rect
  mutate(label=factor(label, levels=order_parameters),
         ymin=pmin(value, base.value),
         ymax=pmax(value, base.value),
         xmin=as.numeric(label)-width/2,
         xmax=as.numeric(label)+width/2)

# create plot
require(ggtext)
options(scipen=999) #turn off scientific notation

ggplot() + 
  geom_rect(data = df_2, 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=paste(direction,"estimate"))) + 
  geom_hline(yintercept = base.value) +
  theme_bw() + 
  #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank())  +
  ylab('Cost per QALY averted (2022 USD)') +
  scale_x_continuous(breaks = c(1:length(order_parameters)), 
                     labels = order_parameters) +
  coord_flip() + 
  geom_hline(mapping = NULL, yintercept = this_setting_GDP, linetype='dashed') +
  annotate("text", x = 4, y = this_setting_GDP*0.65, label = "GDP per capita")

ggplot() + 
  geom_rect(data = df_2[!(df_2$label %in% c("Antiviral wastage (0-60%)","Antiviral schedule price ($25-530)","Inpatient costs (±50%)","Discounting rate (0-5%)")),], 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=paste(direction,"estimate"))) + 
  geom_hline(yintercept = base.value) +
  theme_bw() + 
  #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank())  +
  ylab('Cost per QALY averted (2022 USD)') +
  scale_x_continuous(breaks = c(1:length(order_parameters)), 
                     labels = order_parameters) +
  coord_flip() #+ 
  #geom_hline(mapping = NULL, yintercept = this_setting_GDP, linetype='dashed')

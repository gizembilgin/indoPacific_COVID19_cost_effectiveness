# This script explore how strongly different outcomes contribute to outcomes averted,
# productivity losses, healthcare costs and intervention costs. The results of this
# script are visualized in figures S3.1 and S3.2 of the Supplementary Material.
options(scipen = 1000)

CommandDeck_CONTROLS = list(
  LIST_CEA_settings = list("PNG_low_beta", "TLS", "FJI", "IDN"),
  LIST_perspectives = c("societal"),
  LIST_antiviral_cost_scenario = c("low_generic_cost","middle_income_cost", "high_income_cost"),
  #LIST_antiviral_cost_scenario = c("middle_income_cost"),
  LIST_discounting_rate = 0.03,
  
  LIST_booster_vax_scenarios = list(
    #"all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule"
    "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule"
    #"all willing adults vaccinated with a primary schedule"
  ),
  LIST_antiviral_elig_groups = list("adults_with_comorbidities"),
  LIST_antiviral_types = list("nirmatrelvir_ritonavir"),
  
  TOGGLE_uncertainty = "fixed",
  TOGGLE_numberOfRuns = 1,
  TOGGLE_clusterNumber = 1,
  DECISION_include_net = "N",
  DECISION_sampling_strategy = "single_run"
)

#run start of CommandDeck, and inside of CEA_worker
#_______________________________________________________________________________


### Figure S3.1
figure_S3_1 <- list()

figure_S3_1 [[1]] <- ggplot(outcomesAvertedEstimation$QALY_breakdown) + 
  geom_col(aes(x=outcome_source,y=count_outcomes)) +
  facet_grid(setting ~., scales = "free") + 
  this_plot_standard + 
  labs(title = "QALYs averted") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("") 

interventionCost_estimates <- interventionCost_estimates %>% 
  mutate(antiviral_cost_scenario = 
           case_when(antiviral_cost_scenario == "high_income_cost" ~ "high-income reference price ($530 USD per schedule)",
                     antiviral_cost_scenario == "middle_income_cost" ~ "middle-income reference price ($250 USD per schedule)",
                     antiviral_cost_scenario == "low_generic_cost" ~ "low generic reference price ($25 USD per schedule)"))
interventionCost_estimates$antiviral_cost_scenario <- factor(interventionCost_estimates$antiviral_cost_scenario, 
                                                                levels = c("low generic reference price ($25 USD per schedule)",   "middle-income reference price ($250 USD per schedule)",   "high-income reference price ($530 USD per schedule)"))
figure_S3_1 [[2]] <- ggplot(interventionCost_estimates) + 
  geom_col(aes(x=gsub(" 2023-01-01","",gsub(" 2023-03-01","",intervention)),y=cost,fill=as.factor(antiviral_cost_scenario)),position = "dodge") +
  scale_fill_manual(values = c("low generic reference price ($25 USD per schedule)" = "#46acc8",
                               "middle-income reference price ($250 USD per schedule)" = "#e2d200",
                               "high-income reference price ($530 USD per schedule)" = "#dd8c29"))+
  facet_grid(setting ~., scales = "free") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("")+
  labs(fill = "antiviral cost scenario",title = "intervention costs")
this_legend = get_legend(figure_S3_1 [[2]])
figure_S3_1 [[2]] <- figure_S3_1 [[2]] +  theme(legend.position="none") 

figure_S3_1 [[3]] <- ggplot(productivityCosts$productivity_loss_breakdown) + 
  geom_col(aes(x=productivity_loss_category,y=cost)) +
  facet_grid(setting ~., scales = "free") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("")+ 
  labs(title = "productivity losses prevented") 

figure_S3_1 [[4]] <- ggplot(healthcareCostEstimation$healthcareCosts_breakdown) + 
  geom_col(aes(x=patient_type,y=cost)) +
  facet_grid(setting ~., scales = "free") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("")+
  labs(title = "healthcare costs averted") 
#_______________________________________________________________________________


### Figure S3.2
net_cost_by_component = data.frame()

#collecting costs by components in a unified dataset
workshop <- interventionCost_estimates %>%
  group_by(setting,intervention) %>%
  summarise(cost = sum(cost), .groups = "keep") %>% ungroup() %>%
  mutate(cost_category = paste0("intervention cost (",gsub(" 2023-01-01","",gsub(" 2023-03-01","",intervention)),")")) %>%
  select(-intervention)
net_cost_by_component <- rbind(net_cost_by_component,workshop)

workshop <- healthcareCostEstimation$healthcareCosts_breakdown %>%
  group_by(setting,patient_type) %>%
  summarise(cost = sum(cost), .groups = "keep") %>% ungroup() %>%
  mutate(cost_category = paste0("healthcare costs averted (",patient_type,")")) %>%
  select(-patient_type) %>%
  mutate(cost = -cost)
net_cost_by_component <- rbind(net_cost_by_component,workshop)

workshop <- productivityCosts$productivity_loss_breakdown %>%
  group_by(setting,productivity_loss_category) %>%
  summarise(cost = sum(cost), .groups = "keep") %>% ungroup() %>%
  mutate(cost_category = paste0("productivity losses averted (",productivity_loss_category,")")) %>%
  select(-productivity_loss_category)%>%
  mutate(cost = -cost)
net_cost_by_component <- rbind(net_cost_by_component,workshop)

#include net cost by perspective
# workshop_societal = net_cost_by_component %>%
#   group_by(setting) %>%
#   summarise(cost = sum(cost)) %>%
#   mutate(cost_category = "total incremental cost (societal perspective)")
# workshop_healthcare = net_cost_by_component %>%
#   filter(str_detect(cost_category,"productivity losses",negate=TRUE)) %>%
#   group_by(setting) %>%
#   summarise(cost = sum(cost)) %>%
#   mutate(cost_category = "total incremental cost (healthcare perspective)")
# net_cost_by_component = rbind(net_cost_by_component,workshop_societal,workshop_healthcare)

#formatting data frame for cleaner plot
net_cost_by_component$cost_category <- factor(
  net_cost_by_component$cost_category,
  levels = rev(c("intervention cost (booster dose)",
                 "intervention cost (nirmatrelvir_ritonavir)",
                 "healthcare costs averted (inpatient)",
                 "healthcare costs averted (outpatient)"  ,
                 "productivity losses averted (death)",
                 "productivity losses averted (critical_disease)" ,
                 "productivity losses averted (severe_disease)"   ,
                 "productivity losses averted (mild)"   ,
                 "productivity losses averted (illness)",
                 "total incremental cost (healthcare perspective)",
                 "total incremental cost (societal perspective)")))
net_cost_by_component <- net_cost_by_component %>% 
  arrange(cost_category) %>%
  mutate(
    setting = case_when(
      setting == "FJI" ~ "Fiji",
      setting == "IDN" ~ "Indonesia",
      setting == "PNG" ~ "Papua New Guinea",
      setting == "TLS" ~ "Timor-Leste",
      TRUE ~ setting
    ))

figure_S3_2 = list()
for (this_setting in unique(net_cost_by_component$setting)){
  figure_S3_2[[length(figure_S3_2) + 1]] <- ggplot(net_cost_by_component[net_cost_by_component$setting == this_setting,]) + 
    geom_col(aes(y = cost_category,
                 x = cost,
                 fill = as.factor(cost_category))) +
    theme(legend.position="none") +
    ylab("") +
    labs(title = paste(this_setting)) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
}
#_______________________________________________________________________________


### Print figures
ggarrange(plotlist = figure_S3_1, ncol = 2, nrow = 2,
          legend.grob = this_legend,
          legend = "bottom")
ggarrange(plotlist = figure_S3_2, ncol = 1, nrow = 4,  legend = "none")

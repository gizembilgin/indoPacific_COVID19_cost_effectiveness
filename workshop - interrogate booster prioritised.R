### SM - booster prioritised
LIST_outcomes = list('hosp', 'death')

this_result = "doses_per_outcome_averted" #left-hand side of figure
this_result = "percentage"                #right-hand side of figure

workshop = RECORD_antiviral_model_simulations %>% 
  filter(vax_scenario_risk_group == "adults_with_comorbidities") %>%
  filter(antiviral_type == "nirmatrelvir_ritonavir" | intervention == 'vaccine') %>%
  filter(antiviral_target_group == 'adults_with_comorbidities' | intervention == 'vaccine') %>%
  filter( evaluation_group == 'pop_level') %>% #change eval group here to change from high-risk to pop-level plot
  filter(result == this_result) %>%
  filter( vax_scenario_short %in% c(
    "booster to all (prioritised)",
    "booster to all",
    "booster to high-risk",
    "no booster")) %>%
  mutate(intervention = case_when(
    intervention == 'vaccine' ~ paste('booster dose'),
    intervention ==  "antiviral 2023-01-01" ~  "oral antiviral",
    TRUE ~ intervention
  )) %>% 
  filter(intervention %in% c('booster dose','oral antiviral')) %>%
  group_by(setting_beta,intervention,outcome,antiviral_type,antiviral_target_group,evaluation_group,vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
  summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75))
workshop$vax_scenario_short = factor(workshop$vax_scenario_short, levels = c(
  "booster to all (prioritised)",
  "booster to all",
  "booster to high-risk",
  "no booster"
))

options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  plot_list[[a]] = ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_pointrange(aes(x=median,y=vax_scenario_short,
                        xmin=LQ,xmax=UQ,
                        color=as.factor(intervention)))  +
    labs(title = paste(this_outcome),
         color = 'intervention',
         shape = "setting") +
    ylab('')+
    xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical") +
    facet_grid(setting_beta ~.)
  if(this_result == "percentage"){plot_list[[a]]  = plot_list[[a]]  + xlab('outcomes averted (%)')}
  if(this_result == "doses_per_outcome_averted"){plot_list[[a]]  = plot_list[[a]]  + xlab('doses to avert an outcome')}
  
}
ggarrange(plot_list[[1]],plot_list[[2]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = length(LIST_outcomes)) 
options(warn = 0)
#copy plot 500 x 700 for paper, 500 x 900 for additional two outcomes
#_______________________________________________________________________________


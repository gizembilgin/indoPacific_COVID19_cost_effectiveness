options(scipen = 1000); require(ggpubr)


INPUT_setting_list = c("PNG","TLS")
INPUT_outcome = "QALYs"
INPUT_perspective = "societal"
INPUT_parameter_to_vary = "booster_strategy" 

#R SHINY conditional UI, check boxes for one and button for all others
INPUT_discounting_rate = 0.03
INPUT_antiviral_cost = "middle_income_cost"
INPUT_booster_strategy = "booster to all high-risk adults previously willing to be vaccinated"
INPUT_antiviral_strategy = c("no antiviral","nirmatrelvir_ritonavir 2023-01-01 adults_with_comorbidities")



### Create plots
this_workshop = CEAC_dataframe %>% filter(outcome == INPUT_outcome &
                                            setting %in% INPUT_setting_list &
                                            perspective == INPUT_perspective &
                                            discounting_rate == INPUT_discounting_rate &
                                            antiviral_cost == INPUT_antiviral_cost &
                                            booster_vax_scenario %in% INPUT_booster_strategy &
                                            antiviral_scenario %in% INPUT_antiviral_strategy)
xmax = this_workshop %>% 
  group_by(setting,booster_vax_scenario,antiviral_scenario) %>%
  filter(probability >=1) %>%
  summarise(min = min(WTP), .groups = "keep") %>%
  ungroup()
xmax = max(xmax$min)

xmin = this_workshop %>% 
  group_by(setting,booster_vax_scenario,antiviral_scenario) %>%
  filter(probability == min(probability)) %>%
  summarise(max = max(WTP), .groups = "keep") %>%
  ungroup()
xmin = min(xmin$max)

n_options_selected = max(length(INPUT_discounting_rate),
                         length(INPUT_antiviral_cost),
                         length(INPUT_booster_strategy),
                         length(INPUT_antiviral_strategy))
  
plot_list = list()
for (this_setting in INPUT_setting_list){
  to_plot_setting = this_workshop[this_workshop$setting == this_setting,]
  if (length(INPUT_antiviral_strategy)>1){
    plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
      geom_point(aes(x=WTP,y=probability,color=as.factor(antiviral_scenario))) +
      labs(color="antiviral strategy") 
  } else if (length(INPUT_booster_strategy)>1){
    plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
      geom_point(aes(x=WTP,y=probability,color=as.factor(booster_vax_scenario))) +
      labs(color="booster strategy")
  } else if (length(INPUT_discounting_rate)>1){
    plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
      geom_point(aes(x=WTP,y=probability,color=as.factor(discounting_rate))) +
      labs(color="discounting rate")
  } else if (length(INPUT_antiviral_cost)>1){
    plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
      geom_point(aes(x=WTP,y=probability,color=as.factor(antiviral_cost))) +
      labs(color="antiviral cost")
  } else{
    plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
      geom_point(aes(x=WTP,y=probability)) 
  }
  plot_list[[length(plot_list)]] = plot_list[[length(plot_list)]] +
    xlab("Willingness to pay ($/QALY)") +
    ylab("Probability cost-effective") +
    theme_bw() + 
    theme(legend.position="bottom") +
    labs(title = this_setting) +
    xlim(xmin,xmax) + guides(colour = guide_legend(nrow = n_options_selected))
}



### Arrange plots based no number of settings
if (length(plot_list) == 1){
  plot_list
} else if (length(plot_list) == 2){
  ggarrange(plot_list[[1]],plot_list[[2]], ncol = 1, nrow = 2, common.legend = TRUE)
} else if (length(plot_list) == 3){
  ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], ncol = 2, nrow = 2, common.legend = TRUE)
} else if (length(plot_list) == 4){
  ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]], ncol = 2, nrow = 2, common.legend = TRUE)
}

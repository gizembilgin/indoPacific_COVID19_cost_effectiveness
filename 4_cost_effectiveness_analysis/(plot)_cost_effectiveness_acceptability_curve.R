options(scipen = 1000); require(ggpubr)


INPUT_include_setting = c("Papua New Guinea")
INPUT_include_outcome = "QALYs"
INPUT_perspective = "healthcare perspective"

INPUT_parameter_to_vary = "booster_strategy" 

#R SHINY conditional UI, check boxes for one and button for all others
INPUT_discounting_rate = 3
INPUT_antiviral_cost_scenario = "low_generic_cost"
INPUT_include_booster_vax_scenario = "high risk adults"
# [1] "all adults who have previously completed their primary schedule but have not recieved a booster"      
# [2] "high-risk adults who have previously completed their primary schedule but have not recieved a booster"
# [3] "all adults"                                                                                           
# [4] "high risk adults"                                                                                     
# [5] "no booster
INPUT_include_antiviral_type = c("nirmatrelvir_ritonavir")
INPUT_include_antiviral_type = c(INPUT_include_antiviral_type,"no antiviral")
INPUT_include_antiviral_target_group = c("adults with comorbidities")



### Create plots
this_workshop = CEAC_dataframe %>% filter(outcome == INPUT_include_outcomes &
                                            setting %in% INPUT_include_setting &
                                            perspective == INPUT_perspective &
                                            discounting_rate == INPUT_discounting_rate &
                                            antiviral_cost_scenario == INPUT_antiviral_cost_scenario &
                                            booster_vax_scenario %in% INPUT_include_booster_vax_scenario &
                                            antiviral_type %in% INPUT_include_antiviral_type & 
                                            antiviral_target_group %in% INPUT_include_antiviral_target_group)
xmax = this_workshop %>% 
  group_by(setting,booster_vax_scenario,antiviral_target_group) %>%
  filter(round(probability,digits=2) >=0.99) %>%
  summarise(min = min(WTP), .groups = "keep") %>%
  ungroup()
xmax = max(xmax$min)

xmin = this_workshop %>% 
  group_by(setting,booster_vax_scenario,antiviral_target_group) %>%
  filter(probability == min(probability)) %>%
  summarise(max = max(WTP), .groups = "keep") %>%
  ungroup()
xmin = min(xmin$max)

n_options_selected = max(length(INPUT_discounting_rate),
                         length(INPUT_antiviral_cost_scenario),
                         length(INPUT_include_booster_vax_scenario),
                         length(INPUT_include_antiviral_target_group))

plot_list = list()
for (this_setting in INPUT_include_setting){
  to_plot_setting = this_workshop[this_workshop$setting == this_setting,]
  if (length(INPUT_include_antiviral_target_group)>1){
    plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
      geom_point(aes(x=WTP,y=probability,color=as.factor(antiviral_target_group))) +
      labs(color="antiviral strategy") 
  } else if (length(INPUT_include_booster_vax_scenario)>1){
    plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
      geom_point(aes(x=WTP,y=probability,color=as.factor(booster_vax_scenario))) +
      labs(color="booster strategy")
  } else if (length(INPUT_discounting_rate)>1){
    plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
      geom_point(aes(x=WTP,y=probability,color=as.factor(discounting_rate))) +
      labs(color="discounting rate")
  } else if (length(INPUT_antiviral_cost_scenario)>1){
    plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
      geom_point(aes(x=WTP,y=probability,color=as.factor(antiviral_cost_scenario))) +
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

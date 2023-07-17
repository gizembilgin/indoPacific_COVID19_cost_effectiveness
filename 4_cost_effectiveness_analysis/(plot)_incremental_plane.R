
#Require result (CommandDeck_result_long) with variables outcome, setting, perspective, discounting_rate, antiviral_cost, booster_vax_scenario,antiviral_type,netCost,count_outcomes
require(ggpubr)

INPUT_setting_list = c("PNG","TLS")
INPUT_booster_strategy = "booster to all high-risk adults previously willing to be vaccinated"
INPUT_include_antiviral_type = c("nirmatrelvir_ritonavir 2023-01-01")
INPUT_include_antiviral_type = c(INPUT_include_antiviral_type,"no antiviral")
INPUT_antiviral_target_group = c("adults_with_comorbidities")
INPUT_perspective = "healthcare"
INPUT_discounting_rate = 0.03
INPUT_antiviral_cost = "middle_income_cost"
INPUT_outcome = "QALYs"



options(scipen = 1000)

### Check inputs
if(length(INPUT_antiviral_target_group)>1 & length(INPUT_booster_strategy)>1){
  stop('Please select either multiple antiviral strategies OR multiple booster strategies')
}

### Subset results
to_plot = CommandDeck_result_long %>%
  filter(outcome == INPUT_outcome &
           setting %in% INPUT_setting_list &
           perspective == INPUT_perspective &
           discounting_rate == INPUT_discounting_rate &
           antiviral_cost == INPUT_antiviral_cost &
           booster_vax_scenario %in% INPUT_booster_strategy &
           antiviral_type %in% INPUT_include_antiviral_type &
           antiviral_target_group %in% INPUT_antiviral_target_group)


### Create plots
plot_list = list()
for (this_setting in INPUT_setting_list){
  to_plot_setting = to_plot[to_plot$setting == this_setting,]
  if (length(INPUT_antiviral_target_group)>1){
    plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
      geom_point(aes(x=netCost,y=count_outcomes,color=as.factor(antiviral_type))) +
      labs(color="antiviral strategy") 
  } else if (length(INPUT_booster_strategy)>1){
    plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
      geom_point(aes(x=netCost,y=count_outcomes,color=as.factor(booster_vax_scenario))) +
      labs(color="booster strategy")
  } else{
    plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
      geom_point(aes(x=netCost,y=count_outcomes)) 
  }
  plot_list[[length(plot_list)]] = plot_list[[length(plot_list)]] +
    ylab("QALYs averted") +
    xlab("net cost (2022 USD)") +
    theme_bw() + 
    theme(legend.position="bottom") +
    labs(title = this_setting) +
    ylim(0,max(to_plot_setting$count_outcomes))

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


### Let's compare the time per run & ICER plane plot of varying DECISION_include_net & DECISION_sampling_strategy
ICER_plane_df = data.frame()
tracking_times_df = data.frame()

for (this_DECISION_sampling_strategy in c("empirical_distribution","single_run")){
  for (this_DECISION_include_net in c("Y","N")){
    
    time.start = proc.time()[[3]]
    
    CommandDeck_CONTROLS =
      list(
        TOGGLE_perspective = "healthcare",
        TOGGLE_discounting_rate = 0.03,
        TOGGLE_antiviral_cost_scenario = "middle_income_cost",
        
        TOGGLE_longCOVID = "off",
        TOGGLE_uncertainty = "rand",
        TOGGLE_numberOfRuns = 100, 
        TOGGLE_clusterNumber = 5, 
        DECISION_save_result = "N",
        DECISION_sampling_strategy = this_DECISION_sampling_strategy,
        DECISION_include_net = this_DECISION_include_net
      )
    
    source(paste(getwd(),"/CommandDeck.R",sep=""))
    
    time.end = proc.time()[[3]]
    time_this_run = data.frame(
      time_second = time.end-time.start,
      time_minutes = (time.end-time.start)/60,
      DECISION_sampling_strategy = this_DECISION_sampling_strategy,
      DECISION_include_net = this_DECISION_include_net)
    
    CommandDeck_result_long = CommandDeck_result_long %>%
      mutate(DECISION_sampling_strategy = this_DECISION_sampling_strategy,
             DECISION_include_net = this_DECISION_include_net)
    
    ICER_plane_df = rbind(ICER_plane_df, CommandDeck_result_long)
    tracking_times_df = rbind(tracking_times_df,time_this_run)
    
  }
}
CommandDeck_CONTROLS = list()
beep()

#note: timings for 100 runs, 5 cores, 2 settings =
# time_second time_minutes DECISION_sampling_strategy DECISION_include_net
# 1709.86    28.497667     empirical_distribution                   Y
# 388.97     6.482833      empirical_distribution                   N
# 1546.76    25.779333                single_run                    Y
# 412.12     6.868667                 single_run                    N

require(ggpubr);options(scipen = 1000)
to_plot = ICER_plane_df %>%
  filter(evaluation_level == "incremental") %>%
  filter(outcome == "QALYs" &
           booster_vax_scenario %in% c("booster to all high-risk adults previously willing to be vaccinated") &
           antiviral_type %in% c("nirmatrelvir_ritonavir 2023-01-01") &
           antiviral_target_group == "adults_with_comorbidities") %>%
  mutate(label = paste(DECISION_include_net, DECISION_sampling_strategy))

ggplot(to_plot[to_plot$setting == "TLS",]) +
  geom_point(aes(x=netCost,y=count_outcomes,color=as.factor(label))) +
  ylab("QALYs averted") +
  xlab("net cost (2022 USD)") +
  theme_bw() + 
  theme(legend.position="bottom") +
  facet_grid(label ~.)

plot_list = list()
for (this_setting in unique(ICER_plane_df$setting)){
  to_plot_setting = to_plot[to_plot$setting == this_setting,]
  
  plot_list[[length(plot_list)+1]] = ggplot(to_plot_setting) +
    geom_point(aes(x=netCost,y=count_outcomes,color=as.factor(label))) 
  
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

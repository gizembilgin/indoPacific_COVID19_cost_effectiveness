

timing_df = ICER_plane_df = data.frame()

#for (this_DECISION_include_net in c("N","Y")){
for (this_DECISION_include_net in c("Y")){
  for (num_cores in c(2,3,4,5)){
    if (!(num_cores %in% timing_df$cores & this_DECISION_include_net %in% timing_df$DECISION_include_net)){
      time.start=proc.time()[[3]] #let's see how long this runs for
      
      CommandDeck_CONTROLS =
        list(
          LIST_CEA_settings = list("PNG_low_beta","TLS","FJI","IDN"),
          LIST_perspectives = list("healthcare","societal"),
          LIST_discounting_rate = c(0.03), #for NET run
          LIST_antiviral_cost_scenario = c("low_generic_cost","middle_income_cost", "high_income_cost"),
          
          TOGGLE_longCOVID = "off",
          TOGGLE_uncertainty = "rand",
          TOGGLE_numberOfRuns = 120, #1000 eventually
          TOGGLE_clusterNumber = num_cores,  #4 or 5? test and time!
          DECISION_sampling_strategy = "single_run",
          DECISION_include_net = this_DECISION_include_net
        )
      
      source(paste0(getwd(),"/CommandDeck.R"))
      
      time.end=proc.time()[[3]]
      time_this_run = time.end-time.start
      this_row = data.frame(DECISION_include_net = this_DECISION_include_net,
                            cores = num_cores, 
                            time_seconds = time_this_run,
                            time_minutes = time_this_run/60,
                            time_hours   = time_this_run/(60*60))
      
      CommandDeck_result_long = CommandDeck_result_long %>%
        mutate( DECISION_include_net = this_DECISION_include_net)
      
      ICER_plane_df = rbind(ICER_plane_df, CommandDeck_result_long)
      timing_df = rbind(timing_df,this_row)
      
      temp_name = ''
      time = Sys.time()
      time = gsub(':','-',time)
      time = paste(temp_name,time,sep='')
      write.csv(timing_df, file = paste("x_results/",time,"timing_df.csv"))
    }
  }
}
CommandDeck_CONTROLS = list()
beep()

# TOGGLE_numberOfRuns == 120
# DECISION_include_net cores time_seconds time_minutes time_hours
# 1                    N     2     18356.14     305.9357   5.098928
# 2                    N     3     17782.15     296.3692   4.939486 #best choice!!!
# 3                    N     4     18645.44     310.7573   5.179289
# 4                    N     5     20860.31     347.6718   5.794531
# > timing_df$time_hours/120*1000
# 42.49106 41.16238 43.16074 48.28775

#NB: 30 minutes for one Y CEA run, i.e., 20 days!! Let's stop sampling from a normal distribution if there is >1 million
# DECISION_include_net cores time_seconds time_minutes time_hours
# 1                    Y     1     10974.44     182.9073   3.048456
# 2                    Y     2     11121.25     185.3542   3.089236
# 3                    Y     3     10980.80     183.0133   3.050222

#After stopping sampling > 1 million times from a normal distribution
# DECISION_include_net	cores	time_seconds	time_minutes	time_hours	time_1000
# Y	                        2	958.36	      15.97266667	0.266211111	  2.218425926
# Y	                        3	738.63	      12.3105	    0.205175	    1.709791667
# Y	                        4	650.76	      10.846	    0.180766667	  1.506388889
# Y	                        5	551.27	      9.187833333	0.153130556	  1.276087963


require(ggpubr);options(scipen = 1000)
to_plot = ICER_plane_df %>%
  filter(evaluation_level == "incremental") %>%
  filter(outcome == "QALYs" &
           booster_vax_scenario %in% c("booster to all high-risk adults previously willing to be vaccinated") &
           antiviral_type %in% c("nirmatrelvir_ritonavir 2023-01-01") &
           antiviral_target_group == "adults_with_comorbidities") %>%
  mutate(label = paste(DECISION_include_net))

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

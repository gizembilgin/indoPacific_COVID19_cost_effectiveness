### PLOT
##Note: run for default booster dose strategy and antiviral strategy, go back to (run)_deterministic_results.R to extract more results
## make reactive so doesn't reload when different number of param selected (line 74)

INPUT_setting_list = c("PNG","TLS")
INPUT_perspective = "healthcare"
INPUT_outcome = "QALYs"
INPUT_parameters = c(
  # "Antiviral schedule price ($25-530)",
  # "Antiviral wastage (0-60%)"          ,
  # "Inpatient costs (±50%)"              ,
  # "Discounting rate (0-5%)"              ,
  "RAT price ($1-5)"                      ,
  "RAT wastage factor (3-12)"              ,
  "Cost per extra LOS (±50%)"               ,
  "Reduced LOS (±50%)"                       ,
  "Antiviral operational costs (±50%)"       ,
  "Booster operational cost ($0.21-$13.04)"  ,
  "Long COVID (off/on)"                      ,
  "Booster price ($0.50-$3.00)"              ,
  "Booster wastage (0-50%)"                  ,
  "Outpatient costs (±50%)"                  ,
  "Injection Equipment wastage (0-50%)"      ,
  "Injection Equipment price ($0.025-$0.050)"
)
INPUT_include_GDP = "Y"


load(file = "x_results/tornado_result.Rdata")
tornado_variable_of_interest = paste("cost_per_",
                                     gsub("QALYs","QALY",INPUT_outcome),
                                     "_averted",
                                     sep ="")
tornado_result = tornado_result %>%
  filter(antiviral_type != "no antiviral" &
           variable == tornado_variable_of_interest &
           setting %in% INPUT_setting_list &
           perspective %in% INPUT_perspective) 

plot_list = list()

for (this_setting in unique(tornado_result$setting)){
  to_plot = tornado_result %>%
    filter(setting == this_setting)
  
  if (this_setting == "FJI"){this_setting_GDP = 5316.7}
  if (this_setting == "IDN"){this_setting_GDP = 4788.0}
  if (this_setting == "PNG"){this_setting_GDP = 3020.3}
  if (this_setting == "TLS"){this_setting_GDP = 2358.4}
  
  base.value <- to_plot$mean[to_plot$direction == "lower" & 
                               to_plot$label == "Long COVID (off/on)" ] # final value was baseline estimates
  
  # width of columns in plot (value between 0 and 1)
  width <- 0.95
  order_parameters <- to_plot %>%
    select(label,mean,direction) %>%
    group_by(label) %>%
    summarise(LB = min(mean),
              UB = max(mean)) %>%
    mutate(UL_Difference = UB - LB) %>% 
    arrange(UL_Difference) %>%
    mutate(label=factor(x=label, levels=label)) %>%
    select(label) %>% 
    unlist() %>% 
    levels()
  
  # get data frame in shape for ggplot and geom_rect
  df_2 <- to_plot %>%
    select(label,mean,direction) %>% 
    rename(value = mean) %>%
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
  
  #REACTIVE CHANGE OF PLOT STARTS HERE
  plot_list[[length(plot_list)+1]] = ggplot() + 
    geom_rect(data = df_2[df_2$label %in% INPUT_parameters,], #THIS IS THE MAIN EXPECTED CHANGE
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
    labs(title = this_setting)
  
  if (INPUT_include_GDP == "Y"){
    plot_list[[length(plot_list)]] = plot_list[[length(plot_list)]] + 
    geom_hline(mapping = NULL, yintercept = this_setting_GDP, linetype='dashed') +
    annotate("text", x = 4, y = this_setting_GDP*0.65, label = "GDP per capita")
  }
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


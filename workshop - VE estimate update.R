


VE_estimates <- read.csv("1_inputs/VE_WHO_forest_plot.csv",header=TRUE)
colnames(VE_estimates)

VE_estimates = VE_estimates  %>% select(strain, vaccine_type, dose, outcome,VE,lower_est,upper_est)

to_plot = VE_estimates[VE_estimates$strain == 'delta',]

plot_list = list()
for (i in 1:length(unique(to_plot$outcome))){
  outcome = unique(to_plot$outcome)[i]
  plot_list [[i]] <- ggplot(data=to_plot[to_plot$outcome==outcome,]) + 
    #geom_point(aes(x=VE,y=vaccine_type, color=as.factor(dose),shape=strain)) +
    geom_pointrange(aes(x=VE,y=vaccine_type,color=as.factor(dose),shape=strain,xmin=lower_est,xmax=upper_est)) +
    xlim(0,100) +
    xlab("") + 
    ylab("") + 
    labs(title=paste("VE against ",outcome,sep=""))
}
gridExtra::grid.arrange(grobs=plot_list)

require(ggpubr)
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],
          common.legend = TRUE,
          legend="bottom"
)


#compare VE against death (where avaliable) to VE against severe disease
#compare VE against omicron (where avaliable) to delta
#compare VE against any infection (where avaliable) to symptomatic disease
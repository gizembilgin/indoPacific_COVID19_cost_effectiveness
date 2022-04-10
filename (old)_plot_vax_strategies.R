

### (1) Overarching toggles #######################################################################################################
options(scipen=10000); require(ggpubr)

list_plot_commands = c("no further vaccine rollout","baseline - current roll-out","baseline - idealised prioritisation strategy")
list_plot_commands = c("no further vaccine rollout","baseline - current roll-out","expand to children!")
list_plot_commands = c("no further vaccine rollout","baseline - current roll-out","double speed","x5 speed" )

day_or_date = "day"
if (day_or_date == "day"){
  severe_outcome_tracker = severe_outcome_tracker %>% mutate(time = day)
} else if (day_or_date == 'date'){
  severe_outcome_tracker = severe_outcome_tracker %>% mutate(time = date)
}
  

#list_plot_commands = c("no further vaccine rollout",'vaccinate the most vulnerable','vaccinate the transmitters' )
#list_plot_commands = c("no further vaccine rollout","baseline - current roll-out",'40% coverage','80% coverage' )


### (2) Compare as is #######################################################################################################
#(A/B) table
severe_outcome_table
#write.csv(severe_outcome_table, "x_results/severe_outcome_table.csv", row.names = F)  #easy copy pasting

#(B/B) plots
#(i) absolute
workshop = severe_outcome_tracker[severe_outcome_tracker$label %in% list_plot_commands, ]
plot_list = list()
for (i in 1:length(unique(workshop$outcome))){
  outcome = unique(workshop$outcome)[i]
  plot_list [[i]] <- ggplot(data=workshop[workshop$outcome==outcome,]) + 
    geom_point(aes(x=time,y=proj,color=as.factor(label))) +
    labs(title=paste(outcome)) +
    theme_bw() + 
    xlab("") + 
    ylab("")}
plot = ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],
                 common.legend = TRUE,
                 legend="bottom")
annotate_figure(plot, top = text_grob('absolute outcome by scenario', face = 'bold', size = 16))

#(ii) cumulative
workshop = severe_outcome_tracker[severe_outcome_tracker$label %in% list_plot_commands, ]
plot_list = list()
for (i in 1:length(unique(workshop$outcome))){
  outcome = unique(workshop$outcome)[i]
  plot_list [[i]] <- ggplot(data=workshop[workshop$outcome==outcome,]) + 
    geom_point(aes(x=noquote(time),y=proj_cum,color=as.factor(label))) +
    labs(title=paste(outcome)) +
    theme_bw() + 
    xlab("") + 
    ylab("")}
plot = ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],
                 common.legend = TRUE,
                 legend="bottom")
annotate_figure(plot, top = text_grob('cumulative outcome by scenario', face = 'bold', size = 16))
#_____________________________________________________________________________________________________________________



### (3) Compare to baseline #######################################################################################################
#(A/B) table
averted_table = severe_outcome_table[severe_outcome_table$scenario != 'no further vaccine rollout',]
averted_table_rel = averted_table
for (i in 1:(length(queue)-1)){
  averted_table[i,c(1:length(unique(workshop$outcome)))] = 
    severe_outcome_table[severe_outcome_table$scenario == 'no further vaccine rollout',c(1:length(unique(workshop$outcome)))] -
  averted_table[i,c(1:length(unique(workshop$outcome)))] 
  
  averted_table_rel[i,c(1:length(unique(workshop$outcome)))] = 100 * averted_table[i,c(1:length(unique(workshop$outcome)))]/
    severe_outcome_table[severe_outcome_table$scenario == 'no further vaccine rollout',c(1:length(unique(workshop$outcome)))]
}
averted_table; averted_table_rel


#(B/B) plots
baseline = severe_outcome_tracker[severe_outcome_tracker$label == 'no further vaccine rollout',c('time','outcome','proj','proj_cum')] 
colnames(baseline) = c('time','outcome','baseline','baseline_cum')

averted_tracker = severe_outcome_tracker[severe_outcome_tracker$label != 'no further vaccine rollout',] %>% 
  left_join(baseline) %>%
  mutate(averted_abs = baseline - proj,
         averted_rel = (baseline - proj) /baseline) %>%
  ungroup() %>%
  group_by(outcome,label) %>%
  mutate(averted_cum = cumsum(averted_abs),
         averted_rel_cum = cumsum(averted_abs)/cumsum(baseline))


workshop = averted_tracker[averted_tracker$label %in% list_plot_commands, ]
#abs
plot_list = list()
for (i in 1:length(unique(workshop$outcome))){
  outcome = unique(workshop$outcome)[i]
  plot_list [[i]] <- ggplot(data=workshop[workshop$outcome==outcome,]) + 
    geom_point(aes(x=time,y=averted_abs,color=as.factor(label))) +
    labs(title=paste(outcome)) +
    theme_bw() + 
    xlab("") + 
    ylab("")}
plot = ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],
                 common.legend = TRUE,
                 legend="bottom")
annotate_figure(plot, top = text_grob('daily outcome averted', face = 'bold', size = 16))

#cum
plot_list = list()
for (i in 1:length(unique(workshop$outcome))){
  outcome = unique(workshop$outcome)[i]
  plot_list [[i]] <- ggplot(data=workshop[workshop$outcome==outcome,]) + 
    geom_point(aes(x=time,y=averted_cum,color=as.factor(label))) +
    labs(title=paste(outcome)) +
    theme_bw() + 
    xlab("") + 
    ylab("")}
plot = ggarrange(plot_list[[1]],plot_list[[5]],
          common.legend = TRUE,
          legend="bottom")
#plot = ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],
 #                common.legend = TRUE,
  #               legend="bottom")

annotate_figure(plot, top = text_grob('cumulative outcome averted', face = 'bold', size = 16))


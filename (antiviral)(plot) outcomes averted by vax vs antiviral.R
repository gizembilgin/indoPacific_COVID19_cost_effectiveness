### PLOT (1/2) Vax vs. antivirals ##############################################
RECORD_antiviral_model_simulations = RECORD_antiviral_model_simulations %>%
  mutate(vax_scenario_short = case_when(
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to all prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to all first booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to high-risk prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to high-risk prev first booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule" ~ 
      "no booster"
  ))

LIST_outcomes = list('severe_disease', 
                     'hosp', 
                     'death', 
                     'YLL')

### PLOT dose-impact of nirmatrelvir_ritonavir and booster doses
#option 1: boxplot
workshop = RECORD_antiviral_model_simulations %>% 
  filter(antiviral_type == "nirmatrelvir_ritonavir" | intervention == 'vaccine') %>%
  filter(antiviral_target_group == 'adults_with_comorbidities' | intervention == 'vaccine') %>%
  filter( !(intervention == 'vaccine' & evaluation_group == 'pop_level')) %>% #change eval group here to change from high-risk to pop-level plot
  #filter(!(intervention == 'vaccine' & vax_scenario == 'all willing adults vaccinated with a primary schedule plus booster dose')) %>%
  #filter(result %in% c("average_doses_per_outcome_averted","UCI_doses_per_outcome_averted","LCI_doses_per_outcome_averted")) %>%
  filter(result %in% c("doses_per_outcome_averted")) %>%
  mutate(intervention = case_when(
   # intervention == 'antiviral' ~ paste('antiviral starting',antiviral_start_date),
    intervention == 'vaccine' ~ paste('booster dose starting 2023-01-01'),
    TRUE ~ intervention
  )) #%>% filter(value>0)
  #filter(intervention != "booster dose starting 2023-01-01")

#option 2: median and IQR plot
workshop = RECORD_antiviral_model_simulations %>% 
  filter(antiviral_type == "nirmatrelvir_ritonavir" | intervention == 'vaccine') %>%
  filter(antiviral_target_group == 'adults_with_comorbidities' | intervention == 'vaccine') %>%
  filter( !(intervention == 'vaccine' & evaluation_group == 'pop_level')) %>% #change eval group here to change from high-risk to pop-level plot
  filter(result %in% c("doses_per_outcome_averted")) %>%
  mutate(intervention = case_when(
    intervention == 'vaccine' ~ paste('booster dose starting 2023-01-01'),
    TRUE ~ intervention
  )) %>% group_by(intervention,outcome,antiviral_type,antiviral_target_group,evaluation_group,vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
  summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75))

options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  #option 1: boxplot
  # plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
  #   geom_boxplot(aes(x=value,y=vax_scenario_short,color=as.factor(intervention)))  +
  #   labs(title = paste(this_outcome), color = 'intervention') +
  #   ylab('')+
  #   xlab('doses to avert an outcome')
  
  #option 2: IQR and median
  plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_pointrange(aes(x=median,y=vax_scenario_short,color=as.factor(intervention),xmin=LQ,xmax=UQ))  +
    labs(title = paste(this_outcome), color = 'intervention') +
    ylab('')+
    xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
    xlab('doses to avert an outcome')
  
}
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = 4) 
#ggsave(paste(rootpath,"x_results/plot_VaxVsAntivirals_",time,".png",sep=''), width = 9.6, height = 5.7)
options(warn = 0)


### PLOT dose-impact of nirmatrelvir_ritonavir without booster doses
#option 1: boxplot
workshop = RECORD_antiviral_model_simulations %>% 
  filter(antiviral_type == "nirmatrelvir_ritonavir" | intervention == 'vaccine') %>%
  filter(antiviral_target_group == 'adults_with_comorbidities' | intervention == 'vaccine') %>%
  filter( !(intervention == 'vaccine' & evaluation_group == 'pop_level')) %>% #change eval group here to change from high-risk to pop-level plot
  #filter(!(intervention == 'vaccine' & vax_scenario == 'all willing adults vaccinated with a primary schedule plus booster dose')) %>%
  #filter(result %in% c("average_doses_per_outcome_averted","UCI_doses_per_outcome_averted","LCI_doses_per_outcome_averted")) %>%
  filter(result %in% c("doses_per_outcome_averted")) %>%
  mutate(intervention = case_when(
    # intervention == 'antiviral' ~ paste('antiviral starting',antiviral_start_date),
    intervention == 'vaccine' ~ paste('booster dose starting 2023-01-01'),
    TRUE ~ intervention
  )) %>% 
  filter(value>0) %>% 
  filter(intervention != "booster dose starting 2023-01-01")

#option 2: median and IQR plot
workshop = RECORD_antiviral_model_simulations %>% 
  filter(antiviral_type == "nirmatrelvir_ritonavir" | intervention == 'vaccine') %>%
  filter(antiviral_target_group == 'adults_with_comorbidities' | intervention == 'vaccine') %>%
  filter( !(intervention == 'vaccine' & evaluation_group == 'pop_level')) %>% #change eval group here to change from high-risk to pop-level plot
  filter(result %in% c("doses_per_outcome_averted")) %>%
  mutate(intervention = case_when(
    intervention == 'vaccine' ~ paste('booster dose starting 2023-01-01'),
    TRUE ~ intervention
  )) %>% group_by(intervention,outcome,antiviral_type,antiviral_target_group,evaluation_group,vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
  summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75)) %>% 
  filter(intervention != "booster dose starting 2023-01-01")

options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  #option 1: boxplot
  # plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
  #   geom_boxplot(aes(x=value,y=vax_scenario_short,color=as.factor(intervention)))  +
  #   labs(title = paste(this_outcome), color = 'intervention') +
  #   ylab('')+
  #   xlab('doses to avert an outcome')
  
  #option 2: IQR and median
  plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_pointrange(aes(x=median,y=vax_scenario_short,color=as.factor(intervention),xmin=LQ,xmax=UQ))  +
    labs(title = paste(this_outcome), color = 'intervention') +
    ylab('')+
    xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
    xlab('doses to avert an outcome')
  
}
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = 4) 
#ggsave(paste(rootpath,"x_results/plot_VaxVsAntivirals_",time,".png",sep=''), width = 9.6, height = 5.7)
options(warn = 0)




# # Additional plot for molnupiravir #############################################
# LIST_outcomes = list('severe_disease', 
#                      'hosp', 
#                      'death', 
#                      'YLL')
# 
# ### Calculate # of antivirals per outcome averted
# workshop = RECORD_antiviral_model_simulations %>% 
#   filter(antiviral_type != "nirmatrelvir_ritonavir" | intervention == 'vaccine') %>%
#   filter(antiviral_target_group == 'adults_with_comorbidities' | intervention == 'vaccine') %>%
#   filter( !(intervention == 'vaccine' & evaluation_group == 'high_risk')) %>% #change eval group here to change from high-risk to pop-level plot
#   #filter(!(intervention == 'vaccine' & vax_scenario == 'all willing adults vaccinated with a primary schedule plus booster dose')) %>%
#   #filter(result %in% c("average_doses_per_outcome_averted","UCI_doses_per_outcome_averted","LCI_doses_per_outcome_averted")) %>%
#   filter(result %in% c("doses_per_outcome_averted")) %>%
#   mutate(intervention = case_when(
#     # intervention == 'antiviral' ~ paste('antiviral starting',antiviral_start_date),
#     intervention == 'vaccine' ~ paste('booster dose starting 2023-01-01'),
#     TRUE ~ intervention
#   )) #%>%
#   # pivot_wider(
#   #   id_cols = c(vax_scenario,vax_scenario_risk_group,antiviral_target_group,outcome,intervention),
#   #   names_from = result,
#   #   values_from = value)
# 
# 
# options(warn = -1)
# plot_list = list()
# for (a in 1:length(LIST_outcomes)) {
#   this_outcome = LIST_outcomes[[a]]
#   
#   # plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
#   #   geom_pointrange(aes(x=average_doses_per_outcome_averted,y=vax_scenario,color=as.factor(intervention),xmin=LCI_doses_per_outcome_averted,xmax=UCI_doses_per_outcome_averted))  + 
#   #   labs(title = paste(this_outcome), color = 'intervention') +
#   #   ylab('')+
#   #   xlim(0,max(workshop$UCI_doses_per_outcome_averted[workshop$outcome == this_outcome])) +
#   #   xlab('doses to avert an outcome')
#   
#   plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
#     geom_boxplot(aes(x=value,y=vax_scenario_short,color=as.factor(intervention)))  + 
#     labs(title = paste(this_outcome), color = 'intervention') +
#     ylab('')+
#     xlab('doses to avert an outcome')
#   
# }
# ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
#           common.legend = TRUE,
#           legend="bottom",
#           ncol = 1,
#           nrow = 4) 
# #ggsave(paste(rootpath,"x_results/plot_VaxVsAntivirals_molnupiravir_",time,".png",sep=''), width = 9.6, height = 5.7)
# options(warn = 0)
# #____________________________________________________________________________



### PLOT (2/2) Varying target groups ###########################################
LIST_target_group = list('adults_with_comorbidities', 
                         'unvaccinated_adults',
                         'unvaccinated_adults_AND_adults_with_comorbidities',
                         'all_adults')

### Calculate # of antivirals per outcome averted
#option 1: box plot
workshop = RECORD_antiviral_model_simulations  %>% 
  filter(antiviral_type == "nirmatrelvir_ritonavir" & intervention == "antiviral 2023-01-01") %>% 
  filter(result %in% c("doses_per_outcome_averted")) %>%
  mutate(intervention = case_when(
    intervention == 'vaccine' ~ paste('booster dose starting 2023-01-01'),
    TRUE ~ intervention
  )) 

#option 2: median + IQR
workshop = RECORD_antiviral_model_simulations  %>% 
  filter(antiviral_type == "nirmatrelvir_ritonavir" & intervention == "antiviral 2023-01-01") %>% 
  filter(result %in% c("doses_per_outcome_averted")) %>%
  mutate(intervention = case_when(
    intervention == 'vaccine' ~ paste('booster dose starting 2023-01-01'),
    TRUE ~ intervention
  )) %>% group_by(intervention,outcome,antiviral_type,antiviral_target_group,evaluation_group,vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
  summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75))


options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  # plot_list[[a]] = ggplot(data = workshop[workshop$outcome == this_outcome,]) +
  #   geom_pointrange(aes(x=average_doses_per_outcome_averted,y=vax_scenario,color=as.factor(antiviral_target_group),xmin=LCI_doses_per_outcome_averted,xmax=UCI_doses_per_outcome_averted))  + 
  #   labs(title = paste(this_outcome), color = 'intervention') +
  #   ylab('')+
  #   xlim(0,max(workshop$UCI_doses_per_outcome_averted[workshop$outcome == this_outcome])) +
  #   xlab('antiviral doses to avert an outcome')
  

  
  #option 1: boxplot
  # plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
  #   geom_boxplot(aes(x=value,y=vax_scenario_short,color=as.factor(antiviral_target_group)))  + 
  #   labs(title = paste(this_outcome), color = 'intervention') +
  #   ylab('')+
  #   xlab('doses to avert an outcome')
  
  #option 2: IQR and median
  plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_pointrange(aes(x=median,y=vax_scenario_short,color=as.factor(antiviral_target_group),xmin=LQ,xmax=UQ))  +
    labs(title = paste(this_outcome), color = 'intervention') +
    ylab('')+
    xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
    xlab('doses to avert an outcome')
  
}
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = 4) 
#ggsave(paste(rootpath,"x_results/plot_targetGroups_",time,".png",sep=''), width = 9.6, height = 5.7)
options(warn = 0)
#____________________________________________________________________________





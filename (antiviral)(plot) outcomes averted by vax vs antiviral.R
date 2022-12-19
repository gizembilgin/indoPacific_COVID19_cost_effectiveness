
### SETUP  ################################################################
#load libraries
library(readr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(tidyverse)
library(doParallel)
library(parallel)
library(foreach)

#dependencies -> nil!
rm(list=ls())
rootpath = str_replace(getwd(), "GitHub_vaxAllocation","")

load(file = '1_inputs/last_fit_date.Rdata')
source(paste(getwd(),"/(antiviral)(function) antiviral_model_manger.R",sep=""))
source(paste(getwd(),"/(antiviral)(function) antiviral_model_worker.R",sep=""))
source(paste(getwd(),"/(antiviral)(function) stochastic_severe_outcomes_sampling.R",sep=""))
source(paste(getwd(),"/(antiviral)(function) stochastic_severe_outcomes_application.R",sep=""))
copy_function_into_cluster = antiviral_model_worker
RECORD_antiviral_model_simulations = data.frame()
#____________________________________________________________________________



### ANTIVIRAL SIMULATIONS ##################################################
time.start.AntiviralSimulations=proc.time()[[3]]

for (setting in c("PNG","TLS","IDN","FJI","SLB","PHL")){

  #load latest antiviralSetUp_* (transmission model run for 1 year)
  list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = "antiviralSetUp_*")
  list_poss_Rdata_details = double()
  for (i in 1:length(list_poss_Rdata)){
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste(rootpath,'x_results/',list_poss_Rdata[[i]],sep=''))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste(rootpath,"x_results/",latest_file,sep=''))
  
  #subset to save RAM
  RECORD_antiviral_setup$outcomes_without_antivirals = RECORD_antiviral_setup$outcomes_without_antivirals %>% filter(country == setting)
  RECORD_antiviral_setup$likelihood_severe_outcome = RECORD_antiviral_setup$likelihood_severe_outcome %>% filter(country == setting)
  RECORD_antiviral_setup$incidence_log_tidy = RECORD_antiviral_setup$incidence_log_tidy %>% filter(country == setting)
  RECORD_antiviral_setup$exposed_log = RECORD_antiviral_setup$exposed_log %>% filter(country == setting)
  RECORD_antiviral_setup$incidence_log = RECORD_antiviral_setup$incidence_log %>% filter(country == setting)
  RECORD_antiviral_setup$vaccination_history_FINAL = RECORD_antiviral_setup$vaccination_history_FINAL %>% filter(country == setting)
  
  RECORD_antiviral_model_simulations_0 <- antiviral_model_manger(
   
    LIST_antiviral_start_date = c(as.Date('2023-01-01'),as.Date('2023-07-01')), 
    LIST_vax_scenario = list('all willing adults vaccinated with a primary schedule',
                            'all willing adults vaccinated with a primary schedule and high risk group recieve a booster', 
                            'all willing adults vaccinated with a primary schedule plus booster dose'),
    LIST_antiviral_target_group = list('adults_with_comorbidities', #baseline
                                    'unvaccinated_adults',
                                    'unvaccinated_adults_AND_adults_with_comorbidities',
                                    'all_adults'),
    LIST_antiviral_type = list("nirmatrelvir_ritonavir","molunipiravir"), #options:nirmatrelvir_ritonavir,molunipiravir 
    toggle_high_risk_group = "adults_with_comorbidities",
    
    
    RECORD_antiviral_setup = RECORD_antiviral_setup,
    setting = setting,
    
    toggle_number_of_runs = 100,
    toggle_cluster_number = 4,
    
    toggle_stochastic_SO = "off",
    toggle_compare_to_vaccine_effect = "on",
            
    toggle_sensitivity_analysis = list(),
    pathway_to_care = "fixed",
    toggle_fixed_antiviral_coverage = 0.2,
    manager_stochastic_VE_sampling = "uniform" # options: "normal" or "uniform"
  )
  
  RECORD_antiviral_model_simulations_0 = RECORD_antiviral_model_simulations_0 %>% mutate(country = setting)
  RECORD_antiviral_model_simulations = rbind(RECORD_antiviral_model_simulations,RECORD_antiviral_model_simulations_0)
}

time.end.AntiviralSimulations=proc.time()[[3]]
(time.end.AntiviralSimulations - time.start.AntiviralSimulations)/60 # roughly 8 hours stochastically

time = Sys.time()
time = gsub(':','-',time)

temp_name = ''
time = paste(temp_name,time,sep='')
#____________________________________________________________________________



### PLOT (1/2) Vax vs. antivirals ##############################################
LIST_outcomes = list('severe_disease', 
                     'hosp', 
                     'death', 
                     'YLL')

### Calculate # of antivirals per outcome averted
workshop = RECORD_antiviral_model_simulations %>% 
  filter(antiviral_type == "nirmatrelvir_ritonavir" | intervention == 'vaccine') %>%
  filter(antiviral_target_group == 'adults_with_comorbidities' | intervention == 'vaccine') %>%
  filter( !(intervention == 'vaccine' & evaluation_group == 'high_risk')) %>% #change eval group here to change from high-risk to pop-level plot
  #filter(!(intervention == 'vaccine' & vax_scenario == 'all willing adults vaccinated with a primary schedule plus booster dose')) %>%
  #filter(result %in% c("average_doses_per_outcome_averted","UCI_doses_per_outcome_averted","LCI_doses_per_outcome_averted")) %>%
  filter(result %in% c("doses_per_outcome_averted")) %>%
  mutate(intervention = case_when(
   # intervention == 'antiviral' ~ paste('antiviral starting',antiviral_start_date),
    intervention == 'vaccine' ~ paste('booster dose starting 2023-01-01'),
    TRUE ~ intervention
  )) #%>%
  #pivot_wider(
   # id_cols = c(vax_scenario,vax_scenario_risk_group,antiviral_target_group,outcome,intervention),
    #names_from = result,
    #values_from = value)


options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  # plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
  #   geom_pointrange(aes(x=average_doses_per_outcome_averted,y=vax_scenario,color=as.factor(intervention),xmin=LCI_doses_per_outcome_averted,xmax=UCI_doses_per_outcome_averted))  + 
  #   labs(title = paste(this_outcome), color = 'intervention') +
  #   ylab('')+
  #   xlim(0,max(workshop$UCI_doses_per_outcome_averted[workshop$outcome == this_outcome])) +
  #   xlab('doses to avert an outcome')
  
  plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_boxplot(aes(x=value,y=vax_scenario,color=as.factor(intervention)))  + 
    labs(title = paste(this_outcome), color = 'intervention') +
    ylab('')+
    #xlim(0,max(workshop$UCI_doses_per_outcome_averted[workshop$outcome == this_outcome])) +
    xlab('doses to avert an outcome')
  
}
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = 4) 
ggsave(paste(rootpath,"x_results/plot_VaxVsAntivirals_",time,".png",sep=''), width = 9.6, height = 5.7)
options(warn = 0)


# Additional plot for molnupiravir #############################################
LIST_outcomes = list('severe_disease', 
                     'hosp', 
                     'death', 
                     'YLL')

### Calculate # of antivirals per outcome averted
workshop = RECORD_antiviral_model_simulations %>% 
  filter(antiviral_type != "nirmatrelvir_ritonavir" | intervention == 'vaccine') %>%
  filter(antiviral_target_group == 'adults_with_comorbidities' | intervention == 'vaccine') %>%
  filter( !(intervention == 'vaccine' & evaluation_group == 'high_risk')) %>% #change eval group here to change from high-risk to pop-level plot
  #filter(!(intervention == 'vaccine' & vax_scenario == 'all willing adults vaccinated with a primary schedule plus booster dose')) %>%
  #filter(result %in% c("average_doses_per_outcome_averted","UCI_doses_per_outcome_averted","LCI_doses_per_outcome_averted")) %>%
  filter(result %in% c("doses_per_outcome_averted")) %>%
  mutate(intervention = case_when(
    # intervention == 'antiviral' ~ paste('antiviral starting',antiviral_start_date),
    intervention == 'vaccine' ~ paste('booster dose starting 2023-01-01'),
    TRUE ~ intervention
  )) #%>%
  # pivot_wider(
  #   id_cols = c(vax_scenario,vax_scenario_risk_group,antiviral_target_group,outcome,intervention),
  #   names_from = result,
  #   values_from = value)


options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  # plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
  #   geom_pointrange(aes(x=average_doses_per_outcome_averted,y=vax_scenario,color=as.factor(intervention),xmin=LCI_doses_per_outcome_averted,xmax=UCI_doses_per_outcome_averted))  + 
  #   labs(title = paste(this_outcome), color = 'intervention') +
  #   ylab('')+
  #   xlim(0,max(workshop$UCI_doses_per_outcome_averted[workshop$outcome == this_outcome])) +
  #   xlab('doses to avert an outcome')
  
  plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_boxplot(aes(x=value,y=vax_scenario,color=as.factor(intervention)))  + 
    labs(title = paste(this_outcome), color = 'intervention') +
    ylab('')+
    xlab('doses to avert an outcome')
  
}
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = 4) 
ggsave(paste(rootpath,"x_results/plot_VaxVsAntivirals_molnupiravir_",time,".png",sep=''), width = 9.6, height = 5.7)
options(warn = 0)
#____________________________________________________________________________



### PLOT (2/2) Varying target groups ###########################################
LIST_target_group = list('adults_with_comorbidities', 
                         'unvaccinated_adults',
                         'unvaccinated_adults_AND_adults_with_comorbidities',
                         'all_adults')

### Calculate # of antivirals per outcome averted
workshop = RECORD_antiviral_model_simulations  %>% 
  filter(antiviral_type == "nirmatrelvir_ritonavir" & intervention == "antiviral 2023-01-01") %>% 
  #filter(result %in% c("average_doses_per_outcome_averted","UCI_doses_per_outcome_averted","LCI_doses_per_outcome_averted")) %>%
  filter(result %in% c("doses_per_outcome_averted")) %>%
  mutate(intervention = case_when(
    # intervention == 'antiviral' ~ paste('antiviral starting',antiviral_start_date),
    intervention == 'vaccine' ~ paste('booster dose starting 2023-01-01'),
    TRUE ~ intervention
  )) #%>%
  # pivot_wider(
  #   id_cols = c(vax_scenario,vax_scenario_risk_group,antiviral_target_group,outcome,intervention),
  #   names_from = result,
  #   values_from = value)


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
  
  plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_boxplot(aes(x=value,y=vax_scenario,color=as.factor(antiviral_target_group)))  + 
    labs(title = paste(this_outcome), color = 'intervention') +
    ylab('')+
    xlab('doses to avert an outcome')
  
}
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = 4) 
ggsave(paste(rootpath,"x_results/plot_targetGroups_",time,".png",sep=''), width = 9.6, height = 5.7)
options(warn = 0)
#____________________________________________________________________________



### SAVE ####################################################################
save(RECORD_antiviral_model_simulations, file = paste(rootpath,"x_results/AntiviralRun_",time,".Rdata",sep=''))




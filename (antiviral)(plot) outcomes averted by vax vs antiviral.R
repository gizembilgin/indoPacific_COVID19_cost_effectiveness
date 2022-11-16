
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

#load latest antiviralSetUp_* (transmission model run for 1 year)
list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = "antiviralSetUp_*")
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste(rootpath,'x_results/',list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste(rootpath,"x_results/",latest_file,sep=''))

time.start.AntiviralSimulations=proc.time()[[3]]
#____________________________________________________________________________



### TOGGLES ################################################################
load(file = '1_inputs/last_fit_date.Rdata')

#____________________________________________________________________________



### ANTIVIRAL SIMULATIONS ##################################################
source(paste(getwd(),"/(antiviral)(function) antiviral_model_manger.R",sep=""))
source(paste(getwd(),"/(antiviral)(function) antiviral_model_worker.R",sep=""))
source(paste(getwd(),"/(antiviral)(function) stochastic_severe_outcomes_sampling.R",sep=""))
source(paste(getwd(),"/(antiviral)(function) stochastic_severe_outcomes_application.R",sep=""))

copy_function_into_cluster = antiviral_model_worker

RECORD_antiviral_model_simulations <- antiviral_model_manger(
 
  LIST_antiviral_start_date = c(as.Date('2023-01-01'),as.Date('2023-07-01')), 
  LIST_vax_scenario = c('all willing adults vaccinated with a primary schedule',
                          'all willing adults vaccinated with a primary schedule and high risk group recieve a booster', 
                          'all willing adults vaccinated with a primary schedule plus booster dose'),
  LIST_antiviral_target_group = "adults_with_comorbidities",
  toggle_high_risk_group = "adults_with_comorbidities",
  
  RECORD_antiviral_setup = RECORD_antiviral_setup,
  
  toggle_number_of_runs = 8,
  toggle_cluster_number = 4,
  
  toggle_stochastic_SO = "off",
  toggle_compare_to_vaccine_effect = "on",
  toggle_antiviral_type = 'nirmatrelvir_ritonavir ', #options:nirmatrelvir_ritonavir,molunipiravir           
  toggle_sensitivity_analysis = list(),
  pathway_to_care = "fixed",
  toggle_fixed_antiviral_coverage = 0.2
)
time.end.AntiviralSimulations=proc.time()[[3]]
(time.end.AntiviralSimulations - time.start.AntiviralSimulations)/60 # roughly 8 hours stochastically
#____________________________________________________________________________



### PLOT ####################################################################
LIST_outcomes = list('severe_disease', 
                     'hosp', 
                     'death', 
                     'YLL')

### Calculate # of antivirals per outcome averted
workshop = RECORD_antiviral_model_simulations %>% 
  filter( !(intervention == 'vaccine' & evaluation_group == 'high_risk')) %>% #change eval group here to change from high-risk to pop-level plot
  #filter(!(intervention == 'vaccine' & vax_scenario == 'all willing adults vaccinated with a primary schedule plus booster dose')) %>%
  filter(result %in% c("average_doses_per_outcome_averted","UCI_doses_per_outcome_averted","LCI_doses_per_outcome_averted")) %>%
  mutate(intervention = case_when(
   # intervention == 'antiviral' ~ paste('antiviral starting',antiviral_start_date),
    intervention == 'vaccine' ~ paste('booster dose starting 2023-01-01'),
    TRUE ~ intervention
  )) %>%
  pivot_wider(
    id_cols = c(vax_scenario,vax_scenario_risk_group,antiviral_target_group,outcome,intervention),
    names_from = result,
    values_from = value)


options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_pointrange(aes(x=average_doses_per_outcome_averted,y=vax_scenario,color=as.factor(intervention),xmin=LCI_doses_per_outcome_averted,xmax=UCI_doses_per_outcome_averted))  + 
    labs(title = paste(this_outcome), color = 'intervention') +
    ylab('')+
    xlim(0,max(workshop$UCI_doses_per_outcome_averted[workshop$outcome == this_outcome])) +
    xlab('doses to avert an outcome')
  
}
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = 4) 
options(warn = 0)

time = Sys.time()
time = gsub(':','-',time)
save(RECORD_antiviral_model_simulations, file = paste(rootpath,"x_results/VaxAntiviral_Comparison_",time,".Rdata",sep=''))




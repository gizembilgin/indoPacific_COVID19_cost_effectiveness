
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
#rm(list=ls())
rootpath = str_replace(getwd(), "GitHub_vaxAllocation","")

source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) antiviral_model_manger.R",sep=""))
source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) antiviral_model_worker.R",sep=""))
source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) stochastic_severe_outcomes_sampling.R",sep=""))
source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) stochastic_severe_outcomes_application.R",sep=""))
copy_function_into_cluster = antiviral_model_worker
RECORD_antiviral_model_simulations = data.frame()
#____________________________________________________________________________



### ANTIVIRAL SIMULATIONS ##################################################
time.start.AntiviralSimulations=proc.time()[[3]]

#for (setting in c("PNG","TLS","IDN","FJI","SLB","PHL")){
#for (setting_beta in c("PNG_high_beta")){ #options: "FJI", "SLE",PNG_high_beta, PNG_low_beta

  if (exists("master_toggles")){setting_beta = master_toggles$setting_beta} 
  setting = this_setting = substr(setting_beta,1,3)
  this_risk_group_name = "adults_with_comorbidities"
  
  #load latest antiviralSetUp_* (transmission model run for 1 year)
  list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = paste("antiviralSetUp_",setting_beta,"_",this_risk_group_name,"_*",sep=""))
  list_poss_Rdata_details = double()
  for (i in 1:length(list_poss_Rdata)){
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste(rootpath,'x_results/',list_poss_Rdata[[i]],sep=''))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste(rootpath,"x_results/",latest_file,sep=''))
  
  RECORD_antiviral_model_simulations_0 <- antiviral_model_manger(
    
    LIST_antiviral_start_date = c(as.Date('2023-01-01')
                                  #,as.Date('2023-07-01')
                                  ), 
    LIST_vax_scenario = c( "all willing adults vaccinated with a primary schedule"                                                                                                                      
                           ,"all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule"  
                           #,"all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a first booster dose"
                           #,"all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule"                      
                           #,"all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a first booster dose"                    
                           #,"all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults"                                                           
                           #,"catchup campaign for high-risk adults: assume booster to high-risk adults who have previously completed their primary schedule but have not recieved a booster"             
                           #,"catchup campaign for all adults: assume booster to all adults who have previously completed their primary schedule but have not recieved a booster"  
                           ),
    LIST_antiviral_target_group = list('adults_with_comorbidities' #baseline
                                       #,'unvaccinated_adults',
                                       #'unvaccinated_adults_AND_adults_with_comorbidities',
                                       #'all_adults'
                                       ),
    LIST_antiviral_type = list("nirmatrelvir_ritonavir"
                               #,"molunipiravir"
                               ), #options:nirmatrelvir_ritonavir,molunipiravir 
    toggle_high_risk_group = "adults_with_comorbidities",
    
    
    RECORD_antiviral_setup = RECORD_antiviral_setup,
    setting = setting,
    
    toggle_number_of_runs = 100, #DEFAULT 100
    toggle_cluster_number = 1,
    
    toggle_stochastic_SO = "off", # DEFAULT "on"
    toggle_compare_to_vaccine_effect = "on",
    
    toggle_sensitivity_analysis = list(),
    pathway_to_care = "fixed_RAT", #options: gold_standard (require data on healthcare seeking/access), fixed_RAT (probability of testing positive to a RAT within 5 days), fixed_direct (samples fixed proportion)
    toggle_fixed_antiviral_coverage = 1, #% of people with access to antivirals OR access to RAT tests -> antiviral
    manager_stochastic_VE_sampling = "uniform" # options: "normal" or "uniform"
  )
  
  RECORD_antiviral_model_simulations_0 = RECORD_antiviral_model_simulations_0 %>% mutate(country = setting, setting_beta = setting_beta)
  RECORD_antiviral_model_simulations = bind_rows(RECORD_antiviral_model_simulations,RECORD_antiviral_model_simulations_0)
#}

time.end.AntiviralSimulations=proc.time()[[3]]
(time.end.AntiviralSimulations - time.start.AntiviralSimulations)/60 # roughly 8 hours stochastically

temp_name = ''
time = Sys.time()
time = gsub(':','-',time)
time = paste(temp_name,time,sep='')
#____________________________________________________________________________


### SAVE ####################################################################
save(RECORD_antiviral_model_simulations, file = paste(rootpath,"x_results/AntiviralRun_",setting_beta,"_",this_risk_group_name,"_",time,".Rdata",sep=''))

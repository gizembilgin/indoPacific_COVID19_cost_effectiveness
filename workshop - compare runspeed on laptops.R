
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


#____________________________________________________________________________



### TOGGLES ################################################################
load(file = '1_inputs/last_fit_date.Rdata')
toggle_antiviral_start_date = fitted_max_date

pathway_to_care = 'fixed'
toggle_fixed_antiviral_coverage = 0.2
#toggle_vax_scenario = 'all willing adults vaccinated with a primary schedule and high risk group receive a booster'
#____________________________________________________________________________



### ANTIVIRAL SIMULATIONS ##################################################
source(paste(getwd(),"/(antiviral)(function) antiviral model.R",sep=""))

#detectCores() = 8

RunTime = data.frame()

for (num_clusters in c(2,4,6)){
  for (num_runs in c(1,5,10)){
    
    time.start.AntiviralSimulations=proc.time()[[3]]


    CLUSTER <- parallel::makeCluster(num_clusters) # create cluster
    doParallel::registerDoParallel(CLUSTER) # activate cluster
  
    RECORD_antiviral_model_simulations <- foreach::foreach(
      toggle_antiviral_type = c('paxlovid','molunipiravir'),
      .packages = c('tidyverse'),
      .combine = rbind,
      .inorder = FALSE
    )  %:%
      foreach(
        toggle_antiviral_target = c(
          'adults_with_comorbidities', #baseline
          'unvaccinated_adults',
          'unvaccinated_adults_AND_adults_with_comorbidities',
          'all_adults',
          'pregnant_women'
        ),
        .combine = rbind,
        .inorder = FALSE
      )  %:%
      foreach(
        toggle_vax_scenario = c(
          'all willing adults vaccinated with a primary schedule',
          'all willing adults vaccinated with a primary schedule and high risk group recieve a booster', #(baseline)
          'all willing adults vaccinated with a primary schedule plus booster dose'
        ),
        .combine = rbind,
        .inorder = FALSE
         )  %:%
        #linear!
        foreach(
          toggle_fixed_antiviral_coverage = seq(0.1,1,by=0.1),
          .combine = rbind,
          .inorder = FALSE
      ) %dopar% {
        
        if (toggle_antiviral_target == 'pregnant_women'){toggle_vax_scenario_risk_group = 'pregnant_women'
        } else {                                         toggle_vax_scenario_risk_group = 'adults_with_comorbidities'}
        
        antiviral_model(toggle_antiviral_start_date = as.Date('2023-01-01'),
                        toggle_antiviral_type          = toggle_antiviral_type,
                        toggle_antiviral_target        = toggle_antiviral_target,
                        toggle_vax_scenario            = toggle_vax_scenario,
                        toggle_vax_scenario_risk_group = toggle_vax_scenario_risk_group,
                        
                        RECORD_antiviral_setup          = RECORD_antiviral_setup,
                        
                        toggle_fixed_antiviral_coverage = toggle_fixed_antiviral_coverage,
                        
                        toggle_number_of_runs = num_runs,
                        toggle_stochastic_SO = "off"
        )
        
      }
  
  
  parallel::stopCluster(CLUSTER)
  time.end.AntiviralSimulations=proc.time()[[3]]
  num_minutes = (time.end.AntiviralSimulations - time.start.AntiviralSimulations)/60 # 2.4 minutes
  row = data.frame(clusters = num_clusters, runs = num_runs, minutes = num_minutes)
  RunTime = rbind(RunTime, row)
  
  time = Sys.time()
  time = gsub(':','-',time)
  write.csv(RunTime, file = paste(rootpath,"x_results/RunTime_",time,".csv",sep=''))
  }
}
#____________________________________________________________________________










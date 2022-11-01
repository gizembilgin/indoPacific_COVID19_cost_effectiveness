
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
toggle_antiviral_start_date = fitted_max_date
toggle_antiviral_delivery_capacity = 250 #daily capacity for antiviral delivery
toggle_number_of_runs = 1

pathway_to_care = 'fixed'
toggle_fixed_antiviral_coverage = 0.2
#____________________________________________________________________________



### ANTIVIRAL SIMULATIONS ##################################################
source(paste(getwd(),"/(antiviral)(function) antiviral model.R",sep=""))

#detectCores() = 8

CLUSTER <- parallel::makeCluster(4) # create cluster
doParallel::registerDoParallel(CLUSTER) # activate cluster

#nesting foreach loops as per https://cran.r-project.org/web/packages/foreach/vignettes/nested.html
system.time({
  RECORD_antiviral_model_simulations <- foreach::foreach(
    toggle_antiviral_type = c('paxlovid', #baseline
                              'molunipiravir'),
    .packages = c('tidyverse'),
    .combine = rbind,
    .inorder = FALSE
  ) %:% 
    foreach(
      toggle_antiviral_target = c(
        'adults_with_comorbidities', #baseline
        'unvaccinated_adults',
        'pregnant_women',
        'all_adults'
      ),
      .combine = rbind,
      .inorder = FALSE
    ) %:%
    foreach(
      toggle_vax_scenario = c(
        'all willing adults vaccinated with a primary schedule',
        'all willing adults vaccinated with a primary schedule and high risk group recieve a booster', #(baseline)
        'all willing adults vaccinated with a primary schedule plus booster dose'
      ),
      .combine = rbind,
      .inorder = FALSE
    ) %:%
    foreach(
      toggle_vax_scenario_risk_group = c(
        'adults_with_comorbidities', 
        'pregnant_women', 
        'none'
      ),
      .combine = rbind,
      .inorder = FALSE
    ) %dopar% {
      
      antiviral_model(toggle_antiviral_start_date = as.Date('2023-01-01'),
                      toggle_antiviral_type = toggle_antiviral_type,
                      toggle_antiviral_target = toggle_antiviral_target,
                      toggle_vax_scenario = toggle_vax_scenario,
                      toggle_vax_scenario_risk_group = toggle_vax_scenario_risk_group,
                      
                      RECORD_antiviral_setup = RECORD_antiviral_setup
      )
      
    }
})

parallel::stopCluster(CLUSTER)
#____________________________________________________________________________



### SAVE ####################################################################
save.image(file = paste(rootpath,"x_results/antiviralSimulation_fullImage_",Sys.Date(),".Rdata",sep=''))
save(RECORD_antiviral_model_simulations, file = paste(rootpath,"x_results/antiviralSimulations_",Sys.Date(),".Rdata",sep=''))

time.end.AntiviralSimulations=proc.time()[[3]]
(time.end.AntiviralSimulations - time.start.AntiviralSimulations)/60 # 3 minutes for single simulation of each, 11 minutes for 5 simulations of each (non-linear scaling!) -> ~3 hours for 100 sim each?


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

toggle_number_of_runs = 5

pathway_to_care = 'fixed'
toggle_VE_sensitivity_analysis = 'off'
toggle_vax_scenario = 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster'
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
    )  %:%
    foreach(
      toggle_fixed_antiviral_coverage = seq(0.05,1,by=0.05),
      .combine = rbind,
      .inorder = FALSE
    ) %dopar% {
      
      if (toggle_antiviral_target == 'pregnant_women'){toggle_vax_scenario_risk_group = 'pregnant_women'
      } else {                                         toggle_vax_scenario_risk_group = 'adults_with_comorbidities'}
      
      antiviral_model(toggle_antiviral_type          = toggle_antiviral_type,
                      toggle_antiviral_target        = toggle_antiviral_target,
                      toggle_vax_scenario            = toggle_vax_scenario,
                      toggle_vax_scenario_risk_group = toggle_vax_scenario_risk_group,
                      toggle_VE_sensitivity_analysis = toggle_VE_sensitivity_analysis,
                      
                      RECORD_antiviral_setup          = RECORD_antiviral_setup,
                      
                      toggle_fixed_antiviral_coverage = toggle_fixed_antiviral_coverage
      )
      
    }
})

parallel::stopCluster(CLUSTER)
#____________________________________________________________________________



### SAVE ####################################################################
save.image(file = paste(rootpath,"x_results/antiviralAccessToCare_fullImage_",Sys.Date(),".Rdata",sep=''))
save(RECORD_antiviral_model_simulations, file = paste(rootpath,"x_results/antiviralAccessToCare_",Sys.Date(),".Rdata",sep=''))

time.end.AntiviralSimulations=proc.time()[[3]]
(time.end.AntiviralSimulations - time.start.AntiviralSimulations)/60 # 3 minutes for single simulation of each, 11 minutes for 5 simulations of each (non-linear scaling!) -> ~3 hours for 100 sim each?
#____________________________________________________________________________



### PLOT ####################################################################
LIST_target_group = list('adults_with_comorbidities', 
                         'unvaccinated_adults',
                         'all_adults',
                         'pregnant_women'
                         )
LIST_outcomes = list('severe_disease', 
                     'hosp', 
                     'death', 
                     'YLL')
plot_list = list()
for (a in 1:length(LIST_target_group)){
  
  this_target_group = LIST_target_group[[a]]
  
  for (b in 1:length(LIST_outcomes)){
    
    this_outcome = LIST_outcomes[[b]]
    this_simulation_subset = RECORD_antiviral_model_simulations %>%
      filter(antiviral_target == this_target_group,
             outcome == this_outcome) %>% 
      pivot_wider(
               id_cols = c(antiviral_cov,antiviral_type,antiviral_target),
               names_from = result,
               values_from = value)
    
    plot_list[[length(LIST_target_group)*(a-1)+b]] = 
      ggplot(data = this_simulation_subset) + 
      geom_point(aes(x=antiviral_cov,y=percentage,color=as.factor(antiviral_type))) +
      geom_errorbar(aes(x=antiviral_cov,ymin=LCI_percentage,ymax=UCI_percentage))
    
    
  }
  

}

ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
          plot_list[[5]],plot_list[[6]],plot_list[[7]], plot_list[[8]],
          plot_list[[9]],plot_list[[10]],plot_list[[11]], plot_list[[12]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 4,
          nrow = 1)

ggarrange(plot_list[[13]],plot_list[[14]],plot_list[[15]], plot_list[[16]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 4,
          nrow = 1)


### DEBUG
antiviral_model(toggle_antiviral_type          = 'paxlovid',
                toggle_antiviral_target        =  'unvaccinated_adults',
                toggle_vax_scenario            = toggle_vax_scenario,
                toggle_vax_scenario_risk_group = 'adults_with_comorbidities',
                toggle_VE_sensitivity_analysis = toggle_VE_sensitivity_analysis,
                
                RECORD_antiviral_setup          = RECORD_antiviral_setup,
                
                toggle_fixed_antiviral_coverage = 0.05
)
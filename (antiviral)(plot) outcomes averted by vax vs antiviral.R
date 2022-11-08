
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

pathway_to_care = 'fixed'
toggle_fixed_antiviral_coverage = 0.2
toggle_antiviral_type = "paxlovid"
toggle_antiviral_target = toggle_vax_scenario_risk_group = "adults_with_comorbidities"

toggle_stochastic_SO = "on"
toggle_number_of_runs = 50
#____________________________________________________________________________



### ANTIVIRAL SIMULATIONS ##################################################
source(paste(getwd(),"/(antiviral)(function) antiviral model.R",sep=""))

#detectCores() = 8
CLUSTER <- parallel::makeCluster(4) # create cluster
doParallel::registerDoParallel(CLUSTER) # activate cluster

#nesting foreach loops as per https://cran.r-project.org/web/packages/foreach/vignettes/nested.html
system.time({
  RECORD_antiviral_model_simulations <- foreach::foreach(
    toggle_vax_scenario = c(
      'all willing adults vaccinated with a primary schedule',
      'all willing adults vaccinated with a primary schedule and high risk group recieve a booster', #(baseline)
      'all willing adults vaccinated with a primary schedule plus booster dose'
    ),
    .packages = c('tidyverse'),
    .combine = rbind,
    .inorder = FALSE
  )  %dopar% {
      
      antiviral_model(toggle_antiviral_start_date = as.Date('2023-01-01'),
                      toggle_antiviral_type          = toggle_antiviral_type,
                      toggle_antiviral_target        = toggle_antiviral_target,
                      toggle_vax_scenario            = toggle_vax_scenario,
                      toggle_vax_scenario_risk_group = toggle_vax_scenario_risk_group,
                      
                      RECORD_antiviral_setup          = RECORD_antiviral_setup,
                      
                      toggle_fixed_antiviral_coverage = toggle_fixed_antiviral_coverage,
                      
                      toggle_number_of_runs = toggle_number_of_runs,
                      toggle_stochastic_SO = toggle_stochastic_SO,
                      toggle_compare_to_vaccine_effect = "on"
      )
      
    }
})

parallel::stopCluster(CLUSTER)

#booster rollout for adults with comorb ends around March/April, check with booster starting on 01/01
additional_scenario = antiviral_model(toggle_antiviral_start_date = as.Date('2023-06-01'),
                                      toggle_antiviral_type          = toggle_antiviral_type,
                                      toggle_antiviral_target        = toggle_antiviral_target,
                                      toggle_vax_scenario            = 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster',
                                      toggle_vax_scenario_risk_group = toggle_vax_scenario_risk_group,
                                      
                                      RECORD_antiviral_setup          = RECORD_antiviral_setup,
                                      
                                      toggle_fixed_antiviral_coverage = toggle_fixed_antiviral_coverage,
                                      
                                      toggle_number_of_runs = toggle_number_of_runs,
                                      toggle_stochastic_SO = toggle_stochastic_SO
)

RECORD_antiviral_model_simulations = rbind(RECORD_antiviral_model_simulations,additional_scenario)
#____________________________________________________________________________



### PLOT ####################################################################
LIST_outcomes = list('severe_disease', 
                     'hosp', 
                     'death', 
                     'YLL')

### Calculate # of antivirals per outcome averted
workshop = RECORD_antiviral_model_simulations %>% 
  filter(intervention == 'antiviral' | (intervention == 'vaccine' & evaluation_group == 'high_risk')) %>% #change eval group here to change from high-risk to pop-level plot
  filter(result %in% c("average_doses_per_outcome_averted","UCI_doses_per_outcome_averted","LCI_doses_per_outcome_averted")) %>%
  mutate(intervention = case_when(
    intervention == 'antiviral' ~ paste('antiviral starting',antiviral_start_date),
    intervention == 'vaccine' ~ paste('booster dose starting 2023-01-01')
  )) %>%
  pivot_wider(
    id_cols = c(outcome,intervention,vax_scenario,antiviral_start_date),
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


time.end.AntiviralSimulations=proc.time()[[3]]
(time.end.AntiviralSimulations - time.start.AntiviralSimulations)/60 # 2.4 minutes

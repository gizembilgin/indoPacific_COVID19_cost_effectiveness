
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

pathway_to_care = 'fixed'
toggle_fixed_antiviral_coverage = 0.2
#toggle_vax_scenario = 'all willing adults vaccinated with a primary schedule and high risk group receive a booster'
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
    #linear!
    # ) %:%
    # foreach(
    #   toggle_fixed_antiviral_coverage = seq(0.05,1,by=0.05),
    #   .combine = rbind,
    #   .inorder = FALSE
    ) %dopar% {
      
      if (toggle_antiviral_target == 'pregnant_women'){toggle_vax_scenario_risk_group = 'pregnant_women'
      } else {                                         toggle_vax_scenario_risk_group = 'adults_with_comorbidities'}
      
      antiviral_model(toggle_antiviral_type          = toggle_antiviral_type,
                      toggle_antiviral_target        = toggle_antiviral_target,
                      toggle_vax_scenario            = toggle_vax_scenario,
                      toggle_vax_scenario_risk_group = toggle_vax_scenario_risk_group,
                      
                      RECORD_antiviral_setup          = RECORD_antiviral_setup,
                      
                      toggle_fixed_antiviral_coverage = toggle_fixed_antiviral_coverage,
                      
                      toggle_number_of_runs = 5
      )
      
    }
})

parallel::stopCluster(CLUSTER)
#____________________________________________________________________________



### SAVE ####################################################################
save.image(file = paste(rootpath,"x_results/antiviralAccessToCare_fullImage_",Sys.Date(),".Rdata",sep=''))
save(RECORD_antiviral_model_simulations, file = paste(rootpath,"x_results/antiviralAccessToCare_",Sys.Date(),".Rdata",sep=''))

time.end.AntiviralSimulations=proc.time()[[3]]
(time.end.AntiviralSimulations - time.start.AntiviralSimulations)/60 # 20 minutes for 5 runs each
#____________________________________________________________________________



### PLOT ####################################################################
LIST_target_group = list('adults_with_comorbidities', 
                         'unvaccinated_adults',
                         'unvaccinated_adults_AND_adults_with_comorbidities',
                         'all_adults',
                         'pregnant_women'
                         )
LIST_outcomes = list('severe_disease', 
                     'hosp', 
                     'death', 
                     'YLL')
# plot_list = list()
# for (a in 1:length(LIST_target_group)){
#   
#   this_target_group = LIST_target_group[[a]]
#   
#   for (b in 1:length(LIST_outcomes)){
#     
#     this_outcome = LIST_outcomes[[b]]
#     this_simulation_subset = RECORD_antiviral_model_simulations %>%
#       filter(antiviral_target == this_target_group,
#              outcome == this_outcome) %>% 
#       pivot_wider(
#                id_cols = c(antiviral_cov,antiviral_type,antiviral_target),
#                names_from = result,
#                values_from = value)
#     
#     plot_list[[length(LIST_outcomes)*(a-1)+b]] = 
#       ggplot(data = this_simulation_subset) + 
#       geom_point(aes(x=antiviral_cov*100,y=percentage,color=as.factor(antiviral_type))) +
#       geom_errorbar(aes(x=antiviral_cov*100,ymin=LCI_percentage,ymax=UCI_percentage)) + 
#       xlab('antiviral coverage (%)')
#     
#     
#   }
#   
# 
# }
# 
# #MAIN PAPER - target pop = adults with comorb, unvax, unvax & adults with comorb, all_adults
# ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
#           plot_list[[5]],plot_list[[6]],plot_list[[7]], plot_list[[8]],
#           plot_list[[9]],plot_list[[10]],plot_list[[11]], plot_list[[12]],
#           common.legend = TRUE,
#           legend="bottom",
#           ncol = 4,
#           nrow = 3)
# 
# #SUPPLEMENTARY MATERIAL - target pop = preg women
# ggarrange(plot_list[[13]],plot_list[[14]],plot_list[[15]], plot_list[[16]],
#           common.legend = TRUE,
#           legend="bottom",
#           ncol = 4,
#           nrow = 1)
# 
# 
# #by #
# 
# this_antiviral = 'paxlovid'
# plot_list = list()
# for (a in 1:length(LIST_outcomes)) {
#   this_outcome = LIST_outcomes[[a]]
#   this_simulation_subset = RECORD_antiviral_model_simulations %>%
#     filter(! antiviral_target %in% 'pregnant_women' ) %>%
#     filter(outcome == this_outcome &
#              antiviral_type == this_antiviral) %>%
#     pivot_wider(
#       id_cols = c(antiviral_cov,antiviral_delivered, antiviral_type, antiviral_target),
#       names_from = result,
#       values_from = value
#     )
#   
#   plot_list[[a]] =
#     ggplot(data = this_simulation_subset) +
#     geom_point(aes(
#       x = antiviral_delivered,
#       y = percentage,
#       color = as.factor(antiviral_target),
#       #shape = as.factor(antiviral_type)
#     ))  +
#     labs(title = paste(this_outcome),
#          color = 'antiviral target group') +
#     xlab('antiviral doses delivered')
#   
# }
# ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
#           common.legend = TRUE,
#           legend="bottom",
#           ncol = 2,
#           nrow = 2)  


### Calculate # of antivirals per outcome averted
workshop = RECORD_antiviral_model_simulations %>%
  mutate(doses_to_avert_outcome = antiviral_delivered/value) %>%
  group_by(outcome,result,antiviral_type,antiviral_target,vax_scenario,vax_scenario_risk_group,VE_sensitivity_analysis) %>%
  summarise(doses_to_avert_outcome = mean(doses_to_avert_outcome)) %>% 
  pivot_wider(
    id_cols = c(outcome,antiviral_target,vax_scenario,antiviral_type),
    names_from = result,
    values_from = doses_to_avert_outcome)

workshop = workshop %>% 
  filter(antiviral_type == 'paxlovid' & antiviral_target != 'pregnant_women') %>%
  ungroup() %>%
  select(outcome,vax_scenario,antiviral_target,average, LCI, UCI)

ggplot(data = workshop) +
  geom_pointrange(aes(x=average,y=outcome,color=as.factor(antiviral_target),xmin=LCI,xmax=UCI))  + 
  labs(color = 'antiviral eligible group') +
  xlab('antiviral doses per outcome averted')

plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_pointrange(aes(x=average,y=vax_scenario,color=as.factor(antiviral_target),xmin=LCI,xmax=UCI))  + 
    labs(title = paste(this_outcome), color = 'antiviral eligible group') +
    ylab('')+
    xlab('antiviral doses per outcome averted')
  
}
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = 4) 


### DEBUG
# antiviral_model(toggle_antiviral_type          = 'paxlovid',
#                 toggle_antiviral_target        =  'unvaccinated_adults',
#                 toggle_vax_scenario            = 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster',
#                 toggle_vax_scenario_risk_group = 'adults_with_comorbidities',
# 
#                 RECORD_antiviral_setup          = RECORD_antiviral_setup,
# 
#                 toggle_fixed_antiviral_coverage = 0.05
# )
# 
# likelihood_severe_outcome %>% group_by(outcome,dose) %>% summarise(percentage = mean(percentage,na.rm=TRUE))
# # 1 death              0   0.00202 
# # 2 death              1   0.00123 
# # 3 death              2   0.000887
# # 4 hosp               0   0.0551  
# # 5 hosp               1   0.0383  
# # 6 hosp               2   0.0302  
# # 7 severe_disease     0   0.0197  
# # 8 severe_disease     1   0.0126  
# # 9 severe_disease     2   0.00886 
# # 10 YLL                0   0.0197  
# # 11 YLL                1   0.0122  
# # 12 YLL                2   0.00921 
# 
# workshop = likelihood_severe_outcome %>% filter(is.na(percentage) == TRUE)
# unique(workshop$date) # "2022-10-06"

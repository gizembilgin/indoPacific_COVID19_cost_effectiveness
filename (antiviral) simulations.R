
### SETUP
load(file = paste(rootpath,"x_results/antiviralSetUp_",Sys.Date(),".Rdata",sep=''))
#start timing
time.start.AntiviralSimulations=proc.time()[[3]]

### TOGGLES ################################################################
load(file = '1_inputs/last_fit_date.Rdata')
toggle_antiviral_start_date = fitted_max_date
toggle_antiviral_delivery_capacity = 250 #daily capacity for antiviral delivery
toggle_number_of_runs = 100
#____________________________________________________________________________



LIST_toggle_antiviral_type = list('paxlovid', #(baseline)
                                  'molunipiravir')

LIST_toggle_antiviral_target = list('adults_with_comorbidities', #(baseline)
                                    'unvaccinated_adults', 
                                    'pregnant_women', 
                                    'all_adults')

LIST_toggle_vax_scenario = list('all willing adults vaccinated with a primary schedule',
                                'all willing adults vaccinated with a primary schedule and high risk group recieve a booster', #(baseline)
                                'all willing adults vaccinated with a primary schedule plus booster dose')

LIST_toggle_vax_scenario_risk_group = list('adults_with_comorbidities', 
                                           'pregnant_women', 
                                           'none')

LIST_toggle_VE_sensitivity_analysis = list('on','off')


queue = list()
queue_place = 0
for (a in 1:length(LIST_toggle_antiviral_type)){
  toggle_antiviral_type = LIST_toggle_antiviral_type[[a]]
  
  for (b in 1:length(LIST_toggle_antiviral_target)){
    toggle_antiviral_target = LIST_toggle_antiviral_target[[b]]
    
    for (c in 1:length(LIST_toggle_vax_scenario)){
      toggle_vax_scenario = LIST_toggle_vax_scenario[[c]]
      
      for (d in 1:length(LIST_toggle_vax_scenario_risk_group)){
        toggle_vax_scenario_risk_group = LIST_toggle_vax_scenario_risk_group[[d]]
        if (toggle_vax_scenario_risk_group == 'none' & toggle_vax_scenario == 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster'){
          #require risk group to exist to be able to deliver additional booster doses
        } else if(toggle_vax_scenario_risk_group %in% c('pregnant_women','adults_with_comorbidities') & 
                  toggle_antiviral_target %in% c('pregnant_women','adults_with_comorbidities') &
                  toggle_vax_scenario_risk_group != toggle_antiviral_target){
          #require risk group name to exist to be able to deliver antivirals
        } else {
          for (e in 1:length(LIST_toggle_VE_sensitivity_analysis)){
            toggle_VE_sensitivity_analysis = LIST_toggle_VE_sensitivity_analysis[[e]]
            if (toggle_VE_sensitivity_analysis == "on" & toggle_vax_scenario_risk_group != 'adults_with_comorbidities'){
              # do not run as sensitivity analysis only for reduced VE in adults with comorbidities and older adults
            } else{
              ###LOAD DEPENDENCIES
              #By toggle_vax_scenario, toggle_vax_scenario_risk_group, and toggle_VE_sensitivity_analysis
              outcomes_without_antivirals = RECORD_antiviral_setup$outcomes_without_antivirals %>%
                filter(toggle_vax_scenario == toggle_vax_scenario,
                       toggle_vax_scenario_risk_group == toggle_vax_scenario_risk_group,
                       toggle_VE_sensitivity_analysis == toggle_VE_sensitivity_analysis)
              likelihood_severe_outcome = RECORD_antiviral_setup$likelihood_severe_outcome %>%
                filter(toggle_vax_scenario == toggle_vax_scenario,
                       toggle_vax_scenario_risk_group == toggle_vax_scenario_risk_group,
                       toggle_VE_sensitivity_analysis == toggle_VE_sensitivity_analysis)
              incidence_log_tidy = RECORD_antiviral_setup$incidence_log_tidy %>%
                filter(toggle_vax_scenario == toggle_vax_scenario,
                       toggle_vax_scenario_risk_group == toggle_vax_scenario_risk_group,
                       toggle_VE_sensitivity_analysis == toggle_VE_sensitivity_analysis)
              prop_sympt = RECORD_antiviral_setup$prop_sympt 
              
              ###CONFIGURE QUEUE
              queue_place = queue_place + 1
              queue[[queue_place]] = list(outcomes_without_antivirals = outcomes_without_antivirals,
                                          likelihood_severe_outcome = likelihood_severe_outcome,
                                          incidence_log_tidy = incidence_log_tidy,
                                          prop_sympt = prop_sympt)
            }
          }
        }
      }
    }
  }
}


library(doParallel); library(parallel); library(foreach)
CLUSTER <- parallel::makeCluster(detectCores()/2) # create cluster
doParallel::registerDoParallel(CLUSTER) # activate cluster

# run parallel
time_parallel <- system.time({
  RECORD_antiviral_model_simulations <- foreach::foreach(ticket = 1:5,
                        .combine = rbind) %dopar% {
                          outcomes_without_antivirals = queue[[ticket]]$outcomes_without_antivirals
                          likelihood_severe_outcome = queue[[ticket]]$likelihood_severe_outcome
                          incidence_log_tidy = queue[[ticket]]$incidence_log_tidy
                          prop_sympt = queue[[ticket]]$prop_sympt
                          source(paste(getwd(),"/(antiviral) antiviral delivery pathway.R",sep=""))
                          
                          summary_over_runs_tidy %>% 
                            mutate(toggle_antiviral_type = toggle_antiviral_type,
                                   toggle_antiviral_target = toggle_antiviral_target,
                                   toggle_vax_scenario = toggle_vax_scenario,
                                   toggle_vax_scenario_risk_group = toggle_vax_scenario_risk_group,
                                   toggle_VE_sensitivity_analysis = toggle_VE_sensitivity_analysis)
                        }
})
time_parallel

# close cluster
parallel::stopCluster(CLUSTER)

save.image(file = paste(rootpath,"x_results/antiviralSimulation_fullImage_",Sys.Date(),".Rdata",sep=''))
save(RECORD_antiviral_model_simulations, file = paste(rootpath,"x_results/antiviralSimulations_",Sys.Date(),".Rdata",sep=''))

time.end.AntiviralSimulations=proc.time()[[3]]
time.start.AntiviralSimulations-time.end.AntiviralSimulations



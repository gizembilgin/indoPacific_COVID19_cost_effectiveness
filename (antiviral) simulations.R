
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
toggle_number_of_runs = 5
#____________________________________________________________________________



### ANTIVIRAL SIMULATIONS ##################################################
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
    ) %:%
    foreach(
      toggle_VE_sensitivity_analysis = c(
        'on',
        'off'
      ),
      .combine = rbind,
      .inorder = FALSE
    ) %dopar% {
      
    ###SKIP IF NONSENSE
    skip = 0
    if (toggle_vax_scenario_risk_group == 'none' & toggle_vax_scenario == 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster'){
      #require risk group to exist to be able to deliver additional booster doses
      skip = 1
    } 
    if((toggle_vax_scenario_risk_group %in% c('pregnant_women','adults_with_comorbidities') |
        toggle_antiviral_target %in% c('pregnant_women','adults_with_comorbidities')) &
       toggle_vax_scenario_risk_group != toggle_antiviral_target){
      #require risk group name to exist to be able to deliver antivirals
      skip = 1
    }
    if (toggle_VE_sensitivity_analysis == "on" & toggle_vax_scenario_risk_group != 'adults_with_comorbidities'){
      # do not run as sensitivity analysis only for reduced VE in adults with comorbidities and older adults
      skip = 1
    }
    
    if (skip == 1){
      
    } else{
      ###LOAD DEPENDENCIES
      #DEPENDENCIES FROM PRIMARY MODEL (n=5): incidence_log_tidy, severe_outcome_log_tidy, severe_outcome_this_run, reinfection_protection, param_age (-> prop_sympt)
      #DEPENDENCIES FROM '(antiviral) set up.R' (n=4): incidence_log_tidy,outcomes_without_antivirals,likelihood_severe_outcome,prop_sympt
      
      outcomes_without_antivirals = RECORD_antiviral_setup$outcomes_without_antivirals %>%
        filter(
          vax_scenario            == toggle_vax_scenario,
          vax_scenario_risk_group == toggle_vax_scenario_risk_group,
          VE_sensitivity_analysis == toggle_VE_sensitivity_analysis
        )
      likelihood_severe_outcome = RECORD_antiviral_setup$likelihood_severe_outcome %>%
        filter(
          vax_scenario            == toggle_vax_scenario,
          vax_scenario_risk_group == toggle_vax_scenario_risk_group,
          VE_sensitivity_analysis == toggle_VE_sensitivity_analysis
        )
      incidence_log_tidy = RECORD_antiviral_setup$incidence_log_tidy %>%
        filter(
          vax_scenario            == toggle_vax_scenario,
          vax_scenario_risk_group == toggle_vax_scenario_risk_group,
          VE_sensitivity_analysis == toggle_VE_sensitivity_analysis
        )
      prop_sympt = RECORD_antiviral_setup$prop_sympt
      
      #CHECK
      if (toggle_antiviral_target %in% c('adults_with_comorbidities', 'pregnant_women')) {
        if (!toggle_antiviral_target %in% unique(incidence_log_tidy$risk_group)) {
          stop('target for antivirals not included as a risk group in model run!')
        }
      }
      
      #COMEBACK could make stochastic
      if (toggle_antiviral_type == 'paxlovid') {
        toggle_antiviral_effectiveness = 0.88
      } else if (toggle_antiviral_type == 'molunipiravir') {
        toggle_antiviral_effectiveness = 0.33
      }
      #____________________________________________________________________________
      
      
      ### INITALISE DATA FRAMES #################################################################
      this_scenario_tracker = data.frame()
      #____________________________________________________________________________

      
      
      ### SELECT TARGET GROUP ######################################################
      if (toggle_antiviral_target %in% c('adults_with_comorbidities', 'pregnant_women')) {
        antiviral_target = incidence_log_tidy %>%
          filter(risk_group == toggle_antiviral_target)
      } else if (toggle_antiviral_target == 'unvaccinated_adults') {
        antiviral_target = incidence_log_tidy %>%
          filter(dose == 0)
      } else if (toggle_antiviral_target == 'all_adults') {
        antiviral_target = incidence_log_tidy %>%
          filter(!age_group %in% c("0 to 4", "5 to 9", "10 to 17"))
      } else {
        stop('pick a valid toggle_antiviral_target!')
      }
      antiviral_target = antiviral_target %>% filter(incidence >
                                                       0,
                                                     date >= toggle_antiviral_start_date)
      
      antiviral_delivery_length = as.numeric(max(incidence_log_tidy$date) - toggle_antiviral_start_date)
      rm(incidence_log_tidy)
      #____________________________________________________________________________
      
      
      ### SELECT SYMPTOMATIC INDIVIDUAL#############################################
      #ASSUMPTION: vaccination does not affect the likelihood of an individual being symptomatic
      antiviral_target = antiviral_target %>%
        left_join(prop_sympt, by = c('age_group' = 'agegroup')) %>%
        mutate(symptomatic = incidence * value) %>%
        select(-incidence,-value)
      
      antiviral_target_individuals = antiviral_target %>%
        mutate(symptomatic = round(symptomatic)) %>%
        select(-temp_risk) %>%
        filter(symptomatic > 0)
      
      rm(antiviral_target)
      #____________________________________________________________________________
      
      
      ### CREATE DATASET WITH INDIVIDUALS ############################################
      workshop = as.data.frame(
        lapply(
          antiviral_target_individuals,
          rep,
          antiviral_target_individuals$symptomatic
        )
      )
      if (!nrow(workshop) == sum(antiviral_target_individuals$symptomatic)) {
        stop('split to individuals has lost individuals')
      }
      
      antiviral_target_individuals = workshop %>% select(-symptomatic)
      antiviral_target_individuals$ID <-
        seq.int(nrow(antiviral_target_individuals)) #helpful for checking
      
      total_target = nrow(antiviral_target_individuals)
      #____________________________________________________________________________
      
      
      ###### BEGIN STOCHASTIC
      for (run_number in 1:toggle_number_of_runs) {
        ### PATHWAY TO CARE STEP ONE: Does this individual seek care? ################
        healthcare_seeking = function(age_group) {
          #COMEBACK: need real data to estimate
          if (age_group %in% c("0 to 4",    "5 to 9",    "10 to 17")) {
            sample = rbinom(1, 1, 0.5) #rbinom(number of observations,number of trials,probability of success on each trial)
          } else{
            sample = rbinom(1, 1, 0.25)
          }
          return(sample)
        }
        
        workshop <-
          as.data.frame(sapply(
            antiviral_target_individuals$age_group,
            healthcare_seeking
          ))
        colnames(workshop) = c('healthcare_seeking')
        workshop = cbind(antiviral_target_individuals, workshop)
        
        antiviral_target_individuals_run = workshop %>%
          filter(healthcare_seeking == 1) %>% #retain those who seek care
          select(-healthcare_seeking)
        
        rm(workshop)
        #____________________________________________________________________________
        
        
        ### PATHWAY TO CARE STEP TWO: How many days after symptom onset can the individual access care?#######
        #ASSUMPTION: incidence is day 0 of symptom onset
        
        healthcare_access = function(age_group) {
          #COMEBACK: need real data to estimate
          if (age_group %in% c("0 to 4",    "5 to 9",    "10 to 17")) {
            sample = round(runif(1, min = 0, max = 7))
          } else{
            sample = round(runif(1, min = 0, max = 7))
          }
          return(sample)
        }
        
        workshop <-
          as.data.frame(sapply(
            antiviral_target_individuals_run$age_group,
            healthcare_access
          ))
        colnames(workshop) = c('healthcare_access')
        workshop = cbind(antiviral_target_individuals_run, workshop)
        
        antiviral_target_individuals_run = workshop %>%
          filter(healthcare_access < 6) %>% #retain those who access care within 5 days
          mutate(min_date_access = date + healthcare_access,
                 max_date_access = date + 5) %>%
          select(-healthcare_access)
        
        rm(workshop)
        #____________________________________________________________________________
        
        
        ### PATHWAY TO CARE STEP THREE: Is the individual allocated antivirals?#######
        #ASSUMPTION: allocated at random
        antiviral_delivery_tracker = data.frame()
        
        for (day in 0:antiviral_delivery_length) {
          this_date =  toggle_antiviral_start_date + day
          
          #include all presenting within
          presentations_this_date = antiviral_target_individuals_run %>%
            filter(this_date >= min_date_access &
                     this_date <= max_date_access) %>%
            filter(!ID %in% antiviral_delivery_tracker$ID) #make sure previous recipients removed from future decisions
          
          num_to_sample = min(nrow(presentations_this_date),
                              toggle_antiviral_delivery_capacity) #making sure number of doses delivered <= number of available recipients
          
          antiviral_recipients_this_date = data.frame(ID = sample(presentations_this_date$ID, num_to_sample, replace =
                                                                    FALSE))
          antiviral_recipients_this_date = antiviral_recipients_this_date %>% mutate(date = this_date)
          antiviral_delivery_tracker = rbind(antiviral_delivery_tracker,
                                             antiviral_recipients_this_date)
        }
        
        antiviral_target_individuals_run = antiviral_delivery_tracker %>%
          select(-date) %>%
          left_join(antiviral_target_individuals_run, by = 'ID') #remove all not selected for antivirals
        
        length_antiviral_delivery_tracker = nrow(antiviral_delivery_tracker)
        rm(antiviral_delivery_tracker)
        #____________________________________________________________________________
        
        
        ### PATHWAY TO CARE STEP FOUR: How many cases of severe disease are prevented?#######
        workshop = antiviral_target_individuals_run %>%
          left_join(
            likelihood_severe_outcome,
            by = c("date", "risk_group", "age_group", "dose", "vaccine_type")
          ) %>%
          mutate(percentage = percentage * toggle_antiviral_effectiveness)
        workshop = na.omit(workshop)
        
        prevented_by_antivirals = workshop %>%
          group_by(outcome) %>%
          summarise(n = sum(percentage))
        
        this_scenario_tracker = rbind(this_scenario_tracker, prevented_by_antivirals)
        
        
        #____________________________________________________________________________
      }
      rm(
        antiviral_target_individuals_run,
        workshop,
        prevented_by_antivirals,
        likelihood_severe_outcome,
        antiviral_target_individuals
      )
      #____________________________________________________________________________
      
      
      ### CREATE SUMMARY OVER RUNS #################################################
      summary_over_runs <-
        this_scenario_tracker %>%
        group_by(outcome) %>%
        dplyr::summarise(
          average = mean(n),
          sd = sd(n),
          UCI = average - qnorm(0.975) *
            sd,
          LCI = average - qnorm(0.023) *
            sd
        ) %>%
        left_join(outcomes_without_antivirals, by = "outcome") %>%
        mutate(
          percentage = average / overall * 100,
          UCI_percentage = UCI / overall * 100,
          LCI_percentage = LCI / overall * 100
        ) %>%
        select(outcome,
               average,
               UCI,
               LCI,
               percentage,
               UCI_percentage,
               LCI_percentage)
      
      summary_over_runs_tidy = summary_over_runs %>%
        pivot_longer(
          cols = 2:ncol(summary_over_runs) ,
          names_to = 'result',
          values_to = 'value'
        )
      
      rm(this_scenario_tracker,
         summary_over_runs,
         outcomes_without_antivirals)
      
      #monitor if daily capacity being used or not enough seeking/accessing care
      antiviral_rollout_capacity_utilised = round(
        100 * length_antiviral_delivery_tracker / (
          toggle_antiviral_delivery_capacity * antiviral_delivery_length
        ),
        digits = 1
      )
      antiviral_eligible_pop_coverage = round(100 *
                                                length_antiviral_delivery_tracker / total_target, digits = 1)
      row1 = c(outcome = 'program_measure',
               result = 'antiviral_rollout_capacity_utilised',
               value = antiviral_rollout_capacity_utilised)
      row2 = c(outcome = 'program_measure',
               result = 'antiviral_eligible_pop_coverage',
               value = antiviral_eligible_pop_coverage)
      
      summary_over_runs_tidy = rbind(summary_over_runs_tidy, row1, row2)
      #____________________________________________________________________________
  
      
      #time.end.AntiviralModel=proc.time()[[3]]
      #time.end.AntiviralModel-time.start.AntiviralModel
      #1 run takes 7.36 seconds (16/09)
      #10 runs takes 65.54 seconds(16/09)
      #100 runs take 667.32 seconds (16/09)
      #estimating 1000 runs will take ~2 hours (although would CI converge too much?)

      
      summary_over_runs_tidy %>%
        mutate(
          antiviral_type = toggle_antiviral_type,
          antiviral_target = toggle_antiviral_target,
          vax_scenario = toggle_vax_scenario,
          vax_scenario_risk_group = toggle_vax_scenario_risk_group,
          VE_sensitivity_analysis = toggle_VE_sensitivity_analysis
        )
    }
  }
})

parallel::stopCluster(CLUSTER)
#____________________________________________________________________________



### SAVE ####################################################################
save.image(file = paste(rootpath,"x_results/antiviralSimulation_fullImage_",Sys.Date(),".Rdata",sep=''))
save(RECORD_antiviral_model_simulations, file = paste(rootpath,"x_results/antiviralSimulations_",Sys.Date(),".Rdata",sep=''))

time.end.AntiviralSimulations=proc.time()[[3]]
(time.end.AntiviralSimulations - time.start.AntiviralSimulations)/60 # 3 minutes for single simulation of each, 11 minutes for 5 simulations of each (non-linear scaling!) -> ~3 hours for 100 sim each?

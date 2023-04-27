rootpath = str_replace(getwd(), "GitHub_vaxAllocation","")

source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) antiviral_model_manger.R",sep=""))
source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) antiviral_model_worker.R",sep=""))
source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) stochastic_severe_outcomes_sampling.R",sep=""))
source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) stochastic_severe_outcomes_application.R",sep=""))

stochastiC_VE_timer = data.frame()

for (this_setting_beta in c("TLS","PNG_low_beta")){ #IDN and FJI don't yet have the updated AntiviralSetup
  
  time.start.AntiviralSimulations=proc.time()[[3]]
  
  setting_beta = this_setting_beta
  setting = this_setting = substr(this_setting_beta,1,3)
  this_risk_group_name = "adults_with_comorbidities"
  
  #load latest antiviralSetUp_* (transmission model run for 1 year)
  list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = paste("antiviralSetUp_",this_setting_beta,"_",this_risk_group_name,"_*",sep=""))
  list_poss_Rdata_details = double()
  for (i in 1:length(list_poss_Rdata)){
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste(rootpath,'x_results/',list_poss_Rdata[[i]],sep=''))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste(rootpath,"x_results/",latest_file,sep=''))
  
  RECORD_antiviral_model_simulations_0 <- antiviral_model_manger(
    
    LIST_antiviral_start_date = c(as.Date('2023-01-01'),as.Date('2023-07-01')), 
    LIST_vax_scenario = unique(RECORD_antiviral_setup$outcomes_without_antivirals$vax_scenario),
    LIST_antiviral_target_group = list('adults_with_comorbidities', #baseline
                                       'unvaccinated_adults',
                                       'unvaccinated_adults_AND_adults_with_comorbidities',
                                       'all_adults'),
    LIST_antiviral_type = list("nirmatrelvir_ritonavir","molunipiravir"), #options:nirmatrelvir_ritonavir,molunipiravir 
    toggle_high_risk_group = "adults_with_comorbidities",
    
    
    RECORD_antiviral_setup = RECORD_antiviral_setup,
    setting = setting,
    
    toggle_number_of_runs = 1, #DEFAULT 100
    toggle_cluster_number = 1,
    
    toggle_stochastic_SO = "on", # DEFAULT "on"
    toggle_compare_to_vaccine_effect = "on",
    
    toggle_sensitivity_analysis = list(),
    pathway_to_care = "fixed_RAT", #options: gold_standard (require data on healthcare seeking/access), fixed_RAT (probability of testing positive to a RAT within 5 days), fixed_direct (samples fixed proportion)
    toggle_fixed_antiviral_coverage = 1, #% of people with access to antivirals OR access to RAT tests -> antiviral
    manager_stochastic_VE_sampling = "uniform" # options: "normal" or "uniform"
  )
  
  RECORD_antiviral_model_simulations_0 = RECORD_antiviral_model_simulations_0 %>% mutate(country = setting, setting_beta = setting_beta)
  
  time.end.AntiviralSimulations=proc.time()[[3]]
  
  time_run = (time.end.AntiviralSimulations - time.start.AntiviralSimulations)/60
  
  row = data.frame(setting = this_setting, 
                   time = time_run,
                   risk_group = this_risk_group_name)
  
  stochastiC_VE_timer = rbind(stochastiC_VE_timer,row)
}
beep()
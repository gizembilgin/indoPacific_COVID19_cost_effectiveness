rootpath = str_replace(getwd(), "GitHub_vaxAllocation","")

source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) antiviral_model_manger.R",sep=""))
source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) antiviral_model_worker.R",sep=""))
source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) stochastic_severe_outcomes_sampling.R",sep=""))
source(paste(getwd(),"/3_antiviral_model/(antiviral)(function) stochastic_severe_outcomes_application.R",sep=""))
copy_function_into_cluster = antiviral_model_worker

stochastiC_VE_timer = data.frame()

for (this_num in c(1,2,3)){
  for (this_setting_beta in c("TLS","PNG_low_beta","FJI","IDN")){ #IDN and FJI don't yet have the updated AntiviralSetup
    
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
      
      LIST_antiviral_start_date = c(as.Date('2023-01-01')
                                    #,as.Date('2023-07-01')
      ), 
      LIST_vax_scenario = c( "all willing adults vaccinated with a primary schedule",                                                                                                                      
                             "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule"  ,
                             #"all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a first booster dose",
                             "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule"                      ,
                             #"all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a first booster dose"                    ,
                             #"all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults"                                                           ,
                             "catchup campaign for high-risk adults: assume booster to high-risk adults who have previously completed their primary schedule but have not recieved a booster"             ,
                             "catchup campaign for all adults: assume booster to all adults who have previously completed their primary schedule but have not recieved a booster"  
      ),
      LIST_antiviral_target_group = list('adults_with_comorbidities', #baseline
                                         'unvaccinated_adults',
                                         #'unvaccinated_adults_AND_adults_with_comorbidities',
                                         'all_adults'
      ),
      LIST_antiviral_type = list("nirmatrelvir_ritonavir"
                                 ,"molunipiravir"
      ), #options:nirmatrelvir_ritonavir,molunipiravir 
      toggle_high_risk_group = "adults_with_comorbidities",
      
      
      RECORD_antiviral_setup = RECORD_antiviral_setup,
      setting = setting,
      
      toggle_number_of_runs = this_num, #DEFAULT 100
      toggle_cluster_number = this_num,
      
      toggle_stochastic_SO = "on", # DEFAULT "on"
      toggle_compare_to_vaccine_effect = "on",
      
      toggle_sensitivity_analysis = list(),
      pathway_to_care = "fixed_RAT", #options: gold_standard (require data on healthcare seeking/access), fixed_RAT (probability of testing positive to a RAT within 5 days), fixed_direct (samples fixed proportion)
      toggle_fixed_antiviral_coverage = 1, #% of people with access to antivirals OR access to RAT tests -> antiviral
      manager_stochastic_VE_sampling = "uniform" # options: "normal" or "uniform"
    )
    
    RECORD_antiviral_model_simulations = RECORD_antiviral_model_simulations_0 %>% mutate(country = setting, setting_beta = setting_beta)
    
    temp_name = ''
    time = Sys.time()
    time = gsub(':','-',time)
    time = paste(temp_name,time,sep='')
    save(RECORD_antiviral_model_simulations, file = paste(rootpath,"x_results/Stochastic_VE_AntiviralRun_",setting_beta,"_",this_risk_group_name,"_",time,".Rdata",sep=''))
    
    time.end.AntiviralSimulations=proc.time()[[3]]
    
    time_run = (time.end.AntiviralSimulations - time.start.AntiviralSimulations)/60
    
    row = data.frame(setting = this_setting, 
                     time = time_run,
                     risk_group = this_risk_group_name)
    
    stochastiC_VE_timer = rbind(stochastiC_VE_timer,row)
    write.csv(stochastiC_VE_timer,file=paste("x_results/stochastic_VE_timer",time,".csv"))
  }
}

beep()




# stochastiC_VE_run = RECORD_antiviral_model_simulations_0 
# stochastiC_VE_run$country = stochastiC_VE_run$setting = paste(this_setting,"_stochastic_VE",sep="")
# 
# load(file = paste(rootpath,"x_results/","AntiviralRun_PNG_low_beta_adults_with_comorbidities_2023-04-08 17-28-44.Rdata",sep="")) #RECORD_antiviral_model_simulations
# RECORD_antiviral_model_simulations$country = RECORD_antiviral_model_simulations$setting_beta = paste(this_setting,"_det_VE",sep="")
# 
# MASTER_RECORD_antiviral_model_simulations = bind_rows(stochastiC_VE_run,
#                                                       RECORD_antiviral_model_simulations)

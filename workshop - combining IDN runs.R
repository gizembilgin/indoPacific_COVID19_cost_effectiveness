workshop = data.frame()

this_risk_group =  "adults_with_comorbidities"
this_setting = "IDN"

list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = paste("AntiviralRun_",this_setting,"_",this_risk_group,"*",sep=""))
list_poss_Rdata = list_poss_Rdata[substr(list_poss_Rdata,1,26) != "Stochastic_VE_AntiviralRun"]
list_poss_Rdata_details = double()
for (j in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste(rootpath,'x_results/',list_poss_Rdata[[j]],sep=''))$mtime)
}

for (i in 1:4){
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)-(i-1)]]
  load(file = paste(rootpath,"x_results/",latest_file,sep=''))
  
  workshop = rbind(workshop,RECORD_antiviral_model_simulations)
  
}

length(unique(workshop$run_ID))

RECORD_antiviral_model_simulations = workshop
save(RECORD_antiviral_model_simulations, file = paste(rootpath,"x_results/AntiviralRun_",setting_beta,"_",this_risk_group,"_",time,".Rdata",sep=''))

rm(list=ls())
master_toggles = list(setting_beta = "PNG_low_beta",
                      risk_group_name = "adults_with_comorbidities")
source(paste(getwd(),"/(antiviral) set up.R",sep=""),local=TRUE)
source(paste(getwd(),"/(antiviral)(run).R",sep=""),local=TRUE)

rm(list=ls())
master_toggles = list(setting_beta = "PNG_low_beta",
                      risk_group_name = "pregnant_women")
source(paste(getwd(),"/(antiviral) set up.R",sep=""),local=TRUE)
source(paste(getwd(),"/(antiviral)(run) special case pregnant women.R",sep=""),local=TRUE)
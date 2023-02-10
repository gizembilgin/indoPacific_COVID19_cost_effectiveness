
### FJI - pregnant women
rm(list=ls())
master_toggles = list(setting_beta = "FJI",
                      risk_group_name = "pregnant_women",
                      TOGGLE_include_second_booster_elig = FALSE)
source(paste(getwd(),"/(antiviral) set up.R",sep=""),local=TRUE)
source(paste(getwd(),"/(antiviral)(run) special case pregnant women .R",sep=""),local=TRUE)


### PNG - antiviral model runs
# setting_beta = "PNG_low_beta"
# source(paste(getwd(),"/(antiviral)(run).R",sep=""),local=TRUE)
# source(paste(getwd(),"/(antiviral)(run) special case pregnant women .R",sep=""),local=TRUE)
setting_beta = "PNG_high_beta"
source(paste(getwd(),"/(antiviral)(run).R",sep=""),local=TRUE)
source(paste(getwd(),"/(antiviral)(run) special case pregnant women .R",sep=""),local=TRUE)


### FJI - with second booster dose
rm(list=ls())
master_toggles = list(setting_beta = "FJI",
                      risk_group_name = "adults_with_comorbidities",
                      TOGGLE_include_second_booster_elig = TRUE)
source(paste(getwd(),"/(antiviral) set up.R",sep=""),local=TRUE)
source(paste(getwd(),"/(antiviral)(run).R",sep=""),local=TRUE)



### MAIN
rm(list=ls())
master_toggles = list(setting_beta = "PNG_low_beta",
                      risk_group_name = "adults_with_comorbidities",
                      TOGGLE_include_second_booster_elig = FALSE)
source(paste(getwd(),"/3_antiviral_model/(antiviral) set up.R",sep=""),local=TRUE)
source(paste(getwd(),"/3_antiviral_model/(antiviral)(run).R",sep=""),local=TRUE)

rm(list=ls())
master_toggles = list(setting_beta = "FJI",
                      risk_group_name = "adults_with_comorbidities",
                      TOGGLE_include_second_booster_elig = FALSE)
source(paste(getwd(),"/3_antiviral_model/(antiviral) set up.R",sep=""),local=TRUE)
source(paste(getwd(),"/3_antiviral_model/(antiviral)(run).R",sep=""),local=TRUE)

rm(list=ls())
master_toggles = list(setting_beta = "PNG_high_beta",
                      risk_group_name = "adults_with_comorbidities",
                      TOGGLE_include_second_booster_elig = FALSE)
source(paste(getwd(),"/3_antiviral_model/(antiviral) set up.R",sep=""),local=TRUE)
source(paste(getwd(),"/3_antiviral_model/(antiviral)(run).R",sep=""),local=TRUE)


### PREGNANT WOMEN
rm(list=ls())
master_toggles = list(setting_beta = "FJI",
                      risk_group_name = "pregnant_women",
                      TOGGLE_include_second_booster_elig = FALSE)
source(paste(getwd(),"/3_antiviral_model/(antiviral) set up.R",sep=""),local=TRUE)
source(paste(getwd(),"/3_antiviral_model/(antiviral)(run) special case pregnant women .R",sep=""),local=TRUE)

rm(list=ls())
master_toggles = list(setting_beta = "PNG_low_beta",
                      risk_group_name = "pregnant_women",
                      TOGGLE_include_second_booster_elig = FALSE)
source(paste(getwd(),"/3_antiviral_model/(antiviral) set up.R",sep=""),local=TRUE)
source(paste(getwd(),"/3_antiviral_model/(antiviral)(run) special case pregnant women .R",sep=""),local=TRUE)




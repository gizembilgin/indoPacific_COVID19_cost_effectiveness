### MAIN
rm(list=ls())
master_toggles = list(setting_beta = "PNG_low_beta",
                      risk_group_name = "adults_with_comorbidities",
                      TOGGLE_include_second_booster_elig = FALSE)
source("03_results_generation/(run) high risk adults.R",local=TRUE)

   
rm(list=ls())
master_toggles = list(setting_beta = "FJI",
                      risk_group_name = "adults_with_comorbidities",
                      TOGGLE_include_second_booster_elig = FALSE)
source("03_results_generation/(run) high risk adults.R",local=TRUE)


rm(list=ls())
master_toggles = list(setting_beta = "TLS",
                      risk_group_name = "adults_with_comorbidities",
                      TOGGLE_include_second_booster_elig = FALSE)
source("03_results_generation/(run) high risk adults.R",local=TRUE)


rm(list=ls())
master_toggles = list(setting_beta = "IDN",
                      risk_group_name = "adults_with_comorbidities",
                      TOGGLE_include_second_booster_elig = FALSE)
source("03_results_generation/(run) high risk adults.R",local=TRUE)


rm(list=ls())
master_toggles = list(setting_beta = "PNG_high_beta",
                      risk_group_name = "adults_with_comorbidities",
                      TOGGLE_include_second_booster_elig = FALSE)
source("03_results_generation/(run) high risk adults.R",local=TRUE)
#_______________________________________________________________________________





### PREGNANT WOMEN
rm(list=ls())
master_toggles = list(setting_beta = "FJI",
                      risk_group_name = "pregnant_women",
                      TOGGLE_include_second_booster_elig = FALSE)
source("03_results_generation/(run) special case pregnant women .R",local=TRUE)


rm(list=ls())
master_toggles = list(setting_beta = "PNG_low_beta",
                      risk_group_name = "pregnant_women",
                      TOGGLE_include_second_booster_elig = FALSE)
source("03_results_generation/(run) special case pregnant women .R",local=TRUE)


rm(list=ls())
master_toggles = list(setting_beta = "TLS",
                      risk_group_name = "pregnant_women",
                      TOGGLE_include_second_booster_elig = FALSE)
source("03_results_generation/(run) special case pregnant women .R",local=TRUE)


rm(list=ls())
master_toggles = list(setting_beta = "IDN",
                      risk_group_name = "pregnant_women",
                      TOGGLE_include_second_booster_elig = FALSE)
source("03_results_generation/(run) special case pregnant women .R",local=TRUE)


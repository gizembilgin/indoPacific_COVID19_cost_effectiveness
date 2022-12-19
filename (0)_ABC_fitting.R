### This script uses Approximate Bayesian Computation (ABC) to fit the model from the first reported case until today's date {Sys.Date()}.
### Today's date is then used as the latest fitted date of the model and used as date_start in modelling scenarios.
### (We re derive today's date using the file creation data of fitted_results)
###
### Dependencies: nil
### Creates: fitted_results_<setting>

#clear the field!
rm(list=ls())


this_setting = "FJI"



### Setup ___________________________________________________________________________________________
#general toggles
fitting = "on"
plotting = "on"
outbreak_timing = "off" #i.e., no new outbreak if =="after" than new VOC after last vaccine delivery date, if == 'during" new VOC introduced one week from now
vax_strategy_toggle = "off" #no additional vax, use real vax data only
vax_risk_strategy_toggle = "off"
sensitivity_analysis_toggles = list()
waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = FALSE #let's save some time, this is not used in the modelling scenarios
waning_toggle_rho_acqusition = TRUE

#setting toggles
setting = this_setting

#initial search range of seed dates
if (setting == "FJI"){
  date_start = as.Date('2021-04-30')
  strain_inital = strain_now = 'WT' 
  
  covid19_waves = data.frame(date = c(as.Date('2021-06-15'),as.Date('2021-12-01'),as.Date('2022-04-01')),
                     strain = c('delta','omicron','omicron'))
}
model_weeks = as.numeric((Sys.Date()+1-date_start)/7)

#plot standard
plot_standard = theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'))

#risk group toggles
risk_group_prioritisation_to_date = NA
risk_group_lower_cov_ratio = NA
risk_group_toggle = "on"

risk_group_name = 'adults_with_comorbidities'
RR_estimate = 1.95
#______________________________________________________________________________________________________________



source(paste(getwd(),"/CommandDeck.R",sep=""))

coeff <- 1/20

ggplot() +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=incidence_log,aes(x=date,y=rolling_average)) + 
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+ 
  plot_standard



### Save ___________________________________________________________________________________________
#columns: fitted_setting, fitted_date, strain, date
load(file = '1_inputs/fit/fitted_covid19_waves.Rdata')
fitted_covid19_waves = fitted_covid19_waves %>% 
  mutate(fitted_setting = this_setting,
         fitted_date = Sys.Date())
fitted_covid19_waves = rbind(fitted_covid19_waves,this_fit_covid19_waves)
save(fitted_covid19_waves, file = '1_inputs/fit/fitted_covid19_waves.Rdata')
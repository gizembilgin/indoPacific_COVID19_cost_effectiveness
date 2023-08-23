


### SETUP ______________________________________________________________________
this_setting = setting = "PNG"
strain_inital = strain_now = 'WT' 
baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-01-01'),
                                    as.Date('2021-07-05')-28, #first sequenced date - 4 weeks
                                    as.Date('2021-12-20')-28),#first sequenced date - 4 weeks
                           strain = c('WT','delta','omicron'))
date_start = covid19_waves$date[1] - 2
model_weeks = as.numeric((as.Date('2022-12-31') - date_start)/7)

#general toggles
fitting = "on"
plotting = "off"; ticket = 0
outbreak_timing = "off" #i.e., no new outbreak if =="after" than new VOC after last vaccine delivery date, if == 'during" new VOC introduced one week from now
vax_strategy_toggle = "off" #no additional vax, use real vax data only
vax_risk_strategy_toggle = "off"
sensitivity_analysis_toggles = list()
waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = FALSE #let's save some time, this is not used in the modelling scenarios
waning_toggle_rho_acqusition = TRUE

#risk group toggles
risk_group_prioritisation_to_date = NA
risk_group_lower_cov_ratio = NA
risk_group_toggle = "on"
risk_group_name = 'adults_with_comorbidities'
RR_estimate = 1.95
#_______________________________________________________________________________



### VISUALISATION OF POSSIBLE RANGE ____________________________________________
workshop_incidence_log_tracker = data.frame()
final_state_tracker = data.frame()

this_shift = 0
#for (this_shift in c(0,-7,-14,-21,+7,+14)){
#for (this_param in c(0.9,1,1.05,1.1,1.2,1.3,1.4,1.5,2)){  
for (this_shift in c(0,-15,-30)){
  for (this_param in  seq(1,2,by=0.1)[!(seq(1,2,by=0.1) %in% unique(workshop_incidence_log_tracker$beta_mod[workshop_incidence_log_tracker$shift == this_shift]))]){  
    fitting_beta = rep(this_param,3)
    
    covid19_waves = baseline_covid19_waves
    covid19_waves$date[1] = covid19_waves$date[1] + this_shift
    date_start = covid19_waves$date[1] - 2
    
    source(paste(getwd(),"/CommandDeck.R",sep=""))
    
    incidence_log = incidence_log %>%
      mutate(beta_mod = this_param,
             shift = this_shift)
    
    final_state = next_state %>%
      mutate(beta_mod = this_param,
             shift = this_shift)
    
    final_state_tracker = rbind(final_state_tracker,final_state)
    workshop_incidence_log_tracker = rbind(workshop_incidence_log_tracker,incidence_log)
  }
}

to_plot = workshop_incidence_log_tracker %>%
  filter(beta_mod %in% c(1,1.2,1.4,1.6,1.8,2) &
           shift>-30 &
           !(shift == -15 & beta_mod %in% c(2.0,1.8,1.6)) &
           !(shift == 0 & beta_mod %in% c(1.4,1.2,1)))
coeff <- 1/250
ggplot() +
  geom_point(data=case_history[case_history$date>min(workshop_incidence_log_tracker$date) & case_history$date<max(workshop_incidence_log_tracker$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(beta_mod))) + 
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+ 
  plot_standard + 
  labs(color='beta modifier') + 
  xlab('') #+ 
  #facet_grid(shift ~ . )

#population-level seroprev
final_state_tracker %>%
  filter((shift == -15 & beta_mod %in% c(1.4,1.2,1)) |
           (shift == 0 & beta_mod %in% c(1.6,1.8,2))) %>%
  group_by(beta_mod,shift) %>%
  filter(class == 'R') %>%
  group_by(beta_mod,shift) %>%
  summarise(pop = sum(pop)) %>%
  mutate(seroprev= pop/sum(pop_setting$pop)) %>%
  select(beta_mod,seroprev)

#age-specific seroprevalence
age_dn = final_state_tracker %>%
  filter((shift == -15 & beta_mod %in% c(1.4,1.2,1)) |
           (shift == 0 & beta_mod %in% c(1.6,1.8,2))) %>%
  group_by(beta_mod,shift) %>%
  filter(class == 'R') %>%
  group_by(beta_mod,shift,age_group) %>%
  summarise(pop = sum(pop)) %>%
  rename(recovered = pop) %>%
  left_join(pop_setting,by='age_group') %>%
  mutate(seroprev= recovered/pop) %>%
  select(beta_mod,age_group,seroprev)
ggplot(age_dn) + geom_point(aes(x=age_group,y=seroprev*100,color=as.factor(beta_mod))) + 
  plot_standard+ 
  labs(color='beta modifier') +
  xlab('age group') +
  ylab('seroprevalence (%)') +
  ylim(0,100) #+
  #facet_grid(shift~.)

PNG_ensemble = list(
  final_state_tracker = final_state_tracker,
  workshop_incidence_log_tracker = workshop_incidence_log_tracker
)
save(PNG_ensemble, file = paste("01_inputs/PNG_ensemble_",Sys.Date(),".Rdata"))
#_______________________________________________________________________________



### CREATE FITTED RESULTS FOR 'LOW' AND 'HIGH' PNG
scenarios = data.frame(name = c("PNG_low_beta","PNG_high_beta"),
                       beta = c(1.4,1.8),
                       shift = c(-15,0))

two_scenarios_plot = data.frame()

for (this_scenario in 1:nrow(scenarios)){
  
  model_weeks = as.numeric((as.Date('2022-12-31') - date_start)/7)
  
  fitting_beta = rep(scenarios$beta[this_scenario],3)
  covid19_waves = baseline_covid19_waves 
  covid19_waves$date[1] = covid19_waves$date[1] + scenarios$shift[this_scenario]
  date_start = covid19_waves$date[1] - 2
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  incidence_log = incidence_log %>% select(date, daily_cases)
  
  fitted_results = list(
    FR_parameters = parameters,
    FR_next_state = next_state,
    FR_incidence_log_tidy = incidence_log_tidy,
    FR_incidence_log = incidence_log,
    FR_covid19_waves = covid19_waves,
    FR_fitting_beta = fitting_beta
  )
  
  incidence_log = incidence_log %>%
    mutate(scenario = scenarios$name[this_scenario])
  two_scenarios_plot = rbind(two_scenarios_plot,incidence_log)
  
  save(fitted_results, file = paste("01_inputs/fit/fitted_results_",scenarios$name[this_scenario],Sys.Date(),".Rdata",sep=""))
}

ggplot() +
  geom_point(data=case_history[case_history$date>min(workshop_incidence_log_tracker$date) & case_history$date<max(workshop_incidence_log_tracker$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=two_scenarios_plot[two_scenarios_plot$date<as.Date('2023-01-01'),],aes(x=date,y=daily_cases,color=as.factor(scenario))) + 
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+ 
  plot_standard + 
  labs(color='beta modifier') + 
  xlab('') 
#_______________________________________________________________________________



### CHECK 2023
ensemble_plot_list = list()
ensemble_plot_log = data.frame()
for (this_scenario in 1:nrow(scenarios)){
  model_weeks = as.numeric(ceiling((as.Date('2024-01-01') - date_start)/7)) 
  
  fitting_beta = rep(scenarios$beta[this_scenario],3)
  covid19_waves = baseline_covid19_waves 
  covid19_waves$date[1] = covid19_waves$date[1] + scenarios$shift[this_scenario]
  date_start = covid19_waves$date[1] - 2
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  ensemble_plot_list[[this_scenario]] <- ggplot() + 
    geom_line(data=incidence_log,aes(x=date,y=rolling_average),na.rm=TRUE) +
    xlab("") + 
    #scale_x_date(date_breaks="3 month", date_labels="%b") +
    ylab("daily cases") +
    plot_standard
  
  incidence_log = incidence_log %>%
    mutate(label = scenarios$name[this_scenario])
  ensemble_plot_log = rbind(ensemble_plot_log,incidence_log)
  
}
ensemble_plot_list

ggplot() + 
  geom_line(data=ensemble_plot_log,aes(x=date,y=rolling_average,color=as.factor(label)),na.rm=TRUE) +
  xlab("") + 
  ylab("daily cases") +
  plot_standard +
  labs(linetype="")
#_______________________________________________________________________________



### CHECK fit with pregnant women
# risk_group_name = 'pregnant_women'
# RR_estimate =  2.4
# 
# for (this_scenario in 1:nrow(scenarios)){
# 
#   model_weeks = as.numeric((as.Date('2022-12-31') - date_start)/7)
# 
#   fitting_beta = rep(scenarios$beta[this_scenario],3)
#   covid19_waves = baseline_covid19_waves 
#   covid19_waves$date[1] = covid19_waves$date[1] + scenarios$shift[this_scenario]
#   date_start = covid19_waves$date[1] - 2
# 
#   source(paste(getwd(),"/CommandDeck.R",sep=""))
# 
#   incidence_log = incidence_log %>% select(date, daily_cases)
# 
#   fitted_results = list(
#     FR_parameters = parameters,
#     FR_next_state = next_state,
#     FR_incidence_log_tidy = incidence_log_tidy,
#     FR_incidence_log = incidence_log,
#     FR_covid19_waves = covid19_waves,
#     FR_fitting_beta = fitting_beta
#   )
# 
#   save(fitted_results, file = paste("01_inputs/fit/fitted_results_pregnant_women_",scenarios$name[this_scenario],Sys.Date(),".Rdata",sep=""))
# }
# 
# load("01_inputs/fit/fitted_results_pregnant_women_PNG_high_beta2023-02-06.Rdata")
# preg_high_beta = fitted_results$FR_incidence_log %>% mutate(risk_group = 'pregnant_women', setting_beta = "high_beta")
# load("01_inputs/fit/fitted_results_pregnant_women_PNG_low_beta2023-02-06.Rdata")
# preg_low_beta = fitted_results$FR_incidence_log  %>% mutate(risk_group = 'pregnant_women', setting_beta = "low_beta")
# 
# load("01_inputs/fit/fitted_results_PNG_high_beta2023-01-19.Rdata")
# comorb_high_beta = fitted_results$FR_incidence_log %>% mutate(risk_group = 'adults_with_comorbidities', setting_beta = "high_beta")
# load("01_inputs/fit/fitted_results_PNG_low_beta2023-01-19.Rdata")
# comorb_low_beta = fitted_results$FR_incidence_log  %>% mutate(risk_group = 'adults_with_comorbidities', setting_beta = "low_beta")
# 
# to_plot = rbind(preg_high_beta,preg_low_beta,comorb_high_beta,comorb_low_beta)
# ggplot(to_plot) + geom_line(aes(x=date,y=daily_cases,color=as.factor(risk_group))) + facet_grid(setting_beta ~ .)
# ggplot(to_plot) + geom_line(aes(x=date,y=daily_cases,color=as.factor(setting_beta))) + facet_grid(risk_group ~ .)
#aligns!
#_______________________________________________________________________________
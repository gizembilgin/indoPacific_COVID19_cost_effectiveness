### SETUP ______________________________________________________________________
strain_inital = strain_now = 'WT' 
model_weeks = as.numeric((Sys.Date()+1-date_start)/7)
covid19_waves = data.frame(date = c(as.Date('2021-01-01'),
                                    as.Date('2021-07-05')-28, #first sequenced date - 4 weeks
                                    as.Date('2021-12-20')-28),#first sequenced date - 4 weeks
                           strain = c('WT','delta','omicron'))
#_______________________________________________________________________________



### VISUALISATION OF POSSIBLE RANGE ____________________________________________
workshop_incidence_log_tracker = data.frame()
final_state_tracker = data.frame()

this_shift = 0
#for (this_shift in c(0,-7,-14,-21,+7,+14)){
for (this_param in c(0.9,1,1.05,1.1,1.2,1.3,1.4,1.5,2)){  
  fitting_beta = rep(this_param,3)
  covid19_waves = baseline_covid19_waves %>%
    mutate(date = date + this_shift)
  
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
#}

to_plot = workshop_incidence_log_tracker
coeff <- 1/150
ggplot() +
  geom_point(data=case_history[case_history$date>min(workshop_incidence_log_tracker$date) & case_history$date<max(workshop_incidence_log_tracker$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(beta_mod))) + 
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+ 
  plot_standard + 
  facet_grid(shift ~ .) 

final_state_tracker %>%
  filter(shift == 0) %>%
  group_by(beta_mod,shift) %>%
  filter(class == 'R') %>%
  group_by(beta_mod,shift) %>%
  summarise(pop = sum(pop)) %>%
  mutate(seroprev= pop/sum(pop_setting$pop))

age_dn = final_state_tracker %>%
  filter(shift == 0) %>%
  group_by(beta_mod,shift) %>%
  filter(class == 'R') %>%
  group_by(beta_mod,shift,age_group) %>%
  summarise(pop = sum(pop)) %>%
  rename(recovered = pop) %>%
  left_join(pop_setting,by='age_group') %>%
  mutate(seroprev= recovered/pop) %>%
  select(beta_mod,age_group,seroprev)
ggplot(age_dn) + geom_point(aes(x=age_group,y=seroprev,color=as.factor(beta_mod)))

PNG_ensemble = list(
  final_state_tracker = final_state_tracker,
  workshop_incidence_log_tracker = workshop_incidence_log_tracker
)
save(PNG_ensemble, file = paste("1_inputs/PNG_ensemble_",Sys.Date(),".Rdata"))
#_______________________________________________________________________________



### CREATE FITTED RESULTS FOR 'LOW' AND 'HIGH' PNG
scenarios = data.frame(name = c("low_PNG_beta","high_PNG_beta"),
                       beta = c(1.05,1.5))

for (this_scenario in 1:nrow(scenarios)){
  
  fitting_beta = rep(scenarios$beta[this_scenario],3)
  covid19_waves = baseline_covid19_waves 
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  fitted_results = list(
    FR_parameters = parameters,
    FR_next_state = next_state,
    FR_incidence_log_tidy = incidence_log_tidy,
    FR_incidence_log = incidence_log
  )
  
  save(fitted_results, file = paste("1_inputs/fitted_results_",scenarios$name[this_scenario],Sys.Date(),".Rdata"))
}
#_______________________________________________________________________________



### CHECK 2023
plot_list = list()
for (this_scenario in 1:nrow(scenarios)){
  model_weeks = as.numeric(ceiling((as.Date('2024-01-01') - date_start)/7)) 
  
  fitting_beta = rep(scenarios$beta[this_scenario],3)
  covid19_waves = baseline_covid19_waves 
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  plot_list[this_scenario] <- ggplot() + 
    geom_line(data=incidence_log,aes(x=date,y=rolling_average),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    ylab("daily cases") +
    plot_standard
  
  
}
plot_list


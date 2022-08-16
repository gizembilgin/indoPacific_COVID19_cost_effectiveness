fitting = "on"

fitted_results = list()
#______________________________________________________________________________________________________________


### Default toggles ___________________________________________________________________________________________
setting = "SLE"

date_start = as.Date('2021-03-31')
strain_inital = strain_now = 'WT' 
seed_date = c(as.Date('2021-04-25'),c(as.Date('2021-11-07'))) #first is seed date for delta, second is omicron

#model_weeks = as.numeric(ceiling((max('2021-12-01')-date_start)/7))
model_weeks = as.numeric((Sys.Date()+1-date_start)/7)
#model_weeks = model_weeks + 52 #too see expected trajectory

plotting = "on"

outbreak_timing = "off"
vax_strategy_toggle = "off"
vax_risk_strategy_toggle = "off"

waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = FALSE
waning_toggle_rho_acqusition = TRUE
rho_severe_disease = "on"

#______________________________________________________________________________________________________________


### Fit without risk group
risk_group_toggle = "off" 
source(paste(getwd(),"/CommandDeck.R",sep=""))
grid.arrange(plot1,plot2,plot3,plot4,plot5, layout_matrix = lay)

fitted_incidence_log_tidy = incidence_log_tidy 
fitted_incidence_log = incidence_log %>% select(date,daily_cases)
fitted_results[[1]] = list(parameters, next_state,fitted_incidence_log_tidy,fitted_incidence_log)

fit_wout_risk_group = date_now
#______________________________________________________________________________________________________________



### Fit with risk group
risk_group_prioritisation_to_date = NA
default_prioritisation_proportion = 0.5
VE_at_risk_suppress = 1 #i.e. do not suppress VE at risk

risk_group_toggle = "on"
risk_group_name_list = c('pregnant_women', 'adults_with_comorbidities')
plot_list = list()

for (fit_ticket in 1:length(risk_group_name_list)){
  risk_group_name = risk_group_name_list[fit_ticket]
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  plot_list[[fit_ticket]] = list(plot1,plot2,plot3,plot4,plot5)
  
  fitted_incidence_log_tidy = incidence_log_tidy 
  fitted_incidence_log = incidence_log %>% select(date,daily_cases)
  fitted_results[[(fit_ticket+1)]] = list(parameters,next_state,fitted_incidence_log_tidy,fitted_incidence_log,risk_group_name)
}
grid.arrange(plot_list[[1]][[1]],plot_list[[1]][[2]],plot_list[[1]][[3]],plot_list[[1]][[4]],plot_list[[1]][[5]], layout_matrix = lay)
grid.arrange(plot_list[[2]][[1]],plot_list[[2]][[2]],plot_list[[2]][[3]],plot_list[[2]][[4]],plot_list[[2]][[5]], layout_matrix = lay)


if (! date_now == fit_wout_risk_group){stop('fit with risk group not equal to fit without!')}
#______________________________________________________________________________________________________________


### Save fitted results
if (! Sys.Date() == date_now-1 ){
  warning('fitted date not equal to current date')
  if (Sys.Date() > date_now){stop('fitted date less than current date, may cause problems with real vaccines not being delivered!')}
}

fitted_max_date = date_now-1  #incidence log always missed in first date
save(fitted_max_date,file = '1_inputs/last_fit_date.Rdata')
save(fitted_results, file = '1_inputs/fitted_results.Rdata')
#______________________________________________________________________________________________________________


### Check seroprevalence estimates
#ASSUMPTION - seroprevalence ~ equivalent to having infection-derived immunity
workshop = next_state_FIT #November 2022
workshop = next_state     #steady state in August 2022
workshop = fitted_next_state

sum(workshop$pop[workshop$class == "R"])/sum(workshop$pop)

workshop %>%
  filter(class == 'R') %>%
  group_by(age_group) %>%
  summarise(pop = sum(pop)) %>%
  rename(recovered = pop) %>%
  left_join(pop_setting,by='age_group') %>%
  mutate(seroprev= recovered/pop)

coeff <- 1/2000

ggplot() +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=incidence_log,aes(x=date,y=rolling_average)) + 
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Model projections",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'))
#______________________________________________________________________________________________________________


###TURN OFF FITTING 
fitting = "off"
#______________________________________________________________________________________________________________

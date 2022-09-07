### This program is intended runs the model in multiple ways to achieve the data required for the RMarkdown report
### Currently four main sections:
# (1) Impact of current program targets
# (2) Varying levels of coverage - w and w/out children
# (3) Varying speed of vaccine roll-out
# (4) 'At risk' group consideration
#________________________________________________________________________________________________________________
time.start.FleetAdmiral=proc.time()[[3]]


### specific togggles
risk_group_toggle = "on"
risk_group_name = "pregnant_women"
risk_group_prioritisation_to_date = NA
default_prioritisation_proportion = 0.5
VE_at_risk_suppress = 1 #i.e. do not suppress VE at risk

#risk_group_lower_cov_ratio = 52/73.4
risk_group_lower_cov_ratio = 70/88
sensitivity_analysis_toggles = list(vax_hesistancy_risk_group = vax_strategy_toggles_CURRENT_TARGET$vax_strategy_max_expected_cov *risk_group_lower_cov_ratio )
#______________________________________



### fit to this sensitivity analysis scenario
fitting = "on"; plotting = "on"

date_start = as.Date('2021-03-31')
strain_inital = strain_now = 'WT' 
seed_date = c(as.Date('2021-04-25'),as.Date('2021-09-01')) #first is seed date for delta, second is omicron
model_weeks = as.numeric((Sys.Date()+1-date_start)/7)

outbreak_timing = "off"
vax_strategy_toggle = "off"
vax_risk_strategy_toggle = "off"

waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = FALSE
waning_toggle_rho_acqusition = TRUE
rho_severe_disease = "on"

source(paste(getwd(),"/CommandDeck.R",sep=""))

SA_vaxHest_fitted_incidence_log_tidy = incidence_log_tidy 
SA_vaxHest_fitted_incidence_log = incidence_log %>% select(date,daily_cases)
SA_vaxHest_fitted_results = list(parameters,next_state,SA_vaxHest_fitted_incidence_log_tidy,SA_vaxHest_fitted_incidence_log,risk_group_name)

grid.arrange(plot1,plot2,plot3,plot4,plot5, layout_matrix = lay)

SA_vaxHest_fitted_max_date = date_now-1
save(SA_vaxHest_fitted_max_date,file = '1_inputs/SA_vaxHest_last_fit_date.Rdata')
save(SA_vaxHest_fitted_results, file = '1_inputs/SA_vaxHest_fitted_results.Rdata')

### Check seroprevalence estimates
workshop = next_state_FIT #November 2022
workshop = SA_vaxHest_fitted_results[[2]]
workshop = fitted_next_state
workshop = next_state

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

fitting = "off"
#________________________________________


#### generic toggles
load(file = '1_inputs/SA_vaxHest_last_fit_date.Rdata')
date_start = SA_vaxHest_fitted_max_date ##latest fit date

strain_inital = strain_now = 'omicron'             #options:'WT','delta','omicron'
model_weeks = 52          # how many weeks should the model run for?

waning_toggle_acqusition = TRUE
waning_toggle_severe_outcome = TRUE
waning_toggle_rho_acqusition = TRUE
rho_severe_disease = "on"

setting = "SLE"
if (setting == "SLE"){
  gov_target = 0.516
  #gov_target = 0.7
  workshop_doses = gov_target - sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose == 1])/100
  workshop_doses = round(workshop_doses * sum(pop))
  
  vax_strategy_toggles_CURRENT_TARGET =
    list(vax_strategy_start_date        = date_start,
         vax_strategy_num_doses         = as.integer(workshop_doses),
         vax_strategy_roll_out_speed    = 11075 ,               # doses delivered per day
         vax_delivery_group             = 'universal',
         vax_age_strategy               = "uniform_no_children",            # options: "oldest", "youngest","50_down","uniform", OTHER?
         vax_dose_strategy              = 1,                    # options: 1,2
         vax_strategy_vaccine_type      = "Johnson & Johnson" ,            # options: "Moderna","Pfizer","AstraZeneca","Johnson & Johnson","Sinopharm","Sinovac"
         vax_strategy_vaccine_interval  = c(90) ,                 # (days) interval between doses
         vax_strategy_max_expected_cov  = 0.88                   # value between 0-1 of age group willing to be vaccinated (vaccine hesitancy est in discussion)
    )
} else { stop ('pick a valid setting!')}
#________________________________________



### run results::(Table 3) At risk group analysis

receipt = 2
source(paste(getwd(),"/(Table 3) high-risk groups.R",sep=""))


#________________________________________________________________________________________________________________


### reset
risk_group_lower_cov_ratio = NA
sensitivity_analysis_toggles = list()
#________________________________________


current_coverage = c(sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose ==1]),
                     sum(vaccination_history_POP$coverage_this_date[vaccination_history_POP$date == max(vaccination_history_POP$date) & vaccination_history_POP$dose ==2])) #COMEBACK - if J&J in use!
if ("Johnson & Johnson" %in% unique(vaccination_history_POP$vaccine_type)){warning('True vaccine coverage MUST consider J&J dose 1')}

save.image(file = paste("x_results/sensitivity_analysis_vax_hest_",Sys.Date(),".Rdata",sep=''))

time.end.FleetAdmiral=proc.time()[[3]]
time.end.FleetAdmiral-time.start.FleetAdmiral 
# 6798.61 = 1.9 hours #COMEBACK - time!


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
model_weeks = model_weeks + 52 #too see expected trajectory

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

#______________________________________________________________________________________________________________



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

date_limit = Sys.Date()

incidence_log_plot = incidence_log %>% filter(date >= date_limit) %>%
  mutate(           cumulative_incidence = cumsum(daily_cases),
                    cumulative_incidence_percentage = 100*cumsum(daily_cases)/sum(pop))

  plot1 <- ggplot() + 
    geom_line(data=incidence_log_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
    geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log_plot$date) &
                                   case_history$date >= date_limit,],
               aes(x=date,y=rolling_average*underascertainment_est),na.rm=TRUE) + 
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    ylab("daily cases") +
    ylim(0,150000)+
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  
  plot2 <- ggplot() + 
    geom_line(data=incidence_log_plot,aes(x=date,y=cumulative_incidence),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    ylab("cumulative cases") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  
  grid.arrange(plot1, plot2, nrow=2)

  #number as % of whole population

  plot1 <- 
    ggplot() + 
    geom_line(data=incidence_log_plot,aes(x=date,y=rolling_average_percentage),na.rm=TRUE) +
    geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log_plot$date) &
                                   case_history$date >= date_limit,],
               aes(x=date,y=rolling_average*5000*underascertainment_est/sum(pop)),na.rm=TRUE) + 
    xlab("") + #ylim(0,1.0)+ 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    ylab("daily cases % whole pop") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black')) 
  
  plot2 <- ggplot() + 
    geom_line(data=Reff_tracker[Reff_tracker$date>=date_limit,],aes(x=date,y=Reff),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    #ylim(0,6) +
    ylab("Reff") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  
  plot3<- ggplot() + 
    geom_line(data=incidence_log_plot,aes(x=date,y=cumulative_incidence_percentage),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    ylab("cumulative cases % whole pop") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  
  plot4 = ggplot(rho_tracker_dataframe[rho_tracker_dataframe$date>=date_limit,]) + geom_line(aes(x=date,y=rho))
  plot5 = ggplot(VE_tracker_dataframe[VE_tracker_dataframe$date>=date_limit,]) + geom_line(aes(x=date,y=VE,color=as.factor(dose)))
  lay <- rbind(c(1,2),c(3,3),c(4,5))
  grid.arrange(plot1,plot2,plot3,plot4,plot5, layout_matrix = lay)

###TURN OFF FITTING 
fitting = "off"
#______________________________________________________________________________________________________________

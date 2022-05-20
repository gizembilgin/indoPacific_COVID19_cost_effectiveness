#Limitations
# - age distribution from Qatar (where care seeking patterns may differ, underlying prevalence of risk factors may vary)
# - age distribution from WT, may have changed with Omicron!
# - age distribution adapted from 10 age year distributions, hence don't include detail on 0-4 (better if distribution)
# COMEBACK - sensitivity analysis by diff age distributions,e.g., greater severity in younger and more uniform in older
#             essential sensitivity analysis since this shape may have a big impact on results!



##### Genuine projection from incidence!
  #(A/D) Join incidence_log_tidy with severe outcome incidence by vax status
  workshop = incidence_log_tidy %>%
    left_join(severe_outcome_this_run) %>%
    mutate(proj = incidence*percentage) #calculate incidence -> severe outcome
  if(!nrow(severe_outcome_this_run[severe_outcome_this_run$date <= max(incidence_log_tidy$date),])*num_risk_groups == nrow(workshop)){stop('something has gone amiss')
  } else if (!nrow(severe_outcome_this_run)*num_risk_groups == nrow(workshop)){warning('more doses left to give in this simulation')}
  #NOTE: number of rows in severe_outcome_this_run may be longer than run of model if more doses to give out than the run of the model
  
  severe_outcome_log_tidy = workshop %>% select(date,risk_group,age_group,dose,vaccine_type,outcome,proj) 
  
  #(B/D) Sum across age groups, doses and vaccination status to get overall severe incidence per day
  workshop = workshop %>%
    group_by(date,outcome) %>%
    summarise(proj=sum(proj))
  
  #(C/D) Add cases as an outcome
  workshop_incid =  incidence_log_unedited[,c('date','daily_cases')] %>%
    mutate(outcome ='cases',proj = daily_cases) %>%
    select(date,outcome,proj)
  workshop = rbind(workshop,workshop_incid)
  
  #(D/D) Calculate cumulative severe outcomes by outcome type
  severe_outcome_log = workshop %>%
    group_by(outcome) %>%
    mutate(proj_cum = cumsum(proj))
  
plot1 <- 
  ggplot() + 
  geom_line(data=severe_outcome_log[severe_outcome_log$outcome != 'cases',],aes(x=date,y=proj,color=as.factor(outcome)),na.rm=TRUE) +
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  #ylim(0,40) +
  ylab("incidence") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

plot2 <- ggplot() + 
  geom_line(data=severe_outcome_log[severe_outcome_log$outcome != 'cases',],aes(x=date,y=proj_cum,color=as.factor(outcome)),na.rm=TRUE) +
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("cumulative incidence") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))
#grid.arrange(plot1, plot2)

#create row for table comparing vaccine strategies
row = severe_outcome_log %>% 
  filter(date == max(severe_outcome_log$date)) %>%
  select(-c(proj,date)) %>%
  pivot_wider(names_from=outcome,
              values_from=proj_cum) 
row







  

  
  
  
  
  


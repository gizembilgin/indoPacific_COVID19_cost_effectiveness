#Limitations
# - age distribution from Qatar (where care seeking patterns may differ, underlying prevalence of risk factors may vary)
# - age distribution from WT, may have changed with Omicron!
# - age distribution adapted from 10 age year distributions, hence don't include detail on 0-4 (better if distribution)
# COMEBACK - sensitivity analysis by diff age distributions,e.g., greater severity in younger and more uniform in older
#             essential sensitivity analysis since this shape may have a big impact on results!



### NOTE: rho against SO current assumed to be CONSTANT!
rho_SO_est = unique(rho_dn$protection[rho_dn$outcome == 'severe_outcome'])
if (length(rho_SO_est)>1){stop('Rho against severe outcome not constant as currently assumed, see (function)_severe_outcome_proj')}

reinfection_protection = exposed_log %>%
  mutate(protection = reinfection_ratio * rho_SO_est) %>%
  select(date,age_group,protection)
#ggplot(reinfection_protection) + geom_point(aes(x=date,y=protection,color=as.factor(age_group)))


##### Genuine projection from incidence!
#(A/D) Join incidence_log_tidy with severe outcome incidence by vax status
workshop = severe_outcome_this_run %>%
  left_join(incidence_log_tidy, by = c("date", "age_group", "risk_group", "vaccine_type", "dose")) %>%
  mutate(proj = incidence*percentage) #calculate incidence -> severe outcome
if(!nrow(severe_outcome_this_run[severe_outcome_this_run$date <= max(incidence_log_tidy$date),]) == nrow(workshop)){stop('something has gone amiss')
} else if (!nrow(severe_outcome_this_run) == nrow(workshop)){warning('more doses left to give in this simulation')}
#NOTE: number of rows in severe_outcome_this_run may be longer than run of model if more doses to give out than the run of the model

if (rho_severe_disease == "on"){
  workshop = workshop %>%
    left_join(reinfection_protection, by = c("date", "age_group")) %>%
    mutate(proj = proj*(1-protection))
  #hosp_incid = subset(hosp_incid, select = -c(protection))
}

severe_outcome_log_tidy = workshop %>% select(date,risk_group,age_group,dose,vaccine_type,outcome,proj) 

#(B/D) Sum across age groups, doses and vaccination status to get overall severe incidence per day
if (exists("age_split_results") == FALSE){ age_split_results = "N"}
if (age_split_results == "N"){
  workshop_2 = workshop %>%
    group_by(date,outcome) %>%
    summarise(proj=sum(proj),.groups = "keep")
  
  #(C/D) Add cases as an outcome
  workshop_incid =  incidence_log_unedited[,c('date','daily_cases')] %>%
    mutate(outcome ='cases',proj = daily_cases) %>%
    select(date,outcome,proj)
  workshop_2 = rbind(workshop_2,workshop_incid)
  
  #(D/D) Calculate cumulative severe outcomes by outcome type
  severe_outcome_log = workshop_2 %>%
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
  grid.arrange(plot1, plot2)
  
  #create row for table comparing vaccine strategies
  row = severe_outcome_log %>% 
    filter(date == max(severe_outcome_log$date)) %>%
    select(-c(proj,date)) %>%
    pivot_wider(names_from=outcome,
                values_from=proj_cum) 
  row
} else if (age_split_results == "Y"){
  workshop_2 = workshop %>%
    mutate(macro_age_group = case_when(
      age_group %in% c('0 to 4','5 to 17') ~ 'children',
      TRUE ~ 'adults'
    )) %>%
    group_by(date,macro_age_group,outcome) %>%
    summarise(proj=sum(proj),.groups = "keep")
  
  #(C/D) Add cases as an outcome
  workshop_incid =  incidence_log_tidy[,c('date','incidence','age_group')] %>%
    mutate(outcome ='cases') %>%
    mutate(macro_age_group = case_when(
      age_group %in% c('0 to 4','5 to 17') ~ 'children',
      TRUE ~ 'adults'
    )) %>%
    group_by(date,macro_age_group,outcome) %>%
    summarise(proj=sum(incidence),.groups = "keep") %>%
    select(date,macro_age_group,outcome,proj)
  
  workshop_2 = rbind(workshop_2,workshop_incid)
  
  #(D/D) Calculate cumulative severe outcomes by outcome type
  severe_outcome_log = workshop_2 %>%
    group_by(outcome,macro_age_group) %>%
    mutate(proj_cum = cumsum(proj))
  
  plot1_children <- 
    ggplot() + 
    geom_line(data=severe_outcome_log[severe_outcome_log$outcome != 'cases' & severe_outcome_log$macro_age_group == 'children',]
              ,aes(x=date,y=proj,color=as.factor(outcome)),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    #ylim(0,40) +
    ylab("incidence") +
    labs(title='children')+
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  plot1_adults <- 
    ggplot() + 
    geom_line(data=severe_outcome_log[severe_outcome_log$outcome != 'cases' & severe_outcome_log$macro_age_group == 'adults',]
              ,aes(x=date,y=proj,color=as.factor(outcome)),na.rm=TRUE) +
    xlab("") + 
    scale_x_date(date_breaks="1 month", date_labels="%b") +
    labs(title='adults')+
    #ylim(0,40) +
    ylab("incidence") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
 
  grid.arrange(plot1_children, plot1_adults)
  
  #create row for table comparing vaccine strategies
  row = severe_outcome_log %>% 
    filter(date == max(severe_outcome_log$date)) %>%
    select(-c(proj,date)) %>%
    pivot_wider(names_from=outcome,
                values_from=proj_cum) 
  row
}





  

  
  
  
  
  


### This function calculates the incidence of severe outcomes in our Study setting

#LIMITATIONS:
# - age distribution from Qatar (where care seeking patterns may differ, underlying prevalence of risk factors may vary)
# - age distribution from WT, may have changed with Omicron!
# - age distribution adapted from 10 age year distributions, hence don't include detail on 0-4

##### setup
if (exists("age_split_results") == FALSE){ age_split_results = "N"}
plot_standard =     theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))
if (outbreak_timing != "off"){
  use_date = date_start +1
} else{
  use_date = min(covid19_waves$date)
}


##### infection-derived immunity against severe outcomes
rho_SO_est = data.frame()
for (i in 1:length(unique(incidence_log$date))){
  
  this_date = unique(incidence_log$date)[i]
  
  this_rho = data.frame(protection = rho_time_step(this_date, outcome = "severe_disease")) %>%
    mutate(date = this_date)
  rho_SO_est = rbind(rho_SO_est,this_rho)
}

reinfection_protection = exposed_log %>%
  left_join(rho_SO_est,by='date') %>%
  mutate(protection = reinfection_ratio * protection) %>%
  select(date,age_group,protection)
#ggplot(reinfection_protection) + geom_point(aes(x=date,y=protection,color=as.factor(age_group)))


##### projection from incidence to severe outcomes
#(A/D) Join incidence_log_tidy with severe outcome incidence by vax status
workshop = severe_outcome_this_run %>%
  left_join(incidence_log_tidy, by = c("date", "age_group", "risk_group", "vaccine_type", "dose")) %>%
  mutate(proj = incidence*percentage) %>%
  left_join(reinfection_protection, by = c("date", "age_group")) %>%
  mutate(proj = case_when(
    is.na(protection) == FALSE ~ proj*(1-protection),
    TRUE ~ proj))
if(!nrow(severe_outcome_this_run[severe_outcome_this_run$date <= max(incidence_log_tidy$date),]) == nrow(workshop)){stop('have lost rows in severe outcome proj')} 

severe_outcome_log_tidy = workshop %>% 
  select(date,risk_group,age_group,dose,vaccine_type,outcome,proj) %>%
  filter(date >= date_start+1)

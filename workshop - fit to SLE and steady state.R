### We have seroprevalence estimate for March 2021 (age-stratified), and
### population-level seroprevalence estimate for November 2021
### July peak with delta-variant, and then January peak with omicron variant and steady state since then

beta_fit = 1
sum(next_state_FIT$pop[next_state_FIT$class == "R"])/sum(next_state_FIT$pop) #0.5421676 instead of 0.405
#ASSUMPTION - seroprevalence ~ equivalent to having infection-derived immunity
# 
# beta_fit = 0.6
# sum(next_state_FIT$pop[next_state_FIT$class == "R"])/sum(next_state_FIT$pop) # 0.6974478 instead of 0.405

next_state_FIT %>%
  filter(class == 'R') %>%
  group_by(age_group) %>%
  summarise(pop = sum(pop)) %>%
  rename(recovered = pop) %>%
  left_join(pop_setting,by='age_group') %>%
  mutate(seroprev= recovered/pop)



sum(next_state$pop[next_state$class == "R"])/sum(next_state$pop) #0.621265



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

#tracking eligibility for booster dose (here J&J 2) for individuals 'at risk'
booster_dose_interval = 30*3

risk_group_considered = list(risk_group_name)


#finding eligible individuals whose primary schedule is complete
eligibility_dataframe = vaccination_history_TRUE %>% #NB: using TRUE because hypoth proj should already be boosted
  mutate(primary_schedule_complete = case_when(
    vaccine_type == "Johnson & Johnson" ~  'Y',
    dose %in% c(2,3) ~ 'Y',
    TRUE ~ 'N'
  ),
  boosted = case_when(
    vaccine_type == "Johnson & Johnson" & dose == 2 ~ 'Y',
    dose == 3 ~ 'Y',
    TRUE ~ 'N'
  )) 


#finding proportion of 'at risk' group not covered by existing rollout
#vax_risk_proportion_booster = # unvaccinated at risk / # ALL at risk
#NB: eligible pop is calculated in (function) vax strategies risk.R



to_plot = eligibility_dataframe %>%
  filter(risk_group %in% risk_group_considered &
          # primary_schedule_complete == "Y" &
           boosted == "N" &
           coverage_this_date > 0) %>%
  mutate(date = date + booster_dose_interval) %>% 
  ungroup() %>% group_by(date) %>% 
  summarise(total_doses = sum(doses_delivered_this_date)) %>%
  ungroup() %>%
  mutate(individuals_eligible = cumsum(total_doses))

hypoth_delivery = vaccination_history_FINAL %>%
  filter(dose == 8) %>%
  ungroup() %>% group_by(date) %>% 
  summarise(total_doses = sum(doses_delivered_this_date)) %>%
  ungroup() %>%
  mutate(cum_doses_delivered = cumsum(total_doses))

ggplot() +
  geom_line(data = to_plot, aes(x=date,y=individuals_eligible)) +
  geom_point(data = hypoth_delivery, aes(x=date,y=cum_doses_delivered)) +
  xlab('') + ylab('number of individuals eligible for a booster dose') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))




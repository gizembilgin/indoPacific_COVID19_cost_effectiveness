
#tracking eligibility for booster dose (here J&J 2) for individuals 'at risk'
booster_dose_interval = 30*3

risk_group_considered = list(risk_group_name)


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
  )) %>%
  filter(risk_group %in% risk_group_considered &
           primary_schedule_complete == "Y" &
           boosted == "N" &
           coverage_this_date > 0) %>%
  mutate(date = date + booster_dose_interval)


to_plot = eligibility_dataframe %>% ungroup() %>% group_by(date) %>% 
  summarise(total_doses = sum(doses_delivered_this_date)) %>%
  ungroup() %>%
  mutate(individuals_eligible = cumsum(total_doses))

ggplot(to_plot) +
  geom_line(aes(x=date,y=individuals_eligible)) +
  xlab('') + ylab('number of individuals eligible for a booster dose') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))
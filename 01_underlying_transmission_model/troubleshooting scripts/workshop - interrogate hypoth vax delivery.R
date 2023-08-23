at_risk_delivery_outline  %>% group_by(date,dose) %>% 
  filter(date %in% list_dates) %>%
  summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>% 
  mutate(label = 'at risk delivery')
generalPublic_leftover_outline  %>% group_by(date,dose) %>% 
  filter(date %in% list_dates) %>%
  summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>% 
  mutate(label = 'general public leftover')

primary_speed_modifier %>%
  filter(date %in% list_dates)
  
booster_speed_modifier %>%
  filter(date %in% list_dates)


vax_delivery_outline  %>% group_by(date,dose) %>% 
  filter(date %in% list_dates) %>%
  summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>% 
  mutate(label = 'at risk delivery')

booster_delivery_outline  %>% group_by(date,dose) %>% 
  filter(date %in% list_dates) %>%
  summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>% 
  mutate(label = 'at risk delivery')



delivered = vax_delivery_outline  %>% group_by(date) %>% 
  summarise(doses_delivered_this_date = sum(doses_delivered)) %>%
  mutate(cumsum = cumsum(doses_delivered_this_date))
join_ready = primary_speed_modifier %>% select(date,doses_avaliable,cumsum) %>%
  rename(expected = cumsum)
delivered = delivered %>% left_join(join_ready)


vax_delivery_outline  %>% group_by(dose,age_group) %>% 
  summarise(doses_delivered = sum(doses_delivered))

vax_delivery_outline  %>% group_by(day) %>% 
  summarise(doses_delivered = sum(doses_delivered)) %>%
  filter(round(doses_delivered) < primary_rollout_speed)

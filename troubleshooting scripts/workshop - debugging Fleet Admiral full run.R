
workshop = vax_delivery_outline %>%
  group_by(day) %>%
  summarise(delivered = sum(doses_delivered))
workshop %>% filter(delivered < 11075)

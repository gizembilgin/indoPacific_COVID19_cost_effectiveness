time.start = proc.time()[[3]]
dummy = VE_time_step(strain,date_now,'any_infection')
time.end=proc.time()[[3]]
time.end-time.start

time.start = proc.time()[[3]]
occupancy = "on"
VE_start = VE_time_step(strain,date_now,'any_infection')
time.end=proc.time()[[3]]
time.end-time.start

time.start = proc.time()[[3]]
occupancy = "off"
VE_now = VE_time_step(strain,date_now,'any_infection')
time.end=proc.time()[[3]]
time.end-time.start





VE_compare = VE_start %>% rename(VE_start = VE) %>%
  left_join(VE_now) %>% rename(VE_now = VE)

check = VE_compare[! VE_compare$VE_start == VE_compare$VE_now, ]




VE_1 = VE %>% rename(VE_1 = VE)
VE_2 = parameters$VE  %>% rename(VE_2 = VE)

VE_together = VE_1 %>% left_join(VE_2)
VE_together  = VE_together %>% filter(!VE_1 == VE_2 )

#We use 11,075 as an expected speed of rollout; but roughly what has the 'true' speed of rollout been in SLE?
#NB: this may be requested by a per reviewer, should it be included in the supplementary material?

#average daily rollout
vaccination_history_POP %>%
  mutate(month = format(date,"%m/%y")) %>%
  group_by(month) %>%
  summarise(doses_delivered = sum(doses_delivered_this_date)) %>%
  mutate(daily_delivery_this_month = doses_delivered/30) %>% 
  summarise(average = mean(daily_delivery_this_month),
                       sd = sd(daily_delivery_this_month),
                       UCI = average - qnorm(0.975) *sd,
                       LCI = average - qnorm(0.023) * sd)
# average     sd     UCI    LCI
#   1   7225. 12439. -17156. 32046.


#visualise roll out by month
to_plot = vaccination_history_POP %>%
  mutate(month = format(date,"%Y-%m-15")) %>%
  group_by(month) %>%
  summarise(doses_delivered = sum(doses_delivered_this_date)) %>%
  mutate(daily_delivery_this_month = doses_delivered/30)
to_plot$month = as.Date(to_plot$month)

ggplot(data = to_plot, aes(month, daily_delivery_this_month)) +
  stat_summary(fun = sum, geom = "bar")  +
  scale_x_date(breaks = "1 month", date_labels="%b") 


#average daily roll out after the vaccine program begun in earnest
vaccination_history_POP %>% 
  filter(date>as.Date('2021-06-30')) %>%
  mutate(month = format(date,"%m/%y")) %>%
  group_by(month) %>%
  summarise(doses_delivered = sum(doses_delivered_this_date)) %>%
  mutate(daily_delivery_this_month = doses_delivered/30) %>%
  summarise(average = mean(daily_delivery_this_month),
            sd = sd(daily_delivery_this_month),
            UCI = average - qnorm(0.975) *sd,
            LCI = average - qnorm(0.023) * sd)
# average     sd     UCI    LCI
#   8880. 13467. -17515. 35752.

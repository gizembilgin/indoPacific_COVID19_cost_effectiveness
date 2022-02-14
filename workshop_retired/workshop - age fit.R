#let's push incidence_log into the same long format as case_data_orig

incidence_long <- incidence_log %>%
  pivot_longer(
    cols = cases_0_4:cases_60_100,
    names_to = c('age'),
    values_to = "n"
  )

incidence_long <- incidence_long[,c('date','age','n')] %>%
  mutate(age = case_when(
    age == 'cases_0_4' ~ '0-4',
    age == 'cases_5_11' ~ '5-11',
    age == 'cases_12_15' ~ '12-15',
    age == 'cases_16_29' ~ '16-29',
    age == 'cases_30_59' ~ '30-59',
    age == 'cases_60_100' ~ '60+'
  )) 

incidence_long <- incidence_long  %>%
  group_by(age,date) %>%
  arrange(age,date) %>%
  mutate(cases_14_days = n + lag(n, default = 0) + lag(n,n=2, default = 0) + lag(n,n=3, default = 0) + lag(n,n=4, default = 0)+ lag(n,n=5, default = 0)+ lag(n,n=6, default = 0) +
           lag(n,n=7, default = 0) + lag(n,n=8, default = 0)+ lag(n,n=9, default = 0)+ lag(n,n=10, default = 0)+ lag(n,n=11, default = 0)+ lag(n,n=12, default = 0)+ lag(n,n=13, default = 0),
         cases_7_days = n + lag(n, default = 0) + lag(n,n=2, default = 0) + lag(n,n=3, default = 0) + lag(n,n=4, default = 0)+ lag(n,n=5, default = 0)+ lag(n,n=6, default = 0)
  )
colnames(incidence_long) <- c('diagdate','agegroup','n_proj','cases_14_days_proj','cases_7_days_proj')

comparison <- case_data_orig %>%
  left_join(incidence_long) %>%
  select(diagdate,agegroup,n,n_proj,cases_14_days,cases_14_days_proj,cases_7_days,cases_7_days_proj)

comparison_rolling <- comparison %>%
  mutate(roll_n = cases_7_days/7,
         roll_n_proj = cases_7_days_proj/7,
         diff=roll_n-roll_n_proj)


ggplot() + 
  geom_line(data=comparison_rolling,aes(x=diagdate,y=roll_n_proj),na.rm=TRUE) +
  geom_point(data=comparison_rolling[case_data_orig$diagdate>date_start,],aes(x=diagdate,y=roll_n),na.rm=TRUE) + 
  xlab("") +
  ylab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))  + 
  facet_grid(~ agegroup)

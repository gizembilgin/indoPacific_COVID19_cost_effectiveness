
#1) CHECKED: plot of coverage by age_group by dose
to_plot = vaccination_history_TRUE %>%
  group_by(date,age_group,dose) %>%
  summarise(doses_delivered_this_date = sum(doses_delivered_this_date))%>%
  left_join(pop_setting, by = c("age_group")) %>%
  group_by(age_group,dose) %>%
  mutate(coverage_this_date = case_when(pop > 0 ~ cumsum(doses_delivered_this_date) /pop,
                                        TRUE ~ 0))
ggplot(to_plot) + geom_point(aes(x=date,y=coverage_this_date,color=as.factor(age_group)))+
  plot_standard +
  facet_grid(dose ~ .)

#subplot by dose
vaccination_history_TRUE %>%
  group_by(age_group,vaccine_type) %>%
  summarise(sum = sum(doses_delivered_this_date))

#2) CHECKED: plot of doses delivered by vaccine type over time
to_plot = vaccination_history_TRUE %>%
  group_by(date,vaccine_type,dose) %>%
  summarise(doses_delivered_this_date = sum(doses_delivered_this_date)) %>%
  group_by(vaccine_type,dose) %>%
  mutate(cum_doses = cumsum(doses_delivered_this_date))
ggplot(to_plot) + geom_line(aes(x=date,y=doses_delivered_this_date,color=as.factor(vaccine_type)))+
  plot_standard +
  facet_grid(dose ~ .)
ggplot(to_plot) + geom_point(aes(x=date,y=cum_doses,color=as.factor(vaccine_type)))+
  plot_standard +
  facet_grid(dose ~ .)
#













####OUTDATED
### Part One: we are trying to project the expected vaccine roll out in FJI
#(A/B) Childhood vaccination program
check = vaccine_coverage_end_history %>% filter(dose == 1 & coverage_this_date>0) %>% arrange(age_group)
# dose vaccine_type age_group risk_group                coverage_this_date
# 1     1 Pfizer       5 to 9    adults_with_comorbidities            0.00579
# 2     1 Pfizer       5 to 9    general_public                       0.00569
# 3     1 Moderna      10 to 17  adults_with_comorbidities            0.208  
# 4     1 Moderna      10 to 17  general_public                       0.209  
# 5     1 Pfizer       10 to 17  adults_with_comorbidities            0.366  
# 6     1 Pfizer       10 to 17  general_public                       0.367  
# 7     1 AstraZeneca  18 to 29  adults_with_comorbidities            1      
# 8     1 AstraZeneca  18 to 29  general_public                       1      
# 9     1 AstraZeneca  30 to 44  adults_with_comorbidities            1      
# 10     1 AstraZeneca  30 to 44  general_public                       1      
# 11     1 AstraZeneca  45 to 59  adults_with_comorbidities            1      
# 12     1 AstraZeneca  45 to 59  general_public                       1      
# 13     1 AstraZeneca  60 to 69  adults_with_comorbidities            1      
# 14     1 AstraZeneca  70 to 100 adults_with_comorbidities            1   

vaccine_coverage_end_history %>% filter(dose == 2) %>% arrange(age_group)
# dose vaccine_type age_group risk_group                coverage_this_date
#   1     2 Pfizer       5 to 9    adults_with_comorbidities            0.00578
# 2     2 Pfizer       5 to 9    general_public                       0.00569
# 3     2 Moderna      10 to 17  adults_with_comorbidities            0.122  
# 4     2 Moderna      10 to 17  general_public                       0.122  
# 5     2 Pfizer       10 to 17  adults_with_comorbidities            0.214  
# 6     2 Pfizer       10 to 17  general_public                       0.214  
# 7     2 AstraZeneca  18 to 29  adults_with_comorbidities            0.967  
# 8     2 AstraZeneca  18 to 29  general_public                       0.967  
# 9     2 AstraZeneca  30 to 44  adults_with_comorbidities            0.967  
# 10     2 AstraZeneca  30 to 44  general_public                       0.967  
# 11     2 AstraZeneca  45 to 59  adults_with_comorbidities            0.967  
# 12     2 AstraZeneca  45 to 59  general_public                       0.967  
# 13     2 AstraZeneca  60 to 69  adults_with_comorbidities            0.968  
# 14     2 AstraZeneca  70 to 100 adults_with_comorbidities            0.968  

#DECISION: all adults willing to be vaccinated with a primary schedule have done so,
#NB the slight drop off in second dose coverage

#Plotting doses delivered over time
workshop = vaccination_history_TRUE %>%
  #filter(age_group %in% c('5 to 9','10 to 17')) %>%
  #filter(vaccine_type == "Pfizer") %>%
  group_by(date,dose,age_group) %>%
  summarise(doses_delivered_this_date = sum(doses_delivered_this_date))
ggplot() + geom_point(data = workshop, aes(x=date,y=doses_delivered_this_date,color=as.factor(age_group)))+ 
  facet_grid(dose ~ .)

#Plotting coverage over time
workshop = vaccination_history_TRUE %>%
  filter(schedule != 'booster') %>%
  #filter(age_group %in% c('5 to 9','10 to 17')) %>%
  group_by(date,dose,age_group,risk_group) %>%
  summarise(coverage_this_date = sum(coverage_this_date))
ggplot() + geom_point(data = workshop, aes(x=date,y=coverage_this_date,color=as.factor(age_group)))+ 
  facet_grid(dose ~ .)


#(B/B) Booster doses
vaccine_coverage_end_history %>% filter(dose == 3) %>% arrange(age_group)
# dose vaccine_type age_group risk_group                coverage_this_date
# 1     3 Moderna      18 to 29  adults_with_comorbidities              0.277
# 2     3 Moderna      18 to 29  general_public                         0.277
# 3     3 Moderna      30 to 44  adults_with_comorbidities              0.277
# 4     3 Moderna      30 to 44  general_public                         0.277
# 5     3 Moderna      45 to 59  adults_with_comorbidities              0.277
# 6     3 Moderna      45 to 59  general_public                         0.277
# 7     3 Moderna      60 to 69  adults_with_comorbidities              0.277
# 8     3 Moderna      70 to 100 adults_with_comorbidities              0.277  

vaccine_coverage_end_history %>% filter(dose == 4) %>% arrange(age_group)
# dose vaccine_type age_group risk_group                coverage_this_date
# 1     4 Moderna      18 to 29  adults_with_comorbidities             0.0468
# 2     4 Moderna      18 to 29  general_public                        0.0468
# 3     4 Moderna      30 to 44  adults_with_comorbidities             0.0468
# 4     4 Moderna      30 to 44  general_public                        0.0468
# 5     4 Moderna      45 to 59  adults_with_comorbidities             0.0468
# 6     4 Moderna      45 to 59  general_public                        0.0468
# 7     4 Moderna      60 to 69  adults_with_comorbidities             0.0468
# 8     4 Moderna      70 to 100 adults_with_comorbidities             0.0468

#Plotting doses delivered over time
workshop = vaccination_history_TRUE %>%
  filter(! age_group %in% c('5 to 9','10 to 17')) %>%
  filter(dose>2) %>%
  group_by(date,dose,age_group) %>%
  summarise(doses_delivered_this_date = sum(doses_delivered_this_date))
ggplot() + geom_point(data = workshop, aes(x=date,y=doses_delivered_this_date,color=as.factor(age_group)))+ 
  facet_grid(dose ~ .)

#Plotting coverage over time
workshop = vaccination_history_TRUE %>%
  filter(! age_group %in% c('5 to 9','10 to 17')) %>%
  filter(schedule == "booster") %>%
  group_by(risk_group,date,age_group) %>%
  summarise(coverage_this_date = sum(coverage_this_date))
ggplot() + geom_point(data = workshop, aes(x=date,y=coverage_this_date,color=as.factor(age_group)))


#______________________________________________________________________________________

### Part Two: have a look at PNG
# workshop  = vaccination_history_TRUE %>%
#   mutate(schedule = case_when(
#     dose > 2 ~ "booster",
#     dose == 2 & vaccine_type == "Johnson & Johnson" ~ "booster",
#     TRUE ~ "primary"
#   )) %>%
#   %>%
#   group_by(risk_group, age_group, vaccine_type, dose) %>%
#   mutate(coverage_this_date = case_when(pop > 0 ~ cumsum(doses_delivered_this_date) /pop,
#                                         TRUE ~ 0))
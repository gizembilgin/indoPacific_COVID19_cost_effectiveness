options(scipen = 100)

prev_state[prev_state$pop<0,] #row = 0
next_state[next_state$pop<0,]
# 19   -173519.037155     S   19  10 to 17    1      Moderna            general_public
# 27   -224552.923760     S   27  10 to 17    1       Pfizer            general_public
# 35  -1597613.376225     S   35  10 to 17    1      Sinovac            general_public
# 147     -743.877745     S   11  10 to 17    1  AstraZeneca adults_with_comorbidities
# 155     -293.664330     S   19  10 to 17    1      Moderna adults_with_comorbidities
# 163     -380.030467     S   27  10 to 17    1       Pfizer adults_with_comorbidities
# 171    -2703.553445     S   35  10 to 17    1      Sinovac adults_with_comorbidities



aggregate(next_state$pop, by=list(next_state$age_group), FUN=sum)
pop
sum(pop); sum(next_state$pop)#CHECKED: pop remains constant


#pick an age to trouble shoot off
this_age = '10 to 17'
dataset = fitted_next_state
dataset = next_state
  
workshop= dataset %>% filter(age_group == this_age)
workshop = aggregate(workshop$pop, by=list(workshop$age_group,workshop$risk_group,workshop$dose,workshop$vaccine_type), FUN=sum)
colnames(workshop) = c('age_group','risk_group','dose','vaccine_type','state')
workshop = workshop %>% left_join(pop_risk_group_dn) %>% mutate(cov = round(state/pop,digits=3)) %>% select(-pop) %>% arrange(risk_group,dose)
print(workshop, row.names = FALSE)
# 10 to 17            general_public    0 unvaccinated 13794517.804862  0.369
# 10 to 17            general_public    1  AstraZeneca  -850296.567653 -0.023
# 10 to 17            general_public    1      Moderna  -328885.095314 -0.009
# 10 to 17            general_public    1       Pfizer  -430989.763679 -0.012
# 10 to 17            general_public    1      Sinovac -3084100.352360 -0.082
# 10 to 17            general_public    2  AstraZeneca  4863919.332036  0.130
# 10 to 17            general_public    2      Moderna  1881308.985561  0.050
# 10 to 17            general_public    2       Pfizer  2465374.462531  0.066
# 10 to 17            general_public    2      Sinovac 17641862.729362  0.471
# 10 to 17            general_public    3  AstraZeneca   270408.892652  0.007
# 10 to 17            general_public    3      Moderna        0.000000  0.000
# 10 to 17            general_public    3       Pfizer  1200435.410859  0.032
# 10 to 17            general_public    3      Sinovac      400.161143  0.000
# 10 to 17            general_public    4  AstraZeneca        0.000000  0.000
# 10 to 17            general_public    4      Moderna        0.000000  0.000
# 10 to 17            general_public    4       Pfizer        0.000000  0.000
# 10 to 17            general_public    4      Sinovac        0.000000  0.000
#same for adults_with_comorbidities

workshop %>% group_by(age_group,risk_group,dose) %>%
  summarise(cov = sum(cov))
# age_group risk_group                 dose    cov
#   1 10 to 17  adults_with_comorbidities     0  0.369
# 2 10 to 17  adults_with_comorbidities     1 -0.126
# 3 10 to 17  adults_with_comorbidities     2  0.717
# 4 10 to 17  adults_with_comorbidities     3  0.039
# 5 10 to 17  adults_with_comorbidities     4  0    
  

vaccination_history_FINAL %>% ungroup() %>%
  filter(date <= date_now & age_group == this_age & risk_group == 'general_public') %>%
  #mutate(dose = case_when(dose == 8 ~ 2, TRUE ~ dose)) %>%
  group_by(age_group,risk_group,dose,vaccine_type,FROM_vaccine_type,FROM_dose) %>%
  summarise(doses_delivered = sum(doses_delivered_this_date)) %>% 
  left_join(pop_risk_group_dn) %>% mutate(cov = round(doses_delivered/pop,digits=3)) %>% select(-pop)
# age_group risk_group      dose vaccine_type FROM_vaccine_type FROM_dose doses_delivered   cov
# <fct>     <chr>          <dbl> <chr>        <chr>                 <dbl>           <dbl> <dbl>
#   1 10 to 17  general_public     1 AstraZeneca  AstraZeneca               0       5914613.  0.158
# 2 10 to 17  general_public     1 Moderna      Moderna                   0       2287705.  0.061
# 3 10 to 17  general_public     1 Pfizer       Pfizer                    0       2997939.  0.08 
# 4 10 to 17  general_public     1 Sinovac      Sinovac                   0      21452821.  0.573
# 5 10 to 17  general_public     2 AstraZeneca  AstraZeneca               1       5130413.  0.137
# 6 10 to 17  general_public     2 Moderna      Moderna                   1       1984386.  0.053
# 7 10 to 17  general_public     2 Pfizer       Pfizer                    1       2600452.  0.069
# 8 10 to 17  general_public     2 Sinovac      Sinovac                   1      18608459.  0.497
# 9 10 to 17  general_public     3 AstraZeneca  AstraZeneca               2         48999.  0.001
# 10 10 to 17  general_public     3 AstraZeneca  Moderna                   2         18952.  0.001
# 11 10 to 17  general_public     3 AstraZeneca  Pfizer                    2         24836.  0.001
# 12 10 to 17  general_public     3 AstraZeneca  Sinovac                   2        177725.  0.005
# 13 10 to 17  general_public     3 Pfizer       AstraZeneca               2        217525.  0.006
# 14 10 to 17  general_public     3 Pfizer       Moderna                   2         84136.  0.002
# 15 10 to 17  general_public     3 Pfizer       Pfizer                    2        110257.  0.003
# 16 10 to 17  general_public     3 Pfizer       Sinovac                   2        788982.  0.021
# 17 10 to 17  general_public     3 Sinovac      AstraZeneca               2            72.5 0    
# 18 10 to 17  general_public     3 Sinovac      Moderna                   2            28.0 0    
# 19 10 to 17  general_public     3 Sinovac      Pfizer                    2            36.8 0    
# 20 10 to 17  general_public     3 Sinovac      Sinovac                   2           263.  0    


vaccination_history_FINAL %>% ungroup() %>%
  filter(date <= date_now & age_group == this_age & risk_group == 'general_public') %>%
  #mutate(dose = case_when(dose == 8 ~ 2, TRUE ~ dose)) %>%
  group_by(age_group,risk_group,dose) %>%
  summarise(doses_delivered = sum(doses_delivered_this_date)) %>% 
  left_join(pop_risk_group_dn) %>% mutate(cov = round(doses_delivered/pop,digits=3)) %>% select(-pop)


workshop = aggregate(next_state$pop, by=list(next_state$age_group,next_state$dose,next_state$risk_group), FUN=sum)
colnames(workshop) = c('age_group','dose','risk_group','doses')
workshop = workshop %>% left_join(pop_risk_group_dn) %>% mutate(cov = doses/pop)
print(workshop, row.names = FALSE)
# age_group dose     risk_group        doses     pop       cov
#age_group dose     risk_group         doses     pop          cov
# 0 to 4    0 general_public 1181771.00000 1181771  1.000000000
# 10 to 17    0 general_public 1553091.00000 1553091  1.000000000
# 18 to 29    0 general_public   -1616.30484 1666627 -0.000969806
# 30 to 44    0 general_public   -1346.96451 1388901 -0.000969806
# 45 to 59    0 general_public    -739.38784  762408 -0.000969806
# 5 to 9    0 general_public 1090331.00000 1090331  1.000000000
# 60 to 69    0 general_public    -239.32484  246776 -0.000969806
# 70 to 100    0 general_public    -137.88411  142177 -0.000969806
# 0 to 4    1 general_public       0.00000 1181771  0.000000000
# 10 to 17    1 general_public       0.00000 1553091  0.000000000
# 18 to 29    1 general_public 1255155.30838 1666627  0.753111109
# 30 to 44    1 general_public 1045996.77251 1388901  0.753111109
# 45 to 59    1 general_public  574177.93445  762408  0.753111109
# 5 to 9    1 general_public       0.00000 1090331  0.000000000
# 60 to 69    1 general_public  185849.74705  246776  0.753111109
# 70 to 100    1 general_public  107075.07815  142177  0.753111109
# 0 to 4    2 general_public       0.00000 1181771  0.000000000
# 10 to 17    2 general_public       0.00000 1553091  0.000000000
# 18 to 29    2 general_public  413087.99646 1666627  0.247858697
# 30 to 44    2 general_public  344251.19200 1388901  0.247858697
# 45 to 59    2 general_public  188969.45340  762408  0.247858697
# 5 to 9    2 general_public       0.00000 1090331  0.000000000
# 60 to 69    2 general_public   61165.57779  246776  0.247858697
# 70 to 100    2 general_public   35239.80595  142177  0.247858697
# 0 to 4    0 pregnant_women       0.00000       0          NaN
# 10 to 17    0 pregnant_women   30221.00000   30221  1.000000000
# 18 to 29    0 pregnant_women    3204.92942  156400  0.020491876
# 30 to 44    0 pregnant_women    1729.47336   84398  0.020491876
# 45 to 59    0 pregnant_women      68.06717    3341  0.020373293
# 5 to 9    0 pregnant_women       0.00000       0          NaN
# 60 to 69    0 pregnant_women       0.00000       0          NaN
# 70 to 100    0 pregnant_women       0.00000       0          NaN
# 0 to 4    1 pregnant_women       0.00000       0          NaN
# 10 to 17    1 pregnant_women       0.00000   30221  0.000000000
# 18 to 29    1 pregnant_women  114429.97038  156400  0.731649427
# 30 to 44    1 pregnant_women   61749.74834   84398  0.731649427
# 45 to 59    1 pregnant_women    2444.83692    3341  0.731768010
# 5 to 9    1 pregnant_women       0.00000       0          NaN
# 60 to 69    1 pregnant_women       0.00000       0          NaN
# 70 to 100    1 pregnant_women       0.00000       0          NaN
# 0 to 4    2 pregnant_women       0.00000       0          NaN
# 10 to 17    2 pregnant_women       0.00000   30221  0.000000000
# 18 to 29    2 pregnant_women   38765.10020  156400  0.247858697
# 30 to 44    2 pregnant_women   20918.77830   84398  0.247858697
# 45 to 59    2 pregnant_women     828.09591    3341  0.247858697
# 5 to 9    2 pregnant_women       0.00000       0          NaN
# 60 to 69    2 pregnant_women       0.00000       0          NaN
# 70 to 100    2 pregnant_women       0.00000       0          NaN



###How do we have >88% coverage?
vaccination_history_FINAL %>% filter(date==max(vaccination_history_FINAL$date) & coverage_this_date>88)

#BINGO!
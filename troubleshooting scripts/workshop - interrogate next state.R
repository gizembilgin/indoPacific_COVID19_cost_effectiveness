options(scipen = 100)

prev_state[prev_state$pop<0,] #row = 0
next_state[next_state$pop<0,]
# pop class temp age_group dose vaccine_type     risk_group
# pop class temp age_group dose      vaccine_type                risk_group
# 93  -41.065975661780392158562     S   21  30 to 44    1 Johnson & Johnson adults_with_comorbidities
# 94  -25.210151035029735311355     S   22  45 to 59    1 Johnson & Johnson adults_with_comorbidities
# 95  -90.192674680754237215297     S   23  60 to 69    1 Johnson & Johnson adults_with_comorbidities
# 96  -76.080692087072080198595     S   24 70 to 100    1 Johnson & Johnson adults_with_comorbidities
# 237  -0.916733173465666739688     E   21  30 to 44    1 Johnson & Johnson adults_with_comorbidities
# 238  -0.421021411209816620591     E   22  45 to 59    1 Johnson & Johnson adults_with_comorbidities
# 239  -1.282288358556517016140     E   23  60 to 69    1 Johnson & Johnson adults_with_comorbidities
# 240  -0.404457441801764794320     E   24 70 to 100    1 Johnson & Johnson adults_with_comorbidities
# 381  -4.117631064775431504188     I   21  30 to 44    1 Johnson & Johnson adults_with_comorbidities
# 382  -1.887509751856510042956     I   22  45 to 59    1 Johnson & Johnson adults_with_comorbidities
# 383  -5.740969119742308279797     I   23  60 to 69    1 Johnson & Johnson adults_with_comorbidities
# 384  -1.801921541664784909287     I   24 70 to 100    1 Johnson & Johnson adults_with_comorbidities
# 525 -62.095187864843595093589     R   21  30 to 44    1 Johnson & Johnson adults_with_comorbidities
# 526 -28.715895210979560658870     R   22  45 to 59    1 Johnson & Johnson adults_with_comorbidities
# 527 -87.709298371528404913988     R   23  60 to 69    1 Johnson & Johnson adults_with_comorbidities
# 528 -28.255358100616994931897     R   24 70 to 100    1 Johnson & Johnson adults_with_comorbidities


aggregate(next_state$pop, by=list(next_state$age_group), FUN=sum)
pop
sum(pop); sum(next_state$pop)#CHECKED: pop remains constant


#pick an age to trouble shoot off
this_age = '18 to 29'
dataset = fitted_next_state
dataset = next_state
  
workshop= dataset %>% filter(age_group == this_age)
workshop = aggregate(workshop$pop, by=list(workshop$age_group,workshop$risk_group,workshop$dose,workshop$vaccine_type), FUN=sum)
colnames(workshop) = c('age_group','risk_group','dose','vaccine_type','state')
workshop = workshop %>% left_join(pop_risk_group_dn) %>% mutate(cov = round(state/pop,digits=3)) %>% select(-pop) %>% arrange(risk_group,dose)
print(workshop, row.names = FALSE)
# age_group     risk_group dose      vaccine_type          state   cov
# 30 to 44 general_public    0      unvaccinated  129925.351195 0.094
# 30 to 44 general_public    1       AstraZeneca    -225.575229 0.000
# 30 to 44 general_public    1 Johnson & Johnson   43814.495992 0.032
# 30 to 44 general_public    1            Pfizer    -262.914138 0.000
# 30 to 44 general_public    1         Sinopharm     -75.881576 0.000
# 30 to 44 general_public    2       AstraZeneca    1088.240442 0.001
# 30 to 44 general_public    2 Johnson & Johnson 1213002.834362 0.873
# 30 to 44 general_public    2            Pfizer    1268.374186 0.001
# 30 to 44 general_public    2         Sinopharm     366.074766 0.000
# 30 to 44 pregnant_women    0      unvaccinated    7895.047804 0.094
# 30 to 44 pregnant_women    1       AstraZeneca     -13.707311 0.000
# 30 to 44 pregnant_women    1 Johnson & Johnson    2662.432983 0.032
# 30 to 44 pregnant_women    1            Pfizer     -15.976248 0.000
# 30 to 44 pregnant_women    1         Sinopharm      -4.611022 0.000
# 30 to 44 pregnant_women    2       AstraZeneca      66.128051 0.001
# 30 to 44 pregnant_women    2 Johnson & Johnson   73709.366769 0.873
# 30 to 44 pregnant_women    2            Pfizer      77.074064 0.001
# 30 to 44 pregnant_women    2         Sinopharm      22.244910 0.000

workshop %>% group_by(age_group,risk_group,dose) %>%
  summarise(cov = sum(cov))
# age_group risk_group      dose   cov
# <chr>     <chr>          <dbl> <dbl>
#   1 30 to 44  general_public     0 0.094
# 2 30 to 44  general_public     1 0.032
# 3 30 to 44  general_public     2 0.875
# 4 30 to 44  pregnant_women     0 0.094
# 5 30 to 44  pregnant_women     1 0.032
# 6 30 to 44  pregnant_women     2 0.875

vaccination_history_FINAL %>% ungroup() %>%
  filter(date <= date_now & age_group == this_age & risk_group == 'general_public') %>%
  #mutate(dose = case_when(dose == 8 ~ 2, TRUE ~ dose)) %>%
  group_by(age_group,risk_group,dose,vaccine_type,FROM_vaccine_type,FROM_dose) %>%
  summarise(doses_delivered = sum(doses_delivered_this_date)) %>% 
  left_join(pop_risk_group_dn) %>% mutate(cov = round(doses_delivered/pop,digits=3)) %>% select(-pop)
# age_group risk_group      dose vaccine_type      FROM_vaccine_type FROM_dose doses_delivered   cov
# 1 30 to 44  general_public     1 AstraZeneca       NA                       NA         266434. 0.192
# 2 30 to 44  general_public     1 Johnson & Johnson NA                       NA         555636. 0.4  
# 3 30 to 44  general_public     1 Pfizer            NA                       NA         310537. 0.224
# 4 30 to 44  general_public     1 Sinopharm         NA                       NA          89626. 0.065
# 5 30 to 44  general_public     2 AstraZeneca       NA                       NA         144104. 0.104
# 6 30 to 44  general_public     2 Johnson & Johnson NA                       NA              0  0    
# 7 30 to 44  general_public     2 Pfizer            NA                       NA         167957. 0.121
# 8 30 to 44  general_public     2 Sinopharm         NA                       NA          48475. 0.035
# 9 30 to 44  general_public     8 Johnson & Johnson AstraZeneca               1         122331. 0.088
# 10 30 to 44  general_public     8 Johnson & Johnson AstraZeneca               2         144104. 0.104
# 11 30 to 44  general_public     8 Johnson & Johnson Johnson & Johnson         1         555636. 0.4  
# 12 30 to 44  general_public     8 Johnson & Johnson Johnson & Johnson         2              0  0    
# 13 30 to 44  general_public     8 Johnson & Johnson Pfizer                    1         142580. 0.103
# 14 30 to 44  general_public     8 Johnson & Johnson Pfizer                    2         167957. 0.121
# 15 30 to 44  general_public     8 Johnson & Johnson Sinopharm                 1          41151. 0.03 
# 16 30 to 44  general_public     8 Johnson & Johnson Sinopharm                 2          48475. 0.035



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
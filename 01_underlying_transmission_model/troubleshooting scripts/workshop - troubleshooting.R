options(scipen = 100)

prev_state[prev_state$pop<0,]
next_state[next_state$pop<0,]
# pop class temp age_group dose vaccine_type     risk_group
# pop class temp age_group dose vaccine_type     risk_group
# 84  -120.3480121     S   28  18 to 29    1    Sinopharm pregnant_women
# 85   -62.8294668     S   29  30 to 44    1    Sinopharm pregnant_women
# 86    -3.0478810     S   30  45 to 59    1    Sinopharm pregnant_women
# 196   -6.1241263     E   28  18 to 29    1    Sinopharm pregnant_women
# 197   -3.4020429     E   29  30 to 44    1    Sinopharm pregnant_women
# 198   -0.1124807     E   30  45 to 59    1    Sinopharm pregnant_women
# 308  -28.0782670     I   28  18 to 29    1    Sinopharm pregnant_women
# 309  -15.5985491     I   29  30 to 44    1    Sinopharm pregnant_women
# 310   -0.5165299     I   30  45 to 59    1    Sinopharm pregnant_women
# 420 -318.6888858     R   28  18 to 29    1    Sinopharm pregnant_women
# 421 -173.5436605     R   29  30 to 44    1    Sinopharm pregnant_women
# 422   -6.4323953     R   30  45 to 59    1    Sinopharm pregnant_women

date_now #"2022-10-09"

#NB: this occurs after delivery of additional booster doses ONLY


### CHECK THAT INITAL VACCINE COVERAGE ALIGNS
fitted_next_state[fitted_next_state$pop<0,] #nrow = 0
workshop = fitted_next_state %>% filter(risk_group == risk_group_name & vaccine_type == 'Sinopharm')
workshop = aggregate(workshop$pop, by=list(workshop$age_group,workshop$risk_group,workshop$dose,workshop$vaccine_type), FUN=sum)
colnames(workshop) = c('age_group','risk_group','dose','vaccine_type','state')
workshop = workshop %>% left_join(pop_risk_group_dn) %>% mutate(cov = state/pop) %>% select(-pop)
print(workshop, row.names = FALSE)
# age_group     risk_group dose vaccine_type      state        cov
# 0 to 4 pregnant_women    1    Sinopharm     0.0000        NaN
# 5 to 9 pregnant_women    1    Sinopharm     0.0000        NaN
# 10 to 17 pregnant_women    1    Sinopharm     0.0000 0.00000000
# 18 to 29 pregnant_women    1    Sinopharm 15084.7923 0.09645008
# 30 to 44 pregnant_women    1    Sinopharm  8140.1937 0.09645008
# 45 to 59 pregnant_women    1    Sinopharm   322.2397 0.09645008
# 60 to 69 pregnant_women    1    Sinopharm     0.0000        NaN
# 70 to 100 pregnant_women    1    Sinopharm     0.0000        NaN
# 0 to 4 pregnant_women    2    Sinopharm     0.0000        NaN
# 5 to 9 pregnant_women    2    Sinopharm     0.0000        NaN
# 10 to 17 pregnant_women    2    Sinopharm     0.0000 0.00000000
# 18 to 29 pregnant_women    2    Sinopharm 39890.7796 0.25505614
# 30 to 44 pregnant_women    2    Sinopharm 21526.2277 0.25505614
# 45 to 59 pregnant_women    2    Sinopharm   852.1425 0.25505614
# 60 to 69 pregnant_women    2    Sinopharm     0.0000        NaN
# 70 to 100 pregnant_women    2    Sinopharm     0.0000        NaN

vaccine_coverage %>% filter(risk_group == risk_group_name & vaccine_type == 'Sinopharm')
# risk_group      dose vaccine_type age_group   cov
# <chr>          <int> <chr>        <chr>     <dbl>
#   1 pregnant_women     1 Sinopharm    0 to 4    0    
# 2 pregnant_women     1 Sinopharm    10 to 17  0    
# 3 pregnant_women     1 Sinopharm    18 to 29  0.352
# 4 pregnant_women     1 Sinopharm    30 to 44  0.352
# 5 pregnant_women     1 Sinopharm    45 to 59  0.352
# 6 pregnant_women     1 Sinopharm    5 to 9    0    
# 7 pregnant_women     1 Sinopharm    60 to 69  0    
# 8 pregnant_women     1 Sinopharm    70 to 100 0    
# 9 pregnant_women     2 Sinopharm    0 to 4    0    
# 10 pregnant_women     2 Sinopharm    10 to 17  0    
# 11 pregnant_women     2 Sinopharm    18 to 29  0.255
# 12 pregnant_women     2 Sinopharm    30 to 44  0.255
# 13 pregnant_women     2 Sinopharm    45 to 59  0.255
# 14 pregnant_women     2 Sinopharm    5 to 9    0    
# 15 pregnant_women     2 Sinopharm    60 to 69  0    
# 16 pregnant_women     2 Sinopharm    70 to 100 0    
# > 0.352 - 0.255
# [1] 0.097
#______________________________________________________


### check vaccine coverage at date_now
workshop = next_state %>% filter(risk_group == risk_group_name & vaccine_type == 'Sinopharm')
workshop = aggregate(workshop$pop, by=list(workshop$age_group,workshop$risk_group,workshop$dose,workshop$vaccine_type), FUN=sum)
colnames(workshop) = c('age_group','risk_group','dose','vaccine_type','state')
workshop = workshop %>% left_join(pop_risk_group_dn) %>% mutate(cov = state/pop) %>% select(-pop)
print(workshop, row.names = FALSE)
# age_group     risk_group dose vaccine_type      state          cov
# 0 to 4 pregnant_women    1    Sinopharm    0.00000          NaN
# 10 to 17 pregnant_women    1    Sinopharm    0.00000  0.000000000
# 18 to 29 pregnant_women    1    Sinopharm -473.23929 -0.003025827
# 30 to 44 pregnant_women    1    Sinopharm -255.37372 -0.003025827
# 45 to 59 pregnant_women    1    Sinopharm  -10.10929 -0.003025827
# 5 to 9 pregnant_women    1    Sinopharm    0.00000          NaN
# 60 to 69 pregnant_women    1    Sinopharm    0.00000          NaN
# 70 to 100 pregnant_women    1    Sinopharm    0.00000          NaN
# 0 to 4 pregnant_women    2    Sinopharm    0.00000          NaN
# 10 to 17 pregnant_women    2    Sinopharm    0.00000  0.000000000
# 18 to 29 pregnant_women    2    Sinopharm    0.00000  0.000000000
# 30 to 44 pregnant_women    2    Sinopharm    0.00000  0.000000000
# 45 to 59 pregnant_women    2    Sinopharm    0.00000  0.000000000
# 5 to 9 pregnant_women    2    Sinopharm    0.00000          NaN
# 60 to 69 pregnant_women    2    Sinopharm    0.00000          NaN
# 70 to 100 pregnant_women    2    Sinopharm    0.00000          NaN


workshop = next_state %>% filter(risk_group == risk_group_name)
workshop = aggregate(workshop$pop, by=list(workshop$age_group,workshop$risk_group,workshop$dose,workshop$vaccine_type), FUN=sum)
colnames(workshop) = c('age_group','risk_group','dose','vaccine_type','state')
workshop = workshop %>% left_join(pop_risk_group_dn) %>% mutate(cov = state/pop) %>% select(-pop)
print(workshop, row.names = FALSE)
# age_group     risk_group dose vaccine_type      state          cov
# 0 to 4 pregnant_women    1       AstraZeneca     0.00000          NaN
# 10 to 17 pregnant_women    1       AstraZeneca     0.00000  0.000000000
# 18 to 29 pregnant_women    1       AstraZeneca  6470.48968  0.041371417
# 30 to 44 pregnant_women    1       AstraZeneca  3491.66488  0.041371417
# 45 to 59 pregnant_women    1       AstraZeneca   138.22191  0.041371417
# 5 to 9 pregnant_women    1       AstraZeneca     0.00000          NaN
# 60 to 69 pregnant_women    1       AstraZeneca     0.00000          NaN
# 70 to 100 pregnant_women    1       AstraZeneca     0.00000          NaN
# 0 to 4 pregnant_women    2       AstraZeneca     0.00000          NaN
# 10 to 17 pregnant_women    2       AstraZeneca     0.00000  0.000000000
# 18 to 29 pregnant_women    2       AstraZeneca 19710.10873  0.126023713
# 30 to 44 pregnant_women    2       AstraZeneca 10636.14934  0.126023713
# 45 to 59 pregnant_women    2       AstraZeneca   421.04523  0.126023713
# 5 to 9 pregnant_women    2       AstraZeneca     0.00000          NaN
# 60 to 69 pregnant_women    2       AstraZeneca     0.00000          NaN
# 70 to 100 pregnant_women    2       AstraZeneca     0.00000          NaN
# 0 to 4 pregnant_women    1 Johnson & Johnson     0.00000          NaN
# 10 to 17 pregnant_women    1 Johnson & Johnson     0.00000  0.000000000
# 18 to 29 pregnant_women    1 Johnson & Johnson 20960.65481  0.134019532
# 30 to 44 pregnant_women    1 Johnson & Johnson 11310.98046  0.134019532
# 45 to 59 pregnant_women    1 Johnson & Johnson   447.75926  0.134019532
# 5 to 9 pregnant_women    1 Johnson & Johnson     0.00000          NaN
# 60 to 69 pregnant_women    1 Johnson & Johnson     0.00000          NaN
# 70 to 100 pregnant_women    1 Johnson & Johnson     0.00000          NaN
# 0 to 4 pregnant_women    2 Johnson & Johnson     0.00000          NaN
# 10 to 17 pregnant_women    2 Johnson & Johnson     0.00000  0.000000000
# 18 to 29 pregnant_women    2 Johnson & Johnson 60503.95297  0.386853919
# 30 to 44 pregnant_women    2 Johnson & Johnson 32649.69707  0.386853919
# 45 to 59 pregnant_women    2 Johnson & Johnson  1292.47894  0.386853919
# 5 to 9 pregnant_women    2 Johnson & Johnson     0.00000          NaN
# 60 to 69 pregnant_women    2 Johnson & Johnson     0.00000          NaN
# 70 to 100 pregnant_women    2 Johnson & Johnson     0.00000          NaN
# 0 to 4 pregnant_women    1         Sinopharm     0.00000          NaN
# 10 to 17 pregnant_women    1         Sinopharm     0.00000  0.000000000
# 18 to 29 pregnant_women    1         Sinopharm  -473.23929 -0.003025827
# 30 to 44 pregnant_women    1         Sinopharm  -255.37372 -0.003025827
# 45 to 59 pregnant_women    1         Sinopharm   -10.10929 -0.003025827
# 5 to 9 pregnant_women    1         Sinopharm     0.00000          NaN
# 60 to 69 pregnant_women    1         Sinopharm     0.00000          NaN
# 70 to 100 pregnant_women    1         Sinopharm     0.00000          NaN
# 0 to 4 pregnant_women    2         Sinopharm     0.00000          NaN
# 10 to 17 pregnant_women    2         Sinopharm     0.00000  0.000000000
# 18 to 29 pregnant_women    2         Sinopharm     0.00000  0.000000000
# 30 to 44 pregnant_women    2         Sinopharm     0.00000  0.000000000
# 45 to 59 pregnant_women    2         Sinopharm     0.00000  0.000000000
# 5 to 9 pregnant_women    2         Sinopharm     0.00000          NaN
# 60 to 69 pregnant_women    2         Sinopharm     0.00000          NaN
# 70 to 100 pregnant_women    2         Sinopharm     0.00000          NaN
# 0 to 4 pregnant_women    0      unvaccinated     0.00000          NaN
# 10 to 17 pregnant_women    0      unvaccinated 30221.00000  1.000000000
# 18 to 29 pregnant_women    0      unvaccinated 49228.03311  0.314757245
# 30 to 44 pregnant_women    0      unvaccinated 26564.88196  0.314757245
# 45 to 59 pregnant_women    0      unvaccinated  1051.60396  0.314757245
# 5 to 9 pregnant_women    0      unvaccinated     0.00000          NaN
# 60 to 69 pregnant_women    0      unvaccinated     0.00000          NaN
# 70 to 100 pregnant_women    0      unvaccinated     0.00000          NaN


workshop = vaccination_history_TRUE %>% filter(risk_group == risk_group_name & vaccine_type == 'Sinopharm')
workshop = aggregate(workshop$doses_delivered_this_date, by=list(workshop$age_group,workshop$risk_group,workshop$dose,workshop$vaccine_type), FUN=sum)
colnames(workshop) = c('age_group','risk_group','dose','vaccine_type','state')
workshop = workshop %>% left_join(pop_risk_group_dn) %>% mutate(cov = state/pop) %>% select(-pop)
print(workshop, row.names = FALSE)
# age_group     risk_group dose vaccine_type      state       cov
# 0 to 4 pregnant_women    1    Sinopharm     0.0000       NaN
# 10 to 17 pregnant_women    1    Sinopharm     0.0000 0.0000000
# 18 to 29 pregnant_women    1    Sinopharm 59938.5262 0.3832387
# 30 to 44 pregnant_women    1    Sinopharm 32344.5763 0.3832387
# 45 to 59 pregnant_women    1    Sinopharm  1280.4004 0.3832387
# 5 to 9 pregnant_women    1    Sinopharm     0.0000       NaN
# 60 to 69 pregnant_women    1    Sinopharm     0.0000       NaN
# 70 to 100 pregnant_women    1    Sinopharm     0.0000       NaN
# 0 to 4 pregnant_women    2    Sinopharm     0.0000       NaN
# 10 to 17 pregnant_women    2    Sinopharm     0.0000 0.0000000
# 18 to 29 pregnant_women    2    Sinopharm 45124.7881 0.2885217
# 30 to 44 pregnant_women    2    Sinopharm 24350.6513 0.2885217
# 45 to 59 pregnant_women    2    Sinopharm   963.9509 0.2885217
# 5 to 9 pregnant_women    2    Sinopharm     0.0000       NaN
# 60 to 69 pregnant_women    2    Sinopharm     0.0000       NaN
# 70 to 100 pregnant_women    2    Sinopharm     0.0000       NaN
#aligns EXACTLY with existing coveraged used in selecting those for booster dose

# max(vaccination_history_TRUE$date[vaccination_history_TRUE$doses_delivered_this_date>0])
# [1] "2022-08-11"


booster = at_risk_delivery_outline %>% filter(dose == 8)
aggregate(booster$doses_delivered_this_date,by=list(booster$dose,booster$risk_group,booster$FROM_vaccine_type,booster$FROM_dose),FUN=sum)
# 1       8 pregnant_women AstraZeneca       1 15955.60
# 2       8 pregnant_women   Sinopharm       1 23124.11
# 3       8 pregnant_women AstraZeneca       2 48603.21
# 4       8 pregnant_women   Sinopharm       2 70439.39

workshop = vaccination_history_TRUE %>% filter(risk_group == risk_group_name & vaccine_type == 'Sinopharm')
workshop = aggregate(workshop$doses_delivered_this_date, by=list(workshop$risk_group,workshop$dose,workshop$vaccine_type), FUN=sum)
colnames(workshop) = c('risk_group','dose','vaccine_type','state')
print(workshop, row.names = FALSE)
# pregnant_women    1    Sinopharm 93563.50
# pregnant_women    2    Sinopharm 70439.39
# 93563.50-70439.39
# = 23124.11
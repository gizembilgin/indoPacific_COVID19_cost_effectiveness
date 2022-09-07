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

aggregate(next_state$pop, by=list(next_state$age_group), FUN=sum)
pop
#CHECKED: pop remains constant


#pick an age to trouble shoot off
this_age = '30 to 44'
dataset = fitted_next_state
  
workshop= dataset %>% filter(age_group == this_age)
workshop = aggregate(workshop$pop, by=list(workshop$age_group,workshop$risk_group,workshop$dose,workshop$vaccine_type), FUN=sum)
colnames(workshop) = c('age_group','risk_group','dose','vaccine_type','state')
workshop = workshop %>% left_join(pop_risk_group_dn) %>% mutate(cov = state/pop) %>% select(-pop)
print(workshop, row.names = FALSE)
# age_group     risk_group dose vaccine_type      state        cov
# 30 to 44 general_public    1  AstraZeneca  92246.194 0.06641668
# 30 to 44 pregnant_women    1  AstraZeneca   3971.153 0.04705269
# 30 to 44 general_public    2  AstraZeneca 280996.124 0.20231544
# 30 to 44 pregnant_women    2  AstraZeneca  12096.744 0.14332974
# 30 to 44 general_public    1    Sinopharm 133690.467 0.09625630
# 30 to 44 pregnant_women    1    Sinopharm   5755.308 0.06819247
# 30 to 44 general_public    2    Sinopharm 407240.493 0.29321060
# 30 to 44 pregnant_women    2    Sinopharm  17531.501 0.20772413

# age_group     risk_group dose      vaccine_type      state        cov
# 30 to 44 general_public    1       AstraZeneca  60583.287 0.04361959
# 30 to 44 pregnant_women    1       AstraZeneca   3681.406 0.04361959
# 30 to 44 general_public    2       AstraZeneca 276502.519 0.19908008
# 30 to 44 pregnant_women    2       AstraZeneca  16801.960 0.19908008
# 30 to 44 general_public    1 Johnson & Johnson      0.000 0.00000000
# 30 to 44 pregnant_women    1 Johnson & Johnson      0.000 0.00000000
# 30 to 44 general_public    2 Johnson & Johnson      0.000 0.00000000
# 30 to 44 pregnant_women    2 Johnson & Johnson      0.000 0.00000000
# 30 to 44 general_public    1         Sinopharm  87801.956 0.06321686
# 30 to 44 pregnant_women    1         Sinopharm   5335.376 0.06321686
# 30 to 44 general_public    2         Sinopharm 400728.026 0.28852166
# 30 to 44 pregnant_women    2         Sinopharm  24350.651 0.28852166

#expected up to this point
workshop = vaccination_history_FINAL %>% filter(risk_group == risk_group_name & age_group == this_age & date<=date_now)




workshop = aggregate(next_state$pop, by=list(next_state$age_group,next_state$dose,next_state$risk_group), FUN=sum)
colnames(workshop) = c('age_group','dose','risk_group','doses')
workshop = workshop %>% left_join(pop_risk_group_dn) %>% mutate(cov = doses/pop)
print(workshop, row.names = FALSE)
# age_group dose     risk_group        doses     pop       cov
# 0 to 4    0 general_public 1181771.0000 1181771 1.0000000
# 10 to 17    0 general_public 1553091.0000 1553091 1.0000000
# 18 to 29    0 general_public  584564.6644 1666627 0.3507471
# 30 to 44    0 general_public  487153.0624 1388901 0.3507471
# 45 to 59    0 general_public  267412.4304  762408 0.3507471
# 5 to 9    0 general_public 1090331.0000 1090331 1.0000000
# 60 to 69    0 general_public   86555.9778  246776 0.3507471
# 70 to 100    0 general_public   49868.1770  142177 0.3507471
# 0 to 4    1 general_public       0.0000 1181771 0.0000000
# 10 to 17    1 general_public       0.0000 1553091 0.0000000
# 18 to 29    1 general_public  325186.6613 1666627 0.1951166
# 30 to 44    1 general_public  270997.6971 1388901 0.1951166
# 45 to 59    1 general_public  148758.4877  762408 0.1951166
# 5 to 9    1 general_public       0.0000 1090331 0.0000000
# 60 to 69    1 general_public   48150.1041  246776 0.1951166
# 70 to 100    1 general_public   27741.0986  142177 0.1951166
# 0 to 4    2 general_public       0.0000 1181771 0.0000000
# 10 to 17    2 general_public       0.0000 1553091 0.0000000
# 18 to 29    2 general_public  756875.6743 1666627 0.4541362
# 30 to 44    2 general_public  630750.2404 1388901 0.4541362
# 45 to 59    2 general_public  346237.0819  762408 0.4541362
# 5 to 9    2 general_public       0.0000 1090331 0.0000000
# 60 to 69    2 general_public  112069.9181  246776 0.4541362
# 70 to 100    2 general_public   64567.7244  142177 0.4541362
# 0 to 4    0 pregnant_women       0.0000       0       NaN
# 10 to 17    0 pregnant_women   30221.0000   30221 1.0000000
# 18 to 29    0 pregnant_women   49228.0331  156400 0.3147572
# 30 to 44    0 pregnant_women   26564.8820   84398 0.3147572
# 45 to 59    0 pregnant_women    1051.6040    3341 0.3147572
# 5 to 9    0 pregnant_women       0.0000       0       NaN
# 60 to 69    0 pregnant_women       0.0000       0       NaN
# 70 to 100    0 pregnant_women       0.0000       0       NaN
# 0 to 4    1 pregnant_women       0.0000       0       NaN
# 10 to 17    1 pregnant_women       0.0000   30221 0.0000000
# 18 to 29    1 pregnant_women   26957.9052  156400 0.1723651
# 30 to 44    1 pregnant_women   14547.2716   84398 0.1723651
# 45 to 59    1 pregnant_women     575.8719    3341 0.1723651
# 5 to 9    1 pregnant_women       0.0000       0       NaN
# 60 to 69    1 pregnant_women       0.0000       0       NaN
# 70 to 100    1 pregnant_women       0.0000       0       NaN
# 0 to 4    2 pregnant_women       0.0000       0       NaN
# 10 to 17    2 pregnant_women       0.0000   30221 0.0000000
# 18 to 29    2 pregnant_women   80214.0617  156400 0.5128776
# 30 to 44    2 pregnant_women   43285.8464   84398 0.5128776
# 45 to 59    2 pregnant_women    1713.5242    3341 0.5128776
# 5 to 9    2 pregnant_women       0.0000       0       NaN
# 60 to 69    2 pregnant_women       0.0000       0       NaN
# 70 to 100    2 pregnant_women       0.0000       0       NaN



###How are there pregnant women over 45???
aggregate(prev_state$pop, by=list(prev_state$age_group,prev_state$risk_group), FUN=sum)

aggregate(next_state$pop, by=list(next_state$age_group,next_state$risk_group), FUN=sum)
# 9     0 to 4 pregnant_women       0
# 10  10 to 17 pregnant_women   30221
# 11  18 to 29 pregnant_women  156400
# 12  30 to 44 pregnant_women   84398
# 13  45 to 59 pregnant_women    3341
# 14    5 to 9 pregnant_women       0
# 15  60 to 69 pregnant_women       0
# 16 70 to 100 pregnant_women       0
pop_risk_group_dn
# 9  pregnant_women    0 to 4       0
# 10 pregnant_women    5 to 9       0
# 11 pregnant_women  10 to 17   30221
# 12 pregnant_women  18 to 29  156400
# 13 pregnant_women  30 to 44   84398
# 14 pregnant_women  45 to 59    3341
# 15 pregnant_women  60 to 69       0
# 16 pregnant_women 70 to 100       0

#BINGO!
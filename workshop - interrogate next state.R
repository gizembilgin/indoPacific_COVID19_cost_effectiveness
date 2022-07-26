options(scipen = 100)

prev_state[prev_state$pop<0,]
next_state[next_state$pop<0,]
# pop class temp age_group dose vaccine_type     risk_group
# 60  -0.0519922640     S   11  30 to 44    1  AstraZeneca pregnant_women
# 61  -0.0045129390     S   12  45 to 59    1  AstraZeneca pregnant_women
# 73  -0.0187987614     S   24  18 to 29    1    Sinopharm pregnant_women
# 80  -0.0271562281     S   31  18 to 29    2  AstraZeneca pregnant_women
# 94  -0.0184321498     S   45  18 to 29    2    Sinopharm pregnant_women
# 95  -0.0351051520     S   46  30 to 44    2    Sinopharm pregnant_women
# 158 -0.0128650019     E   11  30 to 44    1  AstraZeneca pregnant_women
# 159 -0.0006648597     E   12  45 to 59    1  AstraZeneca pregnant_women
# 171 -0.0042926947     E   24  18 to 29    1    Sinopharm pregnant_women
# 178 -0.0061146146     E   31  18 to 29    2  AstraZeneca pregnant_women
# 192 -0.0041658027     E   45  18 to 29    2    Sinopharm pregnant_women
# 193 -0.0085908380     E   46  30 to 44    2    Sinopharm pregnant_women
# 256 -0.0823107208     I   11  30 to 44    1  AstraZeneca pregnant_women
# 257 -0.0042479391     I   12  45 to 59    1  AstraZeneca pregnant_women
# 269 -0.0277019749     I   24  18 to 29    1    Sinopharm pregnant_women
# 276 -0.0396234877     I   31  18 to 29    2  AstraZeneca pregnant_women
# 290 -0.0269650989     I   45  18 to 29    2    Sinopharm pregnant_women
# 291 -0.0551490507     I   46  30 to 44    2    Sinopharm pregnant_women
# 354 -0.5021220201     R   11  30 to 44    1  AstraZeneca pregnant_women
# 355 -0.0269528247     R   12  45 to 59    1  AstraZeneca pregnant_women
# 367 -0.1670789347     R   24  18 to 29    1    Sinopharm pregnant_women
# 374 -0.2351441722     R   31  18 to 29    2  AstraZeneca pregnant_women
# 388 -0.1607191575     R   45  18 to 29    2    Sinopharm pregnant_women
# 389 -0.3323915185     R   46  30 to 44    2    Sinopharm pregnant_women

aggregate(next_state$pop, by=list(next_state$age_group), FUN=sum)
pop
#CHECKED: pop remains constant

workshop = aggregate(next_state$pop, by=list(next_state$age_group,next_state$dose), FUN=sum)
colnames(workshop) = c('age_group','dose','doses')
workshop = workshop %>% left_join(pop_setting) %>% mutate(cov = doses/pop)
print(workshop, row.names = FALSE)
# age_group dose      doses     pop        cov
# 0 to 4    0 1181771.00 1181771 1.00000000
# 18 to 29    0  897647.45 1823027 0.49239394
# 30 to 44    0  740635.27 1473299 0.50270534
# 45 to 59    0  400772.67  765749 0.52337342
# 5 to 17    0 2620807.90 2673643 0.98023854
# 60 to 69    0  134180.84  246776 0.54373538
# 70 to 100    0   77306.71  142177 0.54373568
# 0 to 4    1       0.00 1181771 0.00000000
# 18 to 29    1  386449.30 1823027 0.21198222
# 30 to 44    1  297121.42 1473299 0.20167082
# 45 to 59    1  138602.66  765749 0.18100274
# 5 to 17    1   52835.10 2673643 0.01976146
# 60 to 69    1   39642.29  246776 0.16064077
# 70 to 100    1   22839.38  142177 0.16064048
# 0 to 4    2       0.00 1181771 0.00000000
# 18 to 29    2  538930.25 1823027 0.29562384
# 30 to 44    2  435542.31 1473299 0.29562384
# 45 to 59    2  226373.66  765749 0.29562384
# 5 to 17    2       0.00 2673643 0.00000000
# 60 to 69    2   72952.87  246776 0.29562384
# 70 to 100    2   42030.91  142177 0.29562384

workshop = aggregate(next_state$pop, by=list(next_state$age_group,next_state$dose,next_state$risk_group), FUN=sum)
colnames(workshop) = c('age_group','dose','risk_group','doses')
workshop = workshop %>% left_join(pop_risk_group_dn) %>% mutate(cov = doses/pop)
print(workshop, row.names = FALSE)
# age_group dose     risk_group        doses     pop          cov
# 0 to 4    0 general_public       0.0000 1181771  0.000000000
# 18 to 29    0 general_public   45386.2182 1640724  0.027662311
# 30 to 44    0 general_public   24605.0876 1362802  0.018054778
# 45 to 59    0 general_public    -834.4857  746605 -0.001117707
# 5 to 17    0 general_public       0.0000 2673643  0.000000000
# 60 to 69    0 general_public       0.0000  246776  0.000000000
# 70 to 100    0 general_public       0.0000  142177  0.000000000
# 0 to 4    1 general_public       0.0000 1181771  0.000000000
# 18 to 29    1 general_public   83023.6685 1640724  0.050601849
# 30 to 44    1 general_public   53226.3647 1362802  0.039056565
# 45 to 59    1 general_public   14319.0628  746605  0.019178900
# 5 to 17    1 general_public       0.0000 2673643  0.000000000
# 60 to 69    1 general_public       0.0000  246776  0.000000000
# 70 to 100    1 general_public       0.0000  142177  0.000000000
# 0 to 4    2 general_public       0.0000 1181771  0.000000000
# 18 to 29    2 general_public   53893.1133 1640724  0.032847154
# 30 to 44    2 general_public   32665.5477 1362802  0.023969401
# 45 to 59    2 general_public    5659.4228  746605  0.007580210
# 5 to 17    2 general_public       0.0000 2673643  0.000000000
# 60 to 69    2 general_public       0.0000  246776  0.000000000
# 70 to 100    2 general_public       0.0000  142177  0.000000000
# 0 to 4    0 pregnant_women 1181771.0000       0          Inf
# 18 to 29    0 pregnant_women  852261.2327  182303  4.674970970
# 30 to 44    0 pregnant_women  716030.1838  110497  6.480087096
# 45 to 59    0 pregnant_women  401607.1602   19144 20.978226087
# 5 to 17    0 pregnant_women 2620807.8985       0          Inf
# 60 to 69    0 pregnant_women  134180.8428       0          Inf
# 70 to 100    0 pregnant_women   77306.7072       0          Inf
# 0 to 4    1 pregnant_women       0.0000       0          NaN
# 18 to 29    1 pregnant_women  303425.6341  182303  1.664402857
# 30 to 44    1 pregnant_women  243895.0525  110497  2.207254971
# 45 to 59    1 pregnant_women  124283.6009   19144  6.492039330
# 5 to 17    1 pregnant_women   52835.1015       0          Inf
# 60 to 69    1 pregnant_women   39642.2879       0          Inf
# 70 to 100    1 pregnant_women   22839.3817       0          Inf
# 0 to 4    2 pregnant_women       0.0000       0          NaN
# 18 to 29    2 pregnant_women  485037.1332  182303  2.660609717
# 30 to 44    2 pregnant_women  402876.7637  110497  3.646042550
# 45 to 59    2 pregnant_women  220714.2389   19144 11.529159990
# 5 to 17    2 pregnant_women       0.0000       0          NaN
# 60 to 69    2 pregnant_women   72952.8693       0          Inf
# 70 to 100    2 pregnant_women   42030.9110       0          Inf



###How are there pregnant women over 45???
aggregate(prev_state$pop, by=list(prev_state$age_group,prev_state$risk_group), FUN=sum)

aggregate(next_state$pop, by=list(next_state$age_group,next_state$risk_group), FUN=sum)
# 1     0 to 4 general_public       0
# 2   18 to 29 general_public  182303
# 3   30 to 44 general_public  110497
# 4   45 to 59 general_public   19144
# 5    5 to 17 general_public       0
# 6   60 to 69 general_public       0
# 7  70 to 100 general_public       0
# 8     0 to 4 pregnant_women 1181771
# 9   18 to 29 pregnant_women 1640724
# 10  30 to 44 pregnant_women 1362802
# 11  45 to 59 pregnant_women  746605
# 12   5 to 17 pregnant_women 2673643
# 13  60 to 69 pregnant_women  246776
# 14 70 to 100 pregnant_women  142177
pop_risk_group_dn
#risk_group age_group     pop
# 1  general_public    0 to 4 1181771
# 2  general_public   5 to 17 2673643
# 3  general_public  18 to 29 1640724
# 4  general_public  30 to 44 1362802
# 5  general_public  45 to 59  746605
# 6  general_public  60 to 69  246776
# 7  general_public 70 to 100  142177
# 8  pregnant_women    0 to 4       0
# 9  pregnant_women   5 to 17       0
# 10 pregnant_women  18 to 29  182303
# 11 pregnant_women  30 to 44  110497
# 12 pregnant_women  45 to 59   19144
# 13 pregnant_women  60 to 69       0
# 14 pregnant_women 70 to 100       0

#BINGO!
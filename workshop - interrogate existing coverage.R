### three datasets include information on existing coverage ##############################################################################
# (1) vaccination_history_POP
# (2) vaccination_history_TRUE
# (3) vaccination_coverage_end_history

aggregate(vaccination_history_POP$doses_delivered_this_date,by=list(vaccination_history_POP$dose),FUN=sum)
#dose 1 = 1987563
#dose 2 = 1315830

aggregate(vaccination_history_TRUE$doses_delivered_this_date,by=list(vaccination_history_TRUE$dose),FUN=sum)
#dose 1 = 1987563
#dose 2 = 1315830

aggregate(vaccination_history_TRUE$doses_delivered_this_date,by=list(vaccination_history_TRUE$dose,vaccination_history_TRUE$age_group),FUN=sum)
# Dose      Age_group  Doses
#       1    0 to 4      0.00
#       2    0 to 4      0.00
#       1  18 to 29 814054.87
#       2  18 to 29 538930.25
#       1  30 to 44 657887.25
#       2  30 to 44 435542.31
#       1  45 to 59 341937.72
#      2  45 to 59 226373.66
#      1   5 to 17      0.00
#      2   5 to 17      0.00
#      1  60 to 69 110195.41
#      2  60 to 69  72952.87
#      1 70 to 100  63487.75
#      2 70 to 100  42030.91

workshop = aggregate(vaccination_history_TRUE$doses_delivered_this_date,by=list(vaccination_history_TRUE$dose,vaccination_history_TRUE$age_group),FUN=sum)
colnames(workshop) = c('dose','age_group','doses')
print(workshop %>% left_join(pop_setting) %>% mutate(cov = doses/pop), row.names = FALSE)
# dose age_group     doses     pop       cov
# 1    0 to 4      0.00 1181771 0.0000000
# 2    0 to 4      0.00 1181771 0.0000000
# 1  18 to 29 814054.87 1823027 0.4465402
# 2  18 to 29 538930.25 1823027 0.2956238
# 1  30 to 44 657887.25 1473299 0.4465402
# 2  30 to 44 435542.31 1473299 0.2956238
# 1  45 to 59 341937.72  765749 0.4465402
# 2  45 to 59 226373.66  765749 0.2956238
# 1   5 to 17      0.00 2673643 0.0000000
# 2   5 to 17      0.00 2673643 0.0000000
# 1  60 to 69 110195.41  246776 0.4465402
# 2  60 to 69  72952.87  246776 0.2956238
# 1 70 to 100  63487.75  142177 0.4465402
# 2 70 to 100  42030.91  142177 0.2956238


workshop = aggregate(vaccine_coverage_end_history$coverage_this_date,by=list(vaccine_coverage_end_history$dose,vaccine_coverage_end_history$age_group, vaccine_coverage_end_history$risk_group),FUN=sum)
print(workshop, row.names = FALSE)
#      Dose Age_group Coverage(%)
# 1    0 to 4 general_public 0.0000000
# 2    0 to 4 general_public 0.0000000
# 1  18 to 29 general_public 0.4465402
# 2  18 to 29 general_public 0.2956238
# 1  30 to 44 general_public 0.4465402
# 2  30 to 44 general_public 0.2956238
# 1  45 to 59 general_public 0.4465402
# 2  45 to 59 general_public 0.2956238
# 1   5 to 17 general_public 0.0000000
# 2   5 to 17 general_public 0.0000000
# 1  60 to 69 general_public 0.4465402
# 2  60 to 69 general_public 0.2956238
# 1 70 to 100 general_public 0.4465402
# 2 70 to 100 general_public 0.2956238
# 1    0 to 4 pregnant_women 0.0000000
# 2    0 to 4 pregnant_women 0.0000000
# 1  18 to 29 pregnant_women 0.4465402
# 2  18 to 29 pregnant_women 0.2956238
# 1  30 to 44 pregnant_women 0.4465402
# 2  30 to 44 pregnant_women 0.2956238
# 1  45 to 59 pregnant_women 0.4465402
# 2  45 to 59 pregnant_women 0.2956238
# 1   5 to 17 pregnant_women 0.0000000
# 2   5 to 17 pregnant_women 0.0000000
# 1  60 to 69 pregnant_women 0.0000000
# 2  60 to 69 pregnant_women 0.0000000
# 1 70 to 100 pregnant_women 0.0000000
# 2 70 to 100 pregnant_women 0.0000000



### These are added to the vax_delivery_outline
aggregate(at_risk_delivery_outline$doses_delivered_this_date,by=list(at_risk_delivery_outline$dose),FUN=sum)
# 1       1 135215
# 2       2      0
aggregate(generalPublic_restricted_outline$doses_delivered_this_date,by=list(generalPublic_restricted_outline$dose),FUN=sum)
# 1       1 33050.72
# 2       2     0.00
aggregate(generalPublic_leftover_outline$doses_delivered_this_date,by=list(generalPublic_leftover_outline$dose),FUN=sum)
# 1       1 4113881
# 2       2       0


aggregate(vaccination_history_FINAL$doses_delivered_this_date,by=list(vaccination_history_FINAL$dose),FUN=sum)
# 1       1 6269710
# 2       2 1315830


aggregate(vaccination_history_FINAL$doses_delivered_this_date,by=list(vaccination_history_FINAL$dose,vaccination_history_FINAL$age_group),FUN=sum)
#RISK = 2
# Dose      Age_group  Doses
# 1        1    0 to 4       0.00
# 2        2    0 to 4       0.00
# 3        1  18 to 29 1609957.92
# 4        2  18 to 29  538930.25
# 5        1  30 to 44 1301289.92
# 6        2  30 to 44  435542.31
# 7        1  45 to 59  676497.86
# 8        2  45 to 59  226373.66
# 9        1   5 to 17 2371358.50
# 10       2   5 to 17       0.00
# 11       1  60 to 69  218036.54
# 12       2  60 to 69   72952.87
# 13       1 70 to 100  125619.97
# 14       2 70 to 100   42030.91


workshop = aggregate(vaccination_history_FINAL$doses_delivered_this_date,by=list(vaccination_history_FINAL$dose,vaccination_history_FINAL$age_group),FUN=sum)
colnames(workshop) = c('dose','age_group','doses')
workshop = workshop %>% left_join(pop_setting) %>% mutate(cov = doses/pop)
print(workshop, row.names = FALSE)
#RISK = 2
# dose age_group      doses     pop       cov
# 1    0 to 4       0.00 1181771 0.0000000
# 2    0 to 4       0.00 1181771 0.0000000
# 1  18 to 29 1609957.92 1823027 0.8831235
# 2  18 to 29  538930.25 1823027 0.2956238
# 1  30 to 44 1301289.92 1473299 0.8832490
# 2  30 to 44  435542.31 1473299 0.2956238
# 1  45 to 59  676497.86  765749 0.8834460
# 2  45 to 59  226373.66  765749 0.2956238
# 1   5 to 17 2371358.50 2673643 0.8869391
# 2   5 to 17       0.00 2673643 0.0000000
# 1  60 to 69  218036.54  246776 0.8835403
# 2  60 to 69   72952.87  246776 0.2956238
# 1 70 to 100  125619.97  142177 0.8835464
# 2 70 to 100   42030.91  142177 0.2956238

#RISK = 1
# dose age_group      doses     pop       cov
# 1    0 to 4       0.00 1181771 0.0000000
# 2    0 to 4       0.00 1181771 0.0000000
# 1  18 to 29 1604263.87 1823027 0.8800001
# 2  18 to 29  538930.25 1823027 0.2956238
# 1  30 to 44 1296503.25 1473299 0.8800001
# 2  30 to 44  435542.31 1473299 0.2956238
# 1  45 to 59  673858.72  765749 0.8799995
# 2  45 to 59  226373.66  765749 0.2956238
# 1   5 to 17 2352806.00 2673643 0.8800001
# 2   5 to 17       0.00 2673643 0.0000000
# 1  60 to 69  217162.41  246776 0.8799981
# 2  60 to 69   72952.87  246776 0.2956238
# 1 70 to 100  125115.75  142177 0.8799999
# 2 70 to 100   42030.91  142177 0.2956238

workshop = aggregate(vaccination_history_FINAL$doses_delivered_this_date,by=list(vaccination_history_FINAL$dose,vaccination_history_FINAL$age_group,vaccination_history_FINAL$vaccine_type),FUN=sum)
colnames(workshop) = c('dose','age_group','vaccine_type','doses')
workshop = workshop %>% left_join(pop_setting) %>% mutate(cov = doses/pop)
print(workshop, row.names = FALSE)
# dose age_group      vaccine_type      doses     pop       cov
# 1    0 to 4       AstraZeneca       0.00 1181771 0.0000000
# 2    0 to 4       AstraZeneca       0.00 1181771 0.0000000
# 1  18 to 29       AstraZeneca  332365.51 1823027 0.1823152
# 2  18 to 29       AstraZeneca  220036.46 1823027 0.1206984
# 1  30 to 44       AstraZeneca  268604.78 1473299 0.1823152
# 2  30 to 44       AstraZeneca  177824.85 1473299 0.1206984
# 1  45 to 59       AstraZeneca  139607.67  765749 0.1823152
# 2  45 to 59       AstraZeneca   92424.69  765749 0.1206984
# 1   5 to 17       AstraZeneca       0.00 2673643 0.0000000
# 2   5 to 17       AstraZeneca       0.00 2673643 0.0000000
# 1  60 to 69       AstraZeneca   44991.01  246776 0.1823152
# 2  60 to 69       AstraZeneca   29785.47  246776 0.1206984
# 1 70 to 100       AstraZeneca   25921.03  142177 0.1823152
# 2 70 to 100       AstraZeneca   17160.54  142177 0.1206984
# 1    0 to 4 Johnson & Johnson       0.00 1181771 0.0000000
# 2    0 to 4 Johnson & Johnson       0.00 1181771 0.0000000
# 1  18 to 29 Johnson & Johnson  795903.06 1823027 0.4365833
# 2  18 to 29 Johnson & Johnson       0.00 1823027 0.0000000
# 1  30 to 44 Johnson & Johnson  643402.66 1473299 0.4367088
# 2  30 to 44 Johnson & Johnson       0.00 1473299 0.0000000
# 1  45 to 59 Johnson & Johnson  334560.14  765749 0.4369057
# 2  45 to 59 Johnson & Johnson       0.00  765749 0.0000000
# 1   5 to 17 Johnson & Johnson 2371358.50 2673643 0.8869391
# 2   5 to 17 Johnson & Johnson       0.00 2673643 0.0000000
# 1  60 to 69 Johnson & Johnson  107841.13  246776 0.4370001
# 2  60 to 69 Johnson & Johnson       0.00  246776 0.0000000
# 1 70 to 100 Johnson & Johnson   62132.23  142177 0.4370062
# 2 70 to 100 Johnson & Johnson       0.00  142177 0.0000000
# 1    0 to 4         Sinopharm       0.00 1181771 0.0000000
# 2    0 to 4         Sinopharm       0.00 1181771 0.0000000
# 1  18 to 29         Sinopharm  481689.36 1823027 0.2642250
# 2  18 to 29         Sinopharm  318893.79 1823027 0.1749254
# 1  30 to 44         Sinopharm  389282.47 1473299 0.2642250
# 2  30 to 44         Sinopharm  257717.47 1473299 0.1749254
# 1  45 to 59         Sinopharm  202330.05  765749 0.2642250
# 2  45 to 59         Sinopharm  133948.98  765749 0.1749254
# 1   5 to 17         Sinopharm       0.00 2673643 0.0000000
# 2   5 to 17         Sinopharm       0.00 2673643 0.0000000
# 1  60 to 69         Sinopharm   65204.40  246776 0.2642250
# 2  60 to 69         Sinopharm   43167.40  246776 0.1749254
# 1 70 to 100         Sinopharm   37566.72  142177 0.2642250
# 2 70 to 100         Sinopharm   24870.37  142177 0.1749254

#Looking at 45 to 59
workshop = workshop %>% filter(age_group == '45 to 59')
print(workshop, row.names = FALSE)
# dose age_group      vaccine_type     doses    pop       cov
# 1  45 to 59       AstraZeneca 139607.67 765749 0.1823152
# 2  45 to 59       AstraZeneca  92424.69 765749 0.1206984
# 1  45 to 59 Johnson & Johnson 334560.14 765749 0.4369057
# 2  45 to 59 Johnson & Johnson      0.00 765749 0.0000000
# 1  45 to 59         Sinopharm 202330.05 765749 0.2642250
# 2  45 to 59         Sinopharm 133948.98 765749 0.1749254


# Does not out do existing pop!
#Therefore, issue not here but either in initalisation or delivery of vaccine over time

#vaccine_coverage
workshop = aggregate(vaccine_coverage$cov,by=list(vaccine_coverage$risk_group,vaccine_coverage$dose,vaccine_coverage$age_group),FUN=sum)
print(workshop, row.names = FALSE)
# general_public       1    0 to 4 0.0000000
# pregnant_women       1    0 to 4 0.0000000
# general_public       2    0 to 4 0.0000000
# pregnant_women       2    0 to 4 0.0000000
# general_public       1  18 to 29 0.4465402
# pregnant_women       1  18 to 29 0.4465402
# general_public       2  18 to 29 0.2956238
# pregnant_women       2  18 to 29 0.2956238
# general_public       1  30 to 44 0.4465402
# pregnant_women       1  30 to 44 0.4465402
# general_public       2  30 to 44 0.2956238
# pregnant_women       2  30 to 44 0.2956238
# general_public       1  45 to 59 0.4465402
# pregnant_women       1  45 to 59 0.4465402
# general_public       2  45 to 59 0.2956238
# pregnant_women       2  45 to 59 0.2956238
# general_public       1   5 to 17 0.0000000
# pregnant_women       1   5 to 17 0.0000000
# general_public       2   5 to 17 0.0000000
# pregnant_women       2   5 to 17 0.0000000
# general_public       1  60 to 69 0.4465402
# pregnant_women       1  60 to 69 0.0000000
# general_public       2  60 to 69 0.2956238
# pregnant_women       2  60 to 69 0.0000000
# general_public       1 70 to 100 0.4465402
# pregnant_women       1 70 to 100 0.0000000
# general_public       2 70 to 100 0.2956238
# pregnant_women       2 70 to 100 0.0000000

workshop = aggregate(state_tidy$state_inital,by=list(state_tidy$dose,state_tidy$age_group,state_tidy$risk_group),FUN=sum)
colnames(workshop) = c('dose','age_group','risk_group','coverage') 
workshop = workshop %>% left_join(pop_risk_group_dn) %>% mutate(prop = coverage/pop)
print(workshop, row.names = FALSE)
#dose age_group     risk_group    coverage     pop      prop
# 0    0 to 4 general_public 1181771.000 1181771 1.0000000
# 1    0 to 4 general_public       0.000 1181771 0.0000000
# 2    0 to 4 general_public       0.000 1181771 0.0000000
# 0   5 to 17 general_public 2673643.000 2673643 1.0000000
# 1   5 to 17 general_public       0.000 2673643 0.0000000
# 2   5 to 17 general_public       0.000 2673643 0.0000000
# 0  18 to 29 general_public  908074.752 1640724 0.5534598
# 1  18 to 29 general_public  247612.114 1640724 0.1509164
# 2  18 to 29 general_public  485037.133 1640724 0.2956238
# 0  30 to 44 general_public  754256.102 1362802 0.5534598
# 1  30 to 44 general_public  205669.134 1362802 0.1509164
# 2  30 to 44 general_public  402876.764 1362802 0.2956238
# 0  45 to 59 general_public  413215.843  746605 0.5534598
# 1  45 to 59 general_public  112674.918  746605 0.1509164
# 2  45 to 59 general_public  220714.239  746605 0.2956238
# 0  60 to 69 general_public  136580.592  246776 0.5534598
# 1  60 to 69 general_public   37242.539  246776 0.1509164
# 2  60 to 69 general_public   72952.869  246776 0.2956238
# 0 70 to 100 general_public   78689.252  142177 0.5534598
# 1 70 to 100 general_public   21456.837  142177 0.1509164
# 2 70 to 100 general_public   42030.911  142177 0.2956238
# 0    0 to 4 pregnant_women       0.000       0       NaN
# 1    0 to 4 pregnant_women       0.000       0       NaN
# 2    0 to 4 pregnant_women       0.000       0       NaN
# 0   5 to 17 pregnant_women       0.000       0       NaN
# 1   5 to 17 pregnant_women       0.000       0       NaN
# 2   5 to 17 pregnant_women       0.000       0       NaN
# 0  18 to 29 pregnant_women  100897.379  182303 0.5534598
# 1  18 to 29 pregnant_women   27512.507  182303 0.1509164
# 2  18 to 29 pregnant_women   53893.113  182303 0.2956238
# 0  30 to 44 pregnant_women   61155.646  110497 0.5534598
# 1  30 to 44 pregnant_women   16675.806  110497 0.1509164
# 2  30 to 44 pregnant_women   32665.548  110497 0.2956238
# 0  45 to 59 pregnant_women   10595.434   19144 0.5534598
# 1  45 to 59 pregnant_women    2889.143   19144 0.1509164
# 2  45 to 59 pregnant_women    5659.423   19144 0.2956238
# 0  60 to 69 pregnant_women       0.000       0       NaN
# 1  60 to 69 pregnant_women       0.000       0       NaN
# 2  60 to 69 pregnant_women       0.000       0       NaN
# 0 70 to 100 pregnant_women       0.000       0       NaN
# 1 70 to 100 pregnant_women       0.000       0       NaN
# 2 70 to 100 pregnant_women       0.000       0       NaN

#Still looks very standard...

#Interrogate vax coverage of next_state (in workshop - interrogate next state)









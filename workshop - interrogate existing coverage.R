### three datasets include information on existing coverage ##############################################################################
# (1) vaccination_history_POP
# (2) vaccination_history_TRUE
# (3) vaccination_coverage_end_history

aggregate(vaccination_history_POP$doses_delivered_this_date,by=list(vaccination_history_POP$dose),FUN=sum)
#dose 1 = 1727305
#dose 2 = 1114047

aggregate(vaccination_history_TRUE$doses_delivered_this_date,by=list(vaccination_history_TRUE$dose),FUN=sum)
#dose 1 = 1727305
#dose 2 = 1114047
aggregate(vaccination_history_TRUE$doses_delivered_this_date,by=list(vaccination_history_TRUE$dose,vaccination_history_TRUE$age_group),FUN=sum)
# Dose      Age_group  Doses
#       1     0-4      0.00
#       2     0-4      0.00
#       1   18-29 707459.86
#       2   18-29 456285.10
#       1   30-44 571741.34
#       2   30-44 368751.74
#       1   45-59 297163.28
#       2   45-59 191659.18
#       1    5-17      0.00
#       2    5-17      0.00
#       1   60-69  95766.06
#       2   60-69  61765.52
#       1  70-100  55174.45
#       2  70-100  35585.46
workshop = aggregate(vaccination_history_TRUE$doses_delivered_this_date,by=list(vaccination_history_TRUE$dose,vaccination_history_TRUE$age_group),FUN=sum)
colnames(workshop) = c('dose','agegroup','doses')
workshop %>% left_join(pop_setting) %>% mutate(cov = doses/pop)
# dose agegroup     doses     pop       cov
# 1     1      0-4      0.00 1181771 0.0000000
# 2     2      0-4      0.00 1181771 0.0000000
# 3     1    18-29 707459.86 1823027 0.3880688
# 4     2    18-29 456285.10 1823027 0.2502898
# 5     1    30-44 571741.34 1473299 0.3880688
# 6     2    30-44 368751.74 1473299 0.2502898
# 7     1    45-59 297163.28  765749 0.3880688
# 8     2    45-59 191659.18  765749 0.2502898
# 9     1     5-17      0.00 2673643 0.0000000
# 10    2     5-17      0.00 2673643 0.0000000
# 11    1    60-69  95766.06  246776 0.3880688
# 12    2    60-69  61765.52  246776 0.2502898
# 13    1   70-100  55174.45  142177 0.3880688
# 14    2   70-100  35585.46  142177 0.2502898


aggregate(vaccine_coverage_end_history$cov,by=list(vaccine_coverage_end_history$dose,vaccine_coverage_end_history$age_group),FUN=sum)
#      Dose Age_group Coverage(%)
#        1     0-4 0.0000000
#        2     0-4 0.0000000
#        1   18-29 0.3880688
#        2   18-29 0.2502898
#        1   30-44 0.3880688
#        2   30-44 0.2502898
#        1   45-59 0.3880688
#        2   45-59 0.2502898
#        1    5-17 0.0000000
#        2    5-17 0.0000000
#        1   60-69 0.3880688
#        2   60-69 0.2502898
#        1  70-100 0.3880688
#        2  70-100 0.2502898



### These are added to the vax_delivery_outline
# vaccination_history_MODF = rbind(vaccination_history_TRUE,vax_delivery_outline)
aggregate(vax_delivery_outline$doses_delivered_this_date,by=list(vax_delivery_outline$dose,vax_delivery_outline$age_group),FUN=sum)
# 1        1     0-4      0
# 2        2     0-4      0
# 3        1   18-29 896804
# 4        2   18-29      0
# 5        1   30-44 724762
# 6        2   30-44      0
# 7        1   45-59 376696
# 8        2   45-59      0
# 9        1    5-17      0
# 10       2    5-17      0
# 11       1   60-69 121397
# 12       2   60-69      0
# 13       1  70-100  69941
# 14       2  70-100      0


aggregate(vaccination_history_MODF$doses_delivered_this_date,by=list(vaccination_history_MODF$dose,vaccination_history_MODF$age_group),FUN=sum)
# 1        1     0-4       0.00
# 2        2     0-4       0.00
# 3        1   18-29 1604263.86
# 4        2   18-29  456285.10
# 5        1   30-44 1296503.34
# 6        2   30-44  368751.74
# 7        1   45-59  673859.28
# 8        2   45-59  191659.18
# 9        1    5-17       0.00
# 10       2    5-17       0.00
# 11       1   60-69  217163.06
# 12       2   60-69   61765.52
# 13       1  70-100  125115.45
# 14       2  70-100   35585.46

pop
#1181771 2673643 1823027 1473299  765749  246776  142177


#Does not out do existing pop!
#Therefore, issue not here but either in initalisation or delivery of vaccine over time

#vaccine_coverage
aggregate(vaccine_coverage$cov,by=list(vaccine_coverage$dose,vaccine_coverage$age_group),FUN=sum)

aggregate(S_tidy$S_inital,by=list(S_tidy$dose,S_tidy$age_group),FUN=sum)

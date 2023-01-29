### three datasets include information on existing coverage ##############################################################################
# (1) vaccination_history_POP
# (2) vaccination_history_TRUE
# (3) vaccination_coverage_end_history

aggregate(vaccination_history_POP$doses_delivered_this_date,by=list(vaccination_history_POP$dose),FUN=sum)
#dose 1 = 2904588
#dose 2 = 1036386


workshop = aggregate(vaccination_history_TRUE$doses_delivered_this_date,by=list(vaccination_history_TRUE$dose,vaccination_history_TRUE$age_group),FUN=sum)
colnames(workshop) = c('dose','age_group','doses')
print(workshop %>% left_join(pop_setting) %>% mutate(cov = doses/pop), row.names = FALSE)
# dose age_group      doses     pop       cov
# 1    0 to 4       0.00 1181771 0.0000000
# 2    0 to 4       0.00 1181771 0.0000000
# 1  10 to 17       0.00 1583312 0.0000000
# 2  10 to 17       0.00 1583312 0.0000000
# 1  18 to 29 1189644.81 1823027 0.6525657
# 2  18 to 29  424477.15 1823027 0.2328419
# 1  30 to 44  961424.33 1473299 0.6525657
# 2  30 to 44  343045.80 1473299 0.2328419
# 1  45 to 59  499701.50  765749 0.6525657
# 2  45 to 59  178298.48  765749 0.2328419
# 1    5 to 9       0.00 1090331 0.0000000
# 2    5 to 9       0.00 1090331 0.0000000
# 1  60 to 69  161037.54  246776 0.6525657
# 2  60 to 69   57459.80  246776 0.2328419
# 1 70 to 100   92779.83  142177 0.6525657
# 2 70 to 100   33104.77  142177 0.2328419


workshop = vaccination_history_TRUE %>% group_by(age_group,dose,risk_group) %>%
  summarise(cov = sum(doses_delivered_this_date))


workshop = aggregate(vaccine_coverage_end_history$coverage_this_date,by=list(vaccine_coverage_end_history$dose,vaccine_coverage_end_history$age_group, vaccine_coverage_end_history$risk_group),FUN=sum)
print(workshop, row.names = FALSE)
# 1    0 to 4 general_public 0.0000000
# 2    0 to 4 general_public 0.0000000
# 1  10 to 17 general_public 0.0000000
# 2  10 to 17 general_public 0.0000000
# 1  18 to 29 general_public 0.6525657
# 2  18 to 29 general_public 0.2328419
# 1  30 to 44 general_public 0.6525657
# 2  30 to 44 general_public 0.2328419
# 1  45 to 59 general_public 0.6525657
# 2  45 to 59 general_public 0.2328419
# 1    5 to 9 general_public 0.0000000
# 2    5 to 9 general_public 0.0000000
# 1  60 to 69 general_public 0.6525657
# 2  60 to 69 general_public 0.2328419
# 1 70 to 100 general_public 0.6525657
# 2 70 to 100 general_public 0.2328419
# 1    0 to 4 pregnant_women 0.0000000
# 2    0 to 4 pregnant_women 0.0000000
# 1  10 to 17 pregnant_women 0.0000000
# 2  10 to 17 pregnant_women 0.0000000
# 1  18 to 29 pregnant_women 0.6525657
# 2  18 to 29 pregnant_women 0.2328419
# 1  30 to 44 pregnant_women 0.6525657
# 2  30 to 44 pregnant_women 0.2328419
# 1  45 to 59 pregnant_women 0.6525657
# 2  45 to 59 pregnant_women 0.2328419
# 1    5 to 9 pregnant_women 0.0000000
# 2    5 to 9 pregnant_women 0.0000000
# 1  60 to 69 pregnant_women 0.0000000
# 2  60 to 69 pregnant_women 0.0000000
# 1 70 to 100 pregnant_women 0.0000000
# 2 70 to 100 pregnant_women 0.0000000



### These are added to the vax_delivery_outline
aggregate(at_risk_delivery_outline$doses_delivered_this_date,by=list(at_risk_delivery_outline$dose,at_risk_delivery_outline$risk_group),FUN=sum)
# 1       1 adults_with_comorbidities 138366.4
# 2       2 adults_with_comorbidities 138366.4
# 3       8 adults_with_comorbidities 397007.1
#Perfectly 88% of pop

pop_risk_group
# risk_group     pop
# 1 adults_with_comorbidities  608379
# 2 general_public            7698063

booster = at_risk_delivery_outline %>% filter(dose == 8)
aggregate(booster$doses_delivered_this_date,by=list(booster$dose,booster$risk_group,booster$FROM_vaccine_type,booster$FROM_dose),FUN=sum)
# 1       8 adults_with_comorbidities       AstraZeneca       1   2257.180
# 2       8 adults_with_comorbidities Johnson & Johnson       1 237534.762
# 3       8 adults_with_comorbidities            Pfizer       1   2631.007
# 4       8 adults_with_comorbidities         Sinopharm       1    759.273
# 5       8 adults_with_comorbidities       AstraZeneca       2  61482.797
# 6       8 adults_with_comorbidities Johnson & Johnson       2      0.000
# 7       8 adults_with_comorbidities            Pfizer       2  71659.789
# 8       8 adults_with_comorbidities         Sinopharm       2  20682.296

vaccination_history_FINAL %>% filter(dose == 8) %>% group_by(dose,risk_group,FROM_vaccine_type,FROM_dose) %>% summarise(num = sum(doses_delivered_this_date))

booster_target = vaccination_history_TRUE %>% filter(risk_group == risk_group_name)
workshop = aggregate(booster_target$doses_delivered_this_date,by=list(booster_target$dose,booster_target$vaccine_type),FUN=sum)
colnames(workshop) = c('dose','vaccine_type','doses')
print(workshop, row.names = FALSE)
# dose      vaccine_type     doses
# 1       AstraZeneca 107219.27
# 2       AstraZeneca  62056.39
# 1 Johnson & Johnson 158522.29
# 2 Johnson & Johnson      0.00
# 1            Pfizer 124967.01
# 2            Pfizer  72328.43
# 1         Sinopharm  36067.64
# 2         Sinopharm  20875.24

dataset= fitted_next_state
dataset = next_state
workshop = dataset %>% 
  filter(risk_group == risk_group_name) %>% 
  group_by(risk_group,dose,vaccine_type) %>%
  summarise(state=sum(pop)) 
print(workshop, row.names = FALSE)
# risk_group                 dose vaccine_type        state
# 1 adults_with_comorbidities     0 NA                211372.
# 2 adults_with_comorbidities     1 AstraZeneca        40785.
# 3 adults_with_comorbidities     1 Johnson & Johnson 141142.
# 4 adults_with_comorbidities     1 Pfizer             47536.
# 5 adults_with_comorbidities     1 Sinopharm          13720.
# 6 adults_with_comorbidities     2 AstraZeneca        61483.
# 7 adults_with_comorbidities     2 Johnson & Johnson      0 
# 8 adults_with_comorbidities     2 Pfizer             71660.
# 9 adults_with_comorbidities     2 Sinopharm          20682.




workshop = aggregate(vaccination_history_FINAL$doses_delivered_this_date,by=list(vaccination_history_FINAL$vaccine_type,vaccination_history_FINAL$dose,vaccination_history_FINAL$age_group),FUN=sum)
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
# dose age_group      doses     pop        cov
# 1    0 to 4       0.00 1181771 0.00000000
# 2    0 to 4       0.00 1181771 0.00000000
# 8    0 to 4       0.00 1181771 0.00000000
# 1  10 to 17       0.00 1583312 0.00000000
# 2  10 to 17       0.00 1583312 0.00000000
# 8  10 to 17       0.00 1583312 0.00000000
# 1  18 to 29 1525436.98 1823027 0.83676050
# 2  18 to 29  460941.14 1823027 0.25284384
# 8  18 to 29       0.00 1823027 0.00000000
# 1  30 to 44 1239041.46 1473299 0.84099796
# 2  30 to 44  405352.26 1473299 0.27513238
# 8  30 to 44   94219.35 1473299 0.06395128
# 1  45 to 59  643993.34  765749 0.84099795
# 2  45 to 59  210682.29  765749 0.27513231
# 8  45 to 59   48970.47  765749 0.06395107
# 1    5 to 9       0.00 1090331 0.00000000
# 2    5 to 9       0.00 1090331 0.00000000
# 8    5 to 9       0.00 1090331 0.00000000
# 1  60 to 69  217162.88  246776 0.88000000
# 2  60 to 69  118521.18  246776 0.48027841
# 8  60 to 69  161037.49  246776 0.65256543
# 1 70 to 100  125115.76  142177 0.88000000
# 2 70 to 100   68284.54  142177 0.48027841
# 8 70 to 100   92779.80  142177 0.65256543

workshop=vaccination_history_FINAL[vaccination_history_FINAL$date<date_start,]
workshop = aggregate(workshop$doses_delivered_this_date,by=list(workshop$dose,workshop$age_group),FUN=sum)
colnames(workshop) = c('dose','age_group','doses')
workshop = workshop %>% left_join(pop_setting) %>% mutate(cov = doses/pop)
print(workshop, row.names = FALSE)
# dose age_group      doses     pop       cov
# 1    0 to 4       0.00 1181771 0.0000000
# 2    0 to 4       0.00 1181771 0.0000000
# 1  10 to 17       0.00 1583312 0.0000000
# 2  10 to 17       0.00 1583312 0.0000000
# 1  18 to 29 1189644.40 1823027 0.6525654
# 2  18 to 29  460941.14 1823027 0.2528438
# 1  30 to 44  961424.00 1473299 0.6525654
# 2  30 to 44  372514.57 1473299 0.2528438
# 1  45 to 59  499701.33  765749 0.6525654
# 2  45 to 59  193614.92  765749 0.2528438
# 1    5 to 9       0.00 1090331 0.0000000
# 2    5 to 9       0.00 1090331 0.0000000
# 1  60 to 69  161037.49  246776 0.6525654
# 2  60 to 69   62395.79  246776 0.2528438
# 1 70 to 100   92779.80  142177 0.6525654
# 2 70 to 100   35948.58  142177 0.2528438

workshop = fitted_next_state %>% group_by(age_group,dose) %>% summarise(individuals=sum(pop))
workshop = workshop %>% left_join(pop_setting) %>% mutate(cov = individuals/pop) %>%
  filter(!age_group %in% c('0 to 4','5 to 9','10 to 17'))
print(workshop, row.names = FALSE)
# 1 18 to 29      0     456618. 1823027 0.250
# 2 18 to 29      1     905468. 1823027 0.497
# 3 18 to 29      2     460941. 1823027 0.253
# 4 30 to 44      0     369021. 1473299 0.250
# 5 30 to 44      1     731764. 1473299 0.497
# 6 30 to 44      2     372515. 1473299 0.253
# 7 45 to 59      0     191799.  765749 0.250
# 8 45 to 59      1     380335.  765749 0.497
# 9 45 to 59      2     193615.  765749 0.253
# 10 60 to 69      0      61811.  246776 0.250
# 11 60 to 69      1     122570.  246776 0.497
# 12 60 to 69      2      62396.  246776 0.253
# 13 70 to 100     0      35611.  142177 0.250
# 14 70 to 100     1      70617.  142177 0.497
# 15 70 to 100     2      35949.  142177 0.253


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
workshop = workshop %>% filter(age_group == '18 to 29')
print(workshop, row.names = FALSE)
# dose age_group      vaccine_type     doses    pop       cov
# 1  18 to 29       AstraZeneca  173983.10 1823027 0.09543638
# 2  18 to 29       AstraZeneca  169660.45 1823027 0.09306524
# 1  18 to 29 Johnson & Johnson 1168971.80 1823027 0.64122572
# 2  18 to 29 Johnson & Johnson       0.00 1823027 0.00000000
# 1  18 to 29            Pfizer  202782.32 1823027 0.11123385
# 2  18 to 29            Pfizer  197744.15 1823027 0.10847022
# 1  18 to 29         Sinopharm   58526.54 1823027 0.03210404
# 2  18 to 29         Sinopharm   57072.55 1823027 0.03130648


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





vaccination_history_FINAL %>% 
  filter(! age_group %in% c('0 to 4','5 to 9','10 to 17')) %>%
  mutate(dose = case_when(
    dose == 8 ~ 3,
    dose == 2 & vaccine_type == 'Johnson & Johnson' ~ 3,
    TRUE ~dose
  )) %>%
  group_by(risk_group,age_group,dose) %>%
  summarise(doses = sum(doses_delivered_this_date),.groups = "keep") %>%
  left_join(pop_risk_group_dn, by = c("risk_group", "age_group")) %>%
  mutate(cov=doses/pop) %>%
  arrange(dose,age_group)



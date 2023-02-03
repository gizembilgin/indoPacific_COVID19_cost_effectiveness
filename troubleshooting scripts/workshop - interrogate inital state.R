watch = vaccine_coverage 
colnames(watch) = c('risk_group','dose','vaccine_type','age_group','cov')
watch = watch %>% left_join(pop_setting) %>%
  mutate(pop=pop/sum(pop_setting$pop),
         interim = cov*pop)
aggregate(watch$interim, by=list(category=watch$dose),FUN=sum)


sum(state_tidy$state_inital[state_tidy$age_group == '0 to 4'])/pop[1] #CHECKED = 1
sum(state_tidy$state_inital[state_tidy$age_group == '70 to 100'])/pop[7]

sum(state_tidy$state_inital[state_tidy$dose == 0])/sum(pop)
sum(state_tidy$state_inital[state_tidy$dose == 1])/sum(pop)
sum(state_tidy$state_inital[state_tidy$dose == 2])/sum(pop)

age = '70 to 100'
sum(state_tidy$state_inital[state_tidy$dose == 0 & state_tidy$age_group == age])/pop_setting$pop[pop_setting$age_group == age]
sum(state_tidy$state_inital[state_tidy$dose == 1 & state_tidy$age_group == age])/pop_setting$pop[pop_setting$age_group == age]
sum(state_tidy$state_inital[state_tidy$dose == 2 & state_tidy$age_group == age])/pop_setting$pop[pop_setting$age_group == age]


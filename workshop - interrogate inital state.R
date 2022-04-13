watch = vaccine_coverage 
colnames(watch) = c('dose','vaccine_type','agegroup','cov')
watch = watch %>% left_join(pop_setting) %>%
  mutate(pop=pop/sum(pop_setting$pop),
         interim = cov*pop)
aggregate(watch$interim, by=list(category=watch$dose),FUN=sum)


sum(state_TIDY$state_inital[state_TIDY$age_group == '0-4'])/pop[1] #CHECKED = 1
sum(state_TIDY$state_inital[state_TIDY$age_group == '70-100'])/pop[7]

sum(state_TIDY$state_inital[state_TIDY$dose == 0])/sum(pop)
sum(state_TIDY$state_inital[state_TIDY$dose == 1])/sum(pop)
sum(state_TIDY$state_inital[state_TIDY$dose == 2])/sum(pop)

age = '70-100'
sum(state_TIDY$state_inital[state_TIDY$dose == 0 & state_TIDY$age_group == age])/pop_setting$pop[pop_setting$agegroup == age]
sum(state_TIDY$state_inital[state_TIDY$dose == 1 & state_TIDY$age_group == age])/pop_setting$pop[pop_setting$agegroup == age]
sum(state_TIDY$state_inital[state_TIDY$dose == 2 & state_TIDY$age_group == age])/pop_setting$pop[pop_setting$agegroup == age]


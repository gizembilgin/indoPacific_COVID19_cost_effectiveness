
#COMEBACK - should women be cycling in and out of this group?
#COMEBACK - * 3/4 to get women currently pregnant?  or 1/4 for third trimester? or * ~ 2 for all lactating women?

### read in data
ASFR = read.csv("1_inputs/SLE_ASFR.csv",header=TRUE)
women_pop = read.csv(paste(rootpath,"inputs/pop_estimates_female.csv",sep=''),header=TRUE)

### plot ASFR
View(ASFR)
ggplot(data=ASFR) + 
  geom_pointrange(aes(x=ASFR*100,y=AGE,xmin=LCI*100,xmax=UCI*100)) +
 # xlim(0,1) +
  xlab("Age-specific fertility ratio (%)") + 
  ylab("") + 
  labs(title="")

### adapt female pop estimates to model age group
#pop_setting_orig - colnames: age, country, population
#women_pop - colnames: age, country, population, population_thousands
women_pop = women_pop %>% rename(pop_women = population) %>% select(-population_thousands)
pop_together = pop_setting_orig %>% left_join(women_pop) %>%
  mutate(female_prop = pop_women/population)
ggplot(data=pop_together) + 
  geom_point(aes(x=female_prop*100,y=age)) +
  xlim(0,100) +
  ylim(15,49)

#HERE - make this into age groups of ASFR?


### apply female ratio to ASFR to retrieve prevalence of pregnant women

### adapt ASFR to model age groups
library(strex)
ASFR = ASFR %>% mutate(age_first =str_first_number(AGE), age_last = str_last_number(AGE))

### save as output (see dummy version in risk_group.csv) 
#colnames: risk_group, age_group, prop, source

require(readr); require(ggplot2); require(tidyverse)

setting_list = c("PNG","FJI","TLS","IDN")
age_groups_num = c(0,4,9,17,29,44,59,69,110)
age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100')


### Calculate YLL ##############################################################
#Step One: load population distribution
load(file = paste(gsub("4_cost_effectiveness_analysis","",getwd()),
                  "1_inputs/UN_world_population_prospects/UN_pop_est.Rdata",sep=''))

pop_orig <- UN_pop_est %>% 
  rename(country_long = Location,
         population = PopTotal,
         population_female = PopFemale,
         age = AgeGrp) %>%
  filter(ISO3_code %in% setting_list)
rm(UN_pop_est)
#_______________________________________________________________________________


#Step Two: load QALYs by severity and age
raw <- read.csv("2_inputs/age_severity_specific_QALYs.csv",header=TRUE) 
#ggplot(raw) + geom_line(aes(x=age,y=QALYs)) + facet_grid(severity ~ ., scale = "free_y")
#_______________________________________________________________________________


#Step Three: calculate QALYs per model age group weighted by pop dn
QALYs_nonFatal = raw %>%
  left_join(pop_orig, by = c('age')) %>%
  select(severity,ISO3_code,age,QALYs,population) %>%
  mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
  group_by(ISO3_code,severity,age_group) %>%
  mutate(group_percent = population/sum(population),
         interim = QALYs * group_percent) %>%
  summarise(QALYs = sum(interim),.groups="keep") 

#ggplot(QALYs_nonFatal) + geom_col(aes(x=age_group,y=QALYs,fill=as.factor(ISO3_code)),position="dodge") + facet_grid(severity ~ ., scale = "free_y")
#_______________________________________________________________________________

QALYs_fatal = QALYs_fatal %>% mutate(severity = "death")
QALYs_all = rbind(QALYs_nonFatal,QALYs_fatal)
ggplot(QALYs_all) + 
  geom_col(aes(x=age_group,y=QALYs,fill=as.factor(ISO3_code)),position="dodge") +
  theme_bw() +
  labs(fill="")+ 
  facet_grid(severity ~ ., scale = "free_y") 

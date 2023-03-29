require(readr); require(ggplot2); require(tidyverse)

discounting_rate = 0.0

setting_list = c("PNG","FJI","TLS","IDN")
age_groups_num = c(0,4,9,17,29,44,59,69,110)
age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100')


### Calculate YLL ##############################################################
#Step One: load population distribution
load(file = paste(gsub("cost_effectiveness_analysis","",getwd()),
                  "1_inputs/UN_world_population_prospects/UN_pop_est.Rdata",sep=''))

pop_orig <- UN_pop_est %>% 
  rename(country = ISO3_code,
         country_long = Location,
         population = PopTotal,
         population_female = PopFemale,
         age = AgeGrp)
rm(UN_pop_est)

# to_plot = pop_orig %>%
#   filter(country %in% setting_list) %>%
#   group_by(country) %>%
#   mutate(prop = population/sum(population))
# ggplot(to_plot) + geom_point(aes(x=age,y=prop,color=as.factor(country)))
#_______________________________________________________________________________

#Step Two: load life expectancy at age X
#"The average number of remaining years of life expected by a hypothetical cohort of individuals alive at age x 
# who would be subject during the remaining of their lives to the mortality rates of a given period."
# https://population.un.org/wpp/Download/Standard/Mortality/
load(file = paste(gsub("cost_effectiveness_analysis","",getwd()),
                  "1_inputs/UN_world_population_prospects/UN_lifeExpect_est.Rdata",sep=''))
# ggplot(UN_lifeExpect_est[UN_lifeExpect_est$ISO3_code %in% setting_list,]) +
#   geom_point(aes(x=AgeGrp,y=ex,color=as.factor(ISO3_code)),position="dodge")
#_______________________________________________________________________________

#Step Three: calculate YLL per model age group weighted by pop dn
YLL_FINAL = UN_lifeExpect_est %>%
  # filter(ISO3_code == setting) %>%
  rename(country = ISO3_code,
         life_expectancy = ex,
         age = AgeGrp) %>%
  left_join(pop_orig, by = c('age','country')) %>%
  select(country,age,life_expectancy,population) %>%
  mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
  group_by(country,age_group) %>%
  mutate(group_percent = population/sum(population),
         interim = life_expectancy * group_percent) %>%
  summarise(YLL = sum(interim)) 

#ggplot(YLL_FINAL[YLL_FINAL$country %in% setting_list,]) + geom_col(aes(x=age_group,y=YLL,fill=as.factor(country)),position="dodge")
#_______________________________________________________________________________

#Step Four: apply discounting using continuous approach, as per larson et al.
if (discounting_rate >0){YLL_FINAL$YLL = (1/discounting_rate)*(1-exp(-discounting_rate*YLL_FINAL$YLL ))}
#_______________________________________________________________________________

YLL_FINAL %>% filter(country %in% setting_list & age_group %in% c('0 to 4'))
#discounting == 0 -> 5.4 times 60 to 69
# country age_group   YLL
# 1 FJI     0 to 4     67.4
# 2 IDN     0 to 4     67.3
# 3 PNG     0 to 4     66.0
# 4 TLS     0 to 4     69.0

#discounting == 0.03 -> 2.5 times 60 to 69
# country age_group   YLL
# 1 FJI     0 to 4     28.9
# 2 IDN     0 to 4     28.9
# 3 PNG     0 to 4     28.7
# 4 TLS     0 to 4     29.1

#discounting == 0.05 -> 1.9 times 60 to 69
# country age_group   YLL
# 1 FJI     0 to 4     19.3
# 2 IDN     0 to 4     19.3
# 3 PNG     0 to 4     19.3
# 4 TLS     0 to 4     19.4

YLL_FINAL %>% filter(country %in% setting_list & age_group %in% c('60 to 69'))
#discounting == 0 
# country age_group   YLL
# 1 FJI     60 to 69   13.2
# 2 IDN     60 to 69   13.1
# 3 PNG     60 to 69   13.6
# 4 TLS     60 to 69   14.3

#discounting == 0.03
# country age_group   YLL
# 1 FJI     60 to 69   10.9
# 2 IDN     60 to 69   10.8
# 3 PNG     60 to 69   11.1
# 4 TLS     60 to 69   11.7

#discounting == 0.05
# country age_group   YLL
# 1 FJI     60 to 69   9.66
# 2 IDN     60 to 69   9.62
# 3 PNG     60 to 69   9.85
# 4 TLS     60 to 69  10.2 
################################################################################

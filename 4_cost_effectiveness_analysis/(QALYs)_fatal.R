require(readr); require(ggplot2); require(tidyverse)

discounting_rate = 0.03

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
         age = AgeGrp)
rm(UN_pop_est)

# to_plot = pop_orig %>%
#   filter(ISO3_code %in% setting_list) %>%
#   group_by(ISO3_code) %>%
#   mutate(prop = population/sum(population))
# ggplot(to_plot) + geom_point(aes(x=age,y=prop,color=as.factor(ISO3_code)))
#_______________________________________________________________________________


#Step Two: load life expectancy at age X
#"The average number of remaining years of life expected by a hypothetical cohort of individuals alive at age x 
# who would be subject during the remaining of their lives to the mortality rates of a given period."
# https://population.un.org/wpp/Download/Standard/Mortality/
load(file = paste(gsub("4_cost_effectiveness_analysis","",getwd()),
                  "1_inputs/UN_world_population_prospects/UN_lifeExpect_est.Rdata",sep=''))

UN_lifeExpect_est = UN_lifeExpect_est %>%
  rename(life_expectancy = ex,
         age = AgeGrp)

# ggplot(UN_lifeExpect_est[UN_lifeExpect_est$ISO3_code %in% setting_list,]) +
#   geom_point(aes(x=age,y=life_expectancy,color=as.factor(ISO3_code)),position="dodge")
#_______________________________________________________________________________


#Step Three: convert life expectancy to QALYs using HRQoL estimates
raw <- read.csv("2_inputs/age_specific_HRQoL_v2.csv",header=TRUE) 

HRQoL = raw %>%
  left_join(UN_lifeExpect_est, by = c("age")) %>%
  select(ISO3_code,age,HRQoL,life_expectancy) %>%
  filter(ISO3_code %in% setting_list)

workshop = data.frame()
for (this_setting in unique(HRQoL$ISO3_code)){ # for each setting
  for (this_age in unique(HRQoL$age)){ # for each age
    
    #how many years of life left at this age in this setting?
    this_workshop = HRQoL %>% filter(ISO3_code == this_setting &
                                       age == this_age)
    
    #create window of ages we are looking at
    this_workshop_window = HRQoL %>% 
      filter(ISO3_code == this_setting &
               age > this_age &
               age <= (this_age + ceiling(this_workshop$life_expectancy))) 
    
    #apply discounting (if>0) to each subsequent year of life
    if (discounting_rate>0){
      this_workshop_window = this_workshop_window %>%
        mutate(discounting_year = age - this_age,
               discounting_multiplier = 1/(1+discounting_rate)^discounting_year,
               HRQoL = HRQoL*discounting_multiplier) %>%
        select(-discounting_year,-discounting_multiplier)
      
    }
    
    #integer component of years of life left, e.g., if life expectancy 68.8 years -> 68 years
    full = this_workshop_window %>% filter(ISO3_code == this_setting &
                                             age > this_age &
                                             age <= (this_age + floor(this_workshop$life_expectancy)))
    full = sum(full$HRQoL)
    
    #decimal component of years of life left, e.g., if life expectancy 68.8 years -> 0.8 years
    partial = this_workshop_window %>% filter(ISO3_code == this_setting &
                                                age == (this_age + ceiling(this_workshop$life_expectancy)))
    partial = partial$HRQoL*(this_workshop$life_expectancy - floor(this_workshop$life_expectancy))
    if(length(partial) == 0){partial = 0}
    
    this_row = data.frame(age=this_age,
                          ISO3_code = this_setting,
                          life_expectancy = this_workshop$life_expectancy,
                          QALY = full+partial)
    
    workshop = rbind(workshop,this_row)
  }
}
# ggplot(workshop) + geom_line(aes(x=age,y=life_expectancy)) + 
#   geom_line(aes(x=age,y=QALY)) + 
#   facet_grid(ISO3_code ~.)
#_______________________________________________________________________________


#Step Four: calculate YLL per model age group weighted by pop dn
QALYs_fatal = workshop %>%
  left_join(pop_orig, by = c('age','ISO3_code')) %>%
  select(ISO3_code,age,QALY,population) %>%
  mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
  group_by(ISO3_code,age_group) %>%
  mutate(group_percent = population/sum(population),
         interim = QALY * group_percent) %>%
  summarise(QALYs = sum(interim),.groups="keep") 

#ggplot(QALYs_fatal[QALYs_fatal$ISO3_code %in% setting_list,]) + geom_col(aes(x=age_group,y=YLL,fill=as.factor(ISO3_code)),position="dodge")
#_______________________________________________________________________________

#Step Five: apply discounting using continuous approach, as per larson et al.
#if (discounting_rate >0){QALYs_fatal$YLL = (1/discounting_rate)*(1-exp(-discounting_rate*QALYs_fatal$YLL ))}
#_______________________________________________________________________________

QALYs_fatal %>% filter(ISO3_code %in% setting_list & age_group %in% c('0 to 4'))
#discounting == 0 -> 5.4 times 60 to 69
# ISO3_code age_group   YLL
# 1 FJI     0 to 4     67.4
# 2 IDN     0 to 4     67.3
# 3 PNG     0 to 4     66.0
# 4 TLS     0 to 4     69.0
# ISO3_code age_group QALYs -> 6 times 60 to 69
# 1 FJI       0 to 4     59.8
# 2 IDN       0 to 4     59.7
# 3 PNG       0 to 4     58.7
# 4 TLS       0 to 4     61.0

#discounting == 0.03 -> 2.5 times 60 to 69
# ISO3_code age_group   YLL
# 1 FJI     0 to 4     28.9
# 2 IDN     0 to 4     28.9
# 3 PNG     0 to 4     28.7
# 4 TLS     0 to 4     29.1
# ISO3_code age_group QALYs -> 3 times 60 to 69
# 1 FJI       0 to 4     26.3
# 2 IDN       0 to 4     26.3
# 3 PNG       0 to 4     26.1
# 4 TLS       0 to 4     26.4

#discounting == 0.05 -> 1.9 times 60 to 69
# ISO3_code age_group   YLL
# 1 FJI     0 to 4     19.3
# 2 IDN     0 to 4     19.3
# 3 PNG     0 to 4     19.3
# 4 TLS     0 to 4     19.4
# ISO3_code age_group QALYs -> 2 times 60 to 69
# 1 FJI       0 to 4     17.8
# 2 IDN       0 to 4     17.8
# 3 PNG       0 to 4     17.8
# 4 TLS       0 to 4     17.8

QALYs_fatal %>% filter(ISO3_code %in% setting_list & age_group %in% c('60 to 69'))
#discounting == 0 
# ISO3_code age_group   YLL
# 1 FJI     60 to 69   13.2
# 2 IDN     60 to 69   13.1
# 3 PNG     60 to 69   13.6
# 4 TLS     60 to 69   14.3
# ISO3_code age_group QALYs
# 1 FJI       60 to 69   10.3
# 2 IDN       60 to 69   10.3
# 3 PNG       60 to 69   10.6
# 4 TLS       60 to 69   11.2

#discounting == 0.03
# ISO3_code age_group   YLL
# 1 FJI     60 to 69   10.9
# 2 IDN     60 to 69   10.8
# 3 PNG     60 to 69   11.1
# 4 TLS     60 to 69   11.7
# ISO3_code age_group QALYs
# 1 FJI       60 to 69   8.41
# 2 IDN       60 to 69   8.37
# 3 PNG       60 to 69   8.60
# 4 TLS       60 to 69   8.96

#discounting == 0.05
# ISO3_code age_group   YLL
# 1 FJI     60 to 69   9.66
# 2 IDN     60 to 69   9.62
# 3 PNG     60 to 69   9.85
# 4 TLS     60 to 69  10.2 
# ISO3_code age_group QALYs
# 1 FJI       60 to 69   7.42
# 2 IDN       60 to 69   7.38
# 3 PNG       60 to 69   7.56
# 4 TLS       60 to 69   7.83
################################################################################

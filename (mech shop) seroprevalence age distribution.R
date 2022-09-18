### This script takes existing seroprevalence studies and estimates the seroprevalence in 2022 for our model age groups
### This work needs to be re-run every time the age groups in the model change

### Supplementary Materials description:
# We utilised a cross-sectional, nationally representative, age-stratified seroprevalence survey of Sierra Leone from March 2021. 
# This study estimated an overall seroprevalence of 2.6% (95% CI: 1.9% to 3.4%) after the first two waves in Sierra Leone [27]. 
# We could not identify any published seroprevalence studies in Papua New Guinea, or wider Melanesia (as of 24/01/2022). The closest 
# available seroprevalence survey reported 9.9% seroprevalence in the rural city of Jombang, East Java, Indonesia during June-December
# 2020 [28]. We expect the seroprevalence in Jombang to be higher than in Papua New Guinea due to higher population density. 

# Both the Sierra Leonan and Indonesian seroprevalence studies were conducted during the circulation of wild-type COVID-19. We multiplied 
# their estimates of seroprevalence by a factor of 3.3 to estimate seroprevalence after delta outbreaks, as per Indian data [29]. Hence,
# we estimated the initial proportion of individuals with infection-derived immunity to be 10% (0-25% sensitivity analysis) in Sierra Leone, 
# and 30% (0-50% sensitivity analysis) in Papua New Guinea. We conducted sensitivity analysis using broad intervals owing to large uncertainty 
# in this parameter due to the emergence of omicron, time since these seroprevalence surveys, and the effects of waning immunity.

#Age-structure to this infection-derived immunity was includes as per the Barrie et al.’s   age-stratified results in Sierra Leone (Table S6) [27].
#For Papua New Guinea, we applied the age distribution of cases in Papua (the Indonesian district) to the population estimate of seroprevalence (Table S7) [28, 30]. 

### Original work done in C:\Users\u6044061\Documents\PhD\Research\2_scarce_COVID_vaccine_supply\3_parameter_estimation\seroprevalence weighting by case distribution.xlsx

### Note: have included LB and UB in case of sensitivity analysis
#_________________________________________________________________________________________________________________________________________________________________________

seroprev_raw = read.csv("1_inputs/seroprev_raw.csv",header=TRUE)

###(1/3) SLE #########################################################################################################
setting = "SLE"

#(A) select setting specific seroprevalence and split by overall/age-specific
SLE = seroprev_raw[seroprev_raw$setting == setting,]
SLE_overall = SLE[SLE$age_group == "overall",]
SLE_age = SLE[!(SLE$age_group == "overall"),]
names(SLE_age)[names(SLE_age) == 'age_group'] <- 'agegroup_SERO' #change column name

#(B) examine underlying population size of these age groupings
SLE_pop <- pop_orig[pop_orig$country == "SLE",]
underlying_age_grouping <- c(0,9,19,39,59,110)
SLE_pop <- SLE_pop %>%
  mutate(agegroup_SERO = cut(age,breaks = underlying_age_grouping, include.lowest = T, labels = SLE_age$agegroup_SERO),
         agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  group_by(agegroup_MODEL) %>%
  mutate(model_group_percent = population/sum(population))
pop_SERO = aggregate(SLE_pop$population, by=list(category= SLE_pop$agegroup_SERO), FUN=sum)
colnames(pop_SERO) = c('agegroup_SERO','pop')
pop_SERO = pop_SERO %>% mutate(percentage = pop/sum(pop_SERO$pop))


#(C) check that age-specific aligns with pop-level
check = SLE_age %>% left_join(pop_SERO) %>% select(agegroup_SERO,seroprev,percentage) %>%
  mutate(interim = seroprev*percentage)
sum(check$interim) # = 2.22 NOT reported 2.6 -> adjust!

#(D) adjust age-specific to align with population estimate
factor = SLE_overall$seroprev/sum(check$interim)
factor # = 1.169
SLE_age$seroprev = SLE_age$seroprev * factor #CHECKED: re-runing (C) gives 2.6 :)

#(E) translate to model age groups
workshop = SLE_pop %>% left_join(SLE_age) %>% 
  mutate(interim = seroprev*model_group_percent)
workshop = aggregate(workshop$interim, by=list(category= workshop$agegroup_MODEL), FUN=sum)
colnames(workshop) = c('agegroup','seroprev')

#CHECK
pop_MODEL = aggregate(SLE_pop$population, by=list(category= SLE_pop$agegroup_MODEL), FUN=sum)
colnames(pop_MODEL) = c('agegroup','pop')
check = workshop %>% left_join(pop_MODEL) %>% mutate(interim = seroprev * pop /sum(pop_MODEL$pop))
sum(check$interim) #=2.6

#SAVE
SLE_2021 = workshop %>% mutate(year = 2021, setting = "SLE")

#(F) multiply to achieve 2022 estimates
est_2022 = 40.5
factor = est_2022/sum(check$interim)
SLE_2022 = SLE_2021 %>% mutate(seroprev = seroprev*factor,
                               year = 2022)

#CHECK
check = SLE_2022 %>% left_join(pop_MODEL) %>% mutate(interim = seroprev * pop /sum(pop_MODEL$pop))
sum(check$interim) #=2.6
#__________________________________________________________________________________________________



###(2/3) PNG ######################################################################################
setting = "PNG"

#(A) select setting specific seroprevalence and split by overall/age-specific
PNG = seroprev_raw[seroprev_raw$setting == setting,]
PNG_overall = PNG[PNG$age_group == "overall",]
PNG_age = PNG[!(PNG$age_group == "overall"),]
names(PNG_age)[names(PNG_age) == 'age_group'] <- 'agegroup_SERO' #change column name
#NOTE: we do not have age-specific seroprevalence, but we do have age-specific case % (a crude approximation)

#(B) examine underlying population size of these age groupings
PNG_pop <- pop_orig[pop_orig$country == "PNG",]
underlying_age_grouping <- c(0,5,18,30,45,59,110)
PNG_pop <- PNG_pop %>%
  mutate(agegroup_SERO = cut(age,breaks = underlying_age_grouping, include.lowest = T, labels = PNG_age$agegroup_SERO),
         agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  group_by(agegroup_MODEL) %>%
  mutate(model_group_percent = population/sum(population))
pop_SERO = aggregate(PNG_pop$population, by=list(category= PNG_pop$agegroup_SERO), FUN=sum)
colnames(pop_SERO) = c('agegroup_SERO','pop')
pop_SERO = pop_SERO %>% mutate(percentage = pop/sum(pop_SERO$pop))

#(C) crude estimate of age-specific seroprev by *%cases/%pop 
PNG_age = PNG_age %>% left_join(pop_SERO) %>%
  mutate(seroprev = seroprev/percentage * PNG_overall$seroprev)

#(D) check that age-specific aligns with pop-level
check = PNG_age %>% left_join(pop_SERO) %>% select(agegroup_SERO,seroprev,percentage) %>%
  mutate(interim = seroprev*percentage)
sum(check$interim) # = 9.89 

#(D) adjust age-specific to align with population estimate
factor = PNG_overall$seroprev/sum(check$interim)
factor # = 1.001001
PNG_age$seroprev = PNG_age$seroprev * factor #CHECKED: re-runing (C) EXACTLY 9.9

#(E) translate to model age groups
workshop = PNG_pop %>% left_join(PNG_age) %>% 
  mutate(interim = seroprev*model_group_percent)
workshop = aggregate(workshop$interim, by=list(category= workshop$agegroup_MODEL), FUN=sum)
colnames(workshop) = c('agegroup','seroprev')

#CHECK
pop_MODEL = aggregate(PNG_pop$population, by=list(category= PNG_pop$agegroup_MODEL), FUN=sum)
colnames(pop_MODEL) = c('agegroup','pop')
check = workshop %>% left_join(pop_MODEL) %>% mutate(interim = seroprev * pop /sum(pop_MODEL$pop))
sum(check$interim) #=2.6

#SAVE
PNG_2021 = workshop %>% mutate(year = 2021, setting = "PNG")

#(F) multiply to achieve 2022 estimates
est_2022 = 30
factor = est_2022/sum(check$interim)
PNG_2022 = PNG_2021 %>% mutate(seroprev = seroprev*factor,
                               year = 2022)

#CHECK
check = PNG_2022 %>% left_join(pop_MODEL) %>% mutate(interim = seroprev * pop /sum(pop_MODEL$pop))
sum(check$interim) #=2.6
#__________________________________________________________________________________________________




###(3/3) All together now
# seroprev_FINAL = crossing(year = c(2021,2022),
#                           setting = c('SLE','PNG'),
#                           age_group = age_group_labels)

seroprev_FINAL = rbind(SLE_2021,SLE_2022,PNG_2021,PNG_2022)
seroprev_FINAL = seroprev_FINAL %>% select(year,setting,agegroup,seroprev)
names(seroprev_FINAL)[names(seroprev_FINAL) == 'agegroup'] <- 'age_group' #change column name


#CHECK - plot against previous
grid.arrange(ggplot() + geom_point(data=seroprev_FINAL,aes(x=age_group,y=seroprev,color=setting)),
             ggplot() + geom_point(data=seroprev,aes(x=age_group,y=seroprev,color=setting)),
             nrow=2)

#OVERWRITE previous
seroprev = seroprev_FINAL
save(seroprev, file = "1_inputs/seroprev.Rdata")

rm(check,workshop,seroprev_raw,factor,est_2022,
   pop_MODEL,pop_SERO,
   PNG_2021,PNG_2022,PNG_age,PNG_pop,PNG_overall,
   SLE_2021,SLE_2022,SLE_age,SLE_pop,SLE_overall)





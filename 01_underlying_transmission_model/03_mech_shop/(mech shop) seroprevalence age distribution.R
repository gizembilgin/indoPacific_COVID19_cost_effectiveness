### This (mech shop) takes existing seroprevalence studies and estimates the seroprevalence for our model age groups.
### This work needs to be re-run every time the age groups in the model change.
### Seroprevalence is used ONLY when fitting the model.
### Creates: seroprev.
#_________________________________________________________________________________________________________________________________________________________________________

seroprev_raw = read.csv("01_inputs/seroprevalence_RAW.csv",header=TRUE)

this_setting = "IDN"


#(A) select setting specific seroprevalence and split by overall/age-specific
workshop = seroprev_raw[seroprev_raw$setting == this_setting,]
workshop_overall = workshop[workshop$age_group == "overall",]
workshop_age = workshop %>%
  filter(age_group != "overall") %>%
  rename(agegroup_SERO = age_group) %>%
  select(age_start,agegroup_SERO,seroprev)


#(B) examine underlying population size of these age groupings
pop_workshop <- pop_setting_orig %>%
  mutate(agegroup_SERO = cut(age,breaks = c(unique(workshop_age$age_start),110), include.lowest = T, labels = workshop_age$agegroup_SERO),
         agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  group_by(agegroup_MODEL) %>%
  mutate(model_group_percent = population/sum(population)) %>%
  left_join(workshop_age,by='agegroup_SERO') %>%
  mutate(seroprev = seroprev/100 * model_group_percent) %>%
  group_by(agegroup_MODEL) %>%
  summarise(seroprev = sum(seroprev)) %>%
  select(agegroup_MODEL,seroprev) 

# #CHECK
# ggplot(pop_workshop) + geom_col(aes(x=age_group,y=seroprev))
# ggplot(workshop_age) + geom_col(aes(x=agegroup_SERO,y=seroprev))


#SAVE
seroprev = pop_workshop %>% 
  rename(age_group = agegroup_MODEL) %>%
  mutate(year = 2021, setting = this_setting) %>%
  select(year,setting,age_group,seroprev)

save(seroprev, file = paste("01_inputs/seroprev_",this_setting,".Rdata",sep=''))





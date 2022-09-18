#GOAL: this to be passive of all age-value transformations

raw = read.csv("1_inputs/model_param_raw.csv",header=TRUE)
underlying_age_grouping <- c(0,9,19,29,39,49,59,69,110)

setting_list = c('SLE','PNG')
save = data.frame()

for (i in 1:length(setting_list)){
  setting = setting_list[i]
  
  pop_RAW =  pop_orig[pop_orig$country == setting,]
  pop_RAW <- pop_RAW %>%
    mutate(agegroup_RAW = cut(age,breaks = underlying_age_grouping, include.lowest = T, labels = unique(raw$age_group)),
           agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
    ungroup() %>%
    group_by(agegroup_MODEL) %>%
    mutate(model_group_percent = population/sum(population))
  
  raw = raw %>% mutate(agegroup_RAW = age_group)
  workshop = pop_RAW %>% left_join(raw) %>% 
    mutate(interim = model_group_percent * value)
  workshop = aggregate(workshop$interim, by=list(category = workshop$param,workshop$agegroup_MODEL),FUN=sum)
  colnames(workshop) = c('param','agegroup','value')
  
  save = rbind(save,workshop)
}

param_age = aggregate(save$value, by=list(category = save$param,save$agegroup),FUN=mean)
colnames(param_age) = c('param','agegroup','value')

save(param_age, file = "1_inputs/param_age.Rdata")


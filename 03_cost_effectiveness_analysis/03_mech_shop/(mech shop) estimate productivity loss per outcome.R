#### This script creates productivity_loss_reference_df.Rdata - age-specific 
#### estimates of productivity losses per mild, severe, critical, fatal episode 
#### of COVID-19. Productivity losses are a combination of days off work due to 
#### illness and premature death. To save model run_time we have pre-calculated 
#### and saved productivity losses by outcome for discounting rates 0-10%.

discounting_rate_list = seq(0,0.1,by = 0.01)
age_groups_num <- c(0,4,9,17,29,44,59,69,110)
age_group_labels <- c("0 to 4","5 to 9","10 to 17","18 to 29","30 to 44","45 to 59","60 to 69","70 to 100")
LIST_CEA_settings <- c("PNG","TLS","FJI","IDN")


####  Importing key datasets ###################################################
load(file = paste0(gsub("4_cost_effectiveness_analysis","",getwd()),"1_inputs/UN_world_population_prospects/UN_pop_est.Rdata"))
load(file = paste0(gsub("4_cost_effectiveness_analysis","",getwd()),"1_inputs/UN_world_population_prospects/UN_lifeExpect_est.Rdata"))
labour_force_participation_RAW <- read.csv("02_inputs/Labour force participation rate by age - ILO modelled estimates.csv",header=TRUE)
annual_output_per_worker_RAW <- read.csv("02_inputs/Output per worker (GDP constant 2015 US $).csv",header=TRUE)
age_specific_return_to_work_RAW <- read.csv("02_inputs/age_specific_return_to_work.csv",header=TRUE)
return_to_work_RAW <- read.csv("02_inputs/return_to_work.csv",header=TRUE)

UN_lifeExpect_est <- UN_lifeExpect_est %>%
  rename(life_expectancy = ex,
         age = AgeGrp,
         setting = ISO3_code) %>%
  mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels))  %>%
  select(setting,age_group,age,life_expectancy)

# ggplot(UN_lifeExpect_est[UN_lifeExpect_est$setting %in% setting_list,]) +
#   geom_point(aes(x=age,y=life_expectancy,color=as.factor(setting)),position="dodge")
################################################################################




####  Estimating expected daily earnings per age group #########################
### Note: two assumptions due to lack of granular data:
### (1) We assume equal wages across lifetime
### (2) We assume no individuals under the age of 15 work
### Note 2: workshop_IMF from (mech shop) cost estimation

## "Labour force participation rate by sex and age -- ILO modelled estimates, Nov. 2022 (%)"
labour_force_participation <- labour_force_participation_RAW %>%
  filter(time == 2021 & #max year
           sex.label == "Sex: Total" &
           classif1.label %in% c("Age (Aggregate bands): 15-24", "Age (Aggregate bands): 25-54", "Age (Aggregate bands): 55-64","Age (Aggregate bands): 65+")) %>%
  mutate(setting = case_when(
    ref_area.label == "Fiji" ~ "FJI",
    ref_area.label == "Indonesia" ~ "IDN",
    ref_area.label == "Papua New Guinea" ~ "PNG",
    ref_area.label == "Timor-Leste" ~ "TLS"     
  )) %>%
  select(-ref_area.label, - obs_status.label,-sex.label,-source.label,-indicator.label,-time) %>%
  rename(agegroup_RAW = classif1.label,
         value = obs_value)

#Translate to model age groups
underlying_age_grouping <- c(15,24,54,64,110)

pop_RAW <- UN_pop_est   %>%
  rename(age = AgeGrp,
         setting = ISO3_code) %>%
  select(setting,age,PopTotal) %>%
  filter(setting %in% labour_force_participation$setting) %>%
  mutate(agegroup_RAW = cut(age,breaks = underlying_age_grouping, include.lowest = T, labels = unique(labour_force_participation$agegroup_RAW)),
         agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  group_by(agegroup_MODEL,setting) %>%
  mutate(model_group_percent = PopTotal/sum(PopTotal)) 

labour_force_participation <- pop_RAW %>% 
  left_join(labour_force_participation,by = join_by(setting, agegroup_RAW)) %>% 
  mutate(interim = model_group_percent * value) %>%
  group_by(setting,agegroup_MODEL) %>%
  summarise(value = sum(interim,na.rm=TRUE), .groups = "keep") %>%
  rename(age_group = agegroup_MODEL)
#_______________________________________________________________________________


#"Output per worker (GDP constant 2015 US $) -- ILO modelled estimates, Nov. 2022"
annual_output_per_worker <- annual_output_per_worker_RAW %>%
  mutate(setting = case_when(
    ref_area.label == "Fiji" ~ "FJI",
    ref_area.label == "Indonesia" ~ "IDN",
    ref_area.label == "Papua New Guinea" ~ "PNG",
    ref_area.label == "Timor-Leste" ~ "TLS"     
  )) %>%
  filter(is.na(setting) == FALSE &
           time == 2022) %>%
  select(-indicator.label, -source.label, - ref_area.label)

#Inflate to 2022 USD!
adj_factor <- workshop_IMF %>% 
  filter(year %in% c(2015,2022) & variable == "GDP_deflator") %>%
  mutate(year = paste("year",year,sep="_")) %>%
  pivot_wider(names_from = year,values_from = value) %>%
  mutate(adj = year_2022/year_2015) %>%
  select(ISO3_code,adj) %>%
  rename(setting = ISO3_code)

annual_output_per_worker <- annual_output_per_worker %>% 
  left_join(adj_factor, by = "setting") %>%
  mutate(value = obs_value * adj) %>%
  select(setting,value) 
#_______________________________________________________________________________


#Hence, expected daily earnings (2022 USD)
expected_daily_earnings <- annual_output_per_worker %>%
  mutate(daily_earning = value/365) %>%
  select(-value) %>%
  left_join(labour_force_participation, by = "setting") %>%
  mutate(daily_earning = daily_earning * value/100) %>%
  select(setting,age_group,daily_earning)

save(expected_daily_earnings, file = "02_inputs/expected_daily_earnings.Rdata")
################################################################################




#### Productivity loss due to premature death ##################################
# I.e., expected remaining lifetime earnings at each age
remaining_lifetime_earnings <- expected_daily_earnings %>%
  left_join(UN_lifeExpect_est, by = c("age_group","setting")) %>%
  mutate(yearly_earning = daily_earning * 365)

workshop = data.frame()
for (this_setting in unique(remaining_lifetime_earnings$setting)){ # for each setting
  for (this_age in unique(remaining_lifetime_earnings$age)){ # for each age
    
    #how many years of life left at this age in this setting?
    this_workshop <- remaining_lifetime_earnings %>% 
      filter(setting == this_setting &
               age == this_age)
    
    #create window of ages we are looking at
    this_workshop_window <- remaining_lifetime_earnings %>% 
      filter(setting == this_setting &
               age > this_age &
               age <= (this_age + ceiling(this_workshop$life_expectancy))) 
    
    for (toggle_discounting_rate in discounting_rate_list){
      #apply discounting (if>0) to each subsequent year of life
      if (toggle_discounting_rate>0){
        this_workshop_window <- this_workshop_window %>%
          mutate(discounting_year = age - this_age,
                 discounting_multiplier = 1/(1+toggle_discounting_rate)^discounting_year,
                 yearly_earning = yearly_earning*discounting_multiplier) %>%
          select(-discounting_year,-discounting_multiplier)
        
      }
      
      #integer component of years of life left, e.g., if life expectancy 68.8 years -> 68 years
      full <- this_workshop_window %>% filter(setting == this_setting &
                                               age > this_age &
                                               age <= (this_age + floor(this_workshop$life_expectancy)))
      full = sum(full$yearly_earning)
      
      #decimal component of years of life left, e.g., if life expectancy 68.8 years -> 0.8 years
      partial <- this_workshop_window %>% filter(setting == this_setting &
                                                  age == (this_age + ceiling(this_workshop$life_expectancy)))
      partial = partial$yearly_earning*(this_workshop$life_expectancy - floor(this_workshop$life_expectancy))
      if(length(partial) == 0){partial = 0}
      
      this_row <- data.frame(age=this_age,
                            setting = this_setting,
                            discounting_rate = toggle_discounting_rate,
                            life_expectancy = this_workshop$life_expectancy,
                            remaining_lifetime_earnings = full+partial)
      
      workshop <- rbind(workshop,this_row)
    }
  }
}
ggplot(workshop) +
  geom_line(aes(x=age,y=remaining_lifetime_earnings,color=as.factor(discounting_rate))) +
  facet_grid(setting ~.)


# calculate remaining_lifetime_earnings per model age group weighted by pop dn
remaining_lifetime_earnings <- workshop %>%
  left_join(pop_RAW, by = c("age","setting")) %>%
  select(setting,discounting_rate,age,remaining_lifetime_earnings,PopTotal) %>%
  mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
  group_by(setting,discounting_rate,age_group) %>%
  mutate(group_percent = PopTotal/sum(PopTotal),
         interim = remaining_lifetime_earnings * group_percent) %>%
  summarise(remaining_lifetime_earnings = sum(interim),.groups="keep") 

options(scipen = 1000)
ggplot(remaining_lifetime_earnings[remaining_lifetime_earnings$setting == "IDN" & remaining_lifetime_earnings$discounting_rate %in% c(0,0.05),]) + 
  geom_col(aes(x=age_group,y=remaining_lifetime_earnings,fill=as.factor(setting)),position="dodge") +
  facet_grid(discounting_rate ~.)
################################################################################



#### Productivity loss due to time off work ####################################

### Step one: importing age-specific data on time between testing positive and  returning to work 
# Visualise
this_month = 1
# age_group_order = c("56 to 64", "46 to 55", "36 to 45", "26 to 35", "18 to 25")
# age_specific_return_to_work_RAW$age_group = factor(age_specific_return_to_work_RAW$age_group, levels = age_group_order)
# ggplot(age_specific_return_to_work_RAW[age_specific_return_to_work_RAW$month == this_month,]) + 
#   geom_point(aes(x=point_estimate,y=age_group, color=as.factor(sex))) + 
#   facet_grid(admission ~.) +
#   theme_bw()
# qualitatively the trend by sex, admission and age looks the same across each month
# DECISION: to use the trend for the first month after positive test since these values were largest and contained no zero values

# Visualize by sex
age_specific_RTW <- age_specific_return_to_work_RAW %>%
  filter(month == 1) %>%
  group_by(admission,sex) %>%
  mutate(ratio = point_estimate/min(point_estimate))
# ggplot(age_specific_RTW) + 
#      geom_point(aes(x=point_estimate,y=age_group, color=as.factor(sex))) + 
#      facet_grid(admission ~.) +
#      theme_bw()
# ggplot(age_specific_RTW) + 
#   geom_point(aes(x=ratio,y=age_group, color=as.factor(sex))) + 
#   facet_grid(admission ~.) +
#   theme_bw()

# Collapse sex
age_specific_RTW <- age_specific_return_to_work_RAW %>%
  filter(month == 1) %>%
  group_by(admission,sex) %>%
  mutate(ratio = point_estimate/min(point_estimate)) %>%
  group_by(admission,age_group,month) %>%
  summarise(ratio = mean(ratio)) %>%
  rename(agegroup_RAW = age_group)
# ggplot(age_specific_RTW) + 
#   geom_point(aes(x=ratio,y=agegroup_RAW)) + 
#   facet_grid(admission ~.) +
#   theme_bw()

# Translate to % pop adm rate per age group
#NB: algebra is hand written in 1_derivation folder pdf dated 2023/06/29
underlying_age_grouping <- c(18,25,35,45,55,64)

pop_RAW <- UN_pop_est   %>%
  rename(age = AgeGrp) %>%
  select(ISO3_code,age,PopTotal) %>%
  filter(ISO3_code %in% LIST_CEA_settings) %>%
  mutate(agegroup_RAW = cut(age,breaks = underlying_age_grouping, include.lowest = T, labels = unique(age_specific_return_to_work_RAW$age_group))) %>%
  filter(is.na(agegroup_RAW) == FALSE) %>%
  ungroup() %>%
  group_by(ISO3_code) %>%
  mutate(raw_group_percent = PopTotal/sum(PopTotal)) %>%
  group_by(ISO3_code,agegroup_RAW) %>%
  summarise(raw_group_percent = sum(raw_group_percent))

workshop <- pop_RAW %>% 
  left_join(age_specific_RTW,by = join_by(agegroup_RAW),
            relationship = "many-to-many") %>% #applying same relationship to multiple settings
  mutate(interim = raw_group_percent * ratio) %>%
  group_by(ISO3_code,admission) %>%
  summarise(ratio_pop = 1/sum(interim))

age_specific_RTW <- age_specific_RTW %>%
  left_join(workshop, by = c("admission"),
            relationship = "many-to-many") %>% #applying same relationship to multiple settings
  mutate(ratio_pop = ratio_pop * ratio) %>%
  select(-ratio)
# ggplot(age_specific_RTW[age_specific_RTW$ISO3_code == "IDN",]) + 
#   geom_point(aes(x=ratio_pop,y=agegroup_RAW)) + 
#   facet_grid(admission ~.) +
#   theme_bw()
#CHECKED: still sums to one
# age_specific_RTW %>%
#   left_join(pop_RAW, by = c("ISO3_code","agegroup_RAW")) %>%
#   mutate(interim = ratio_pop * raw_group_percent) %>%
#   group_by(admission,ISO3_code) %>%
#   summarise(one = sum(interim))

#Translate to model age groups
pop_RAW <- UN_pop_est   %>%
  rename(age = AgeGrp) %>%
  select(ISO3_code,age,PopTotal) %>%
  filter(ISO3_code %in% LIST_CEA_settings ) %>%
  mutate(agegroup_RAW = cut(age,breaks = underlying_age_grouping, include.lowest = T, labels = unique(age_specific_return_to_work_RAW$age_group)),
         agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  filter(is.na(agegroup_RAW) == FALSE) %>%
  group_by(agegroup_MODEL,ISO3_code) %>%
  mutate(model_group_percent = PopTotal/sum(PopTotal))

workshop <- pop_RAW %>% 
  left_join(age_specific_RTW,by = join_by(agegroup_RAW,ISO3_code),
            relationship = "many-to-many") %>% #applying same relationship to multiple settings
  mutate(interim = model_group_percent * ratio_pop) %>%
  group_by(agegroup_MODEL,ISO3_code,admission) %>%
  summarise(ratio_pop = sum(interim,na.rm=TRUE), .groups = "keep") %>%
  rename(age_group = agegroup_MODEL)
ggplot(workshop[workshop$ISO3_code == "IDN",]) + 
  geom_point(aes(x=ratio_pop,y=age_group)) + 
  facet_grid(admission ~.) +
  theme_bw()
#CHECKED: visually with previous age group values

#Add missing age groups (although likely not included since low workforce participation)
for (this_age_group in age_group_labels[!(age_group_labels %in% unique(workshop$age_group))]){
  if (this_age_group %in% c("0 to 4",    "5 to 9" ,   "10 to 17" )){
    rows <- workshop %>%
      filter(age_group == "18 to 29") %>%
      mutate(age_group = this_age_group)
  } else{
    rows <- workshop %>%
      filter(age_group ==  "60 to 69") %>%
      mutate(age_group = this_age_group)
  }
  workshop <- rbind(workshop,rows)
}
age_specific_RTW <- workshop
#_______________________________________________________________________________


### Step two: importing  data on time between testing positive and  returning to work by admission type
#CHECKED: visualise imported values
# ggplot(return_to_work_RAW) +
#   geom_line(aes(x=weeks,y=proportion,color=as.factor(patient_type))) +
#   ylim(0,1)
#_______________________________________________________________________________


### Step three: bring together two sources of data on return to work
return_to_work_COLLATED <- return_to_work_RAW %>%
  mutate(admission = case_when(
    patient_type  %in% c("ICU","inpatient") ~ "Y",
    patient_type == "not admitted" ~ "N"
  )) %>%
  left_join(age_specific_RTW, by = "admission", relationship = "many-to-many") %>%
  mutate(not_returning = (1-proportion) * ratio_pop) %>%
  mutate(not_returning = case_when(
    not_returning > 1 ~ 1,
    TRUE ~ not_returning
  )) %>%
  rename(setting = ISO3_code)

# ggplot(return_to_work_COLLATED[return_to_work_COLLATED$setting == "IDN" & !(return_to_work_COLLATED$age_group %in% c("0 to 4","5 to 9","10 to 17","70 to 100") ),]) +
#   geom_line(aes(x=weeks,y=not_returning,color=as.factor(patient_type))) +
#   ylim(0,1) +
#   facet_grid(age_group ~.)
#_______________________________________________________________________________


### Step four: calculate productivity loss in the first year
# Essentially integrating under the curve of % not returning to work * expected daily earnings at that age
first_year_loss <- return_to_work_COLLATED %>%
  select(setting,age_group,patient_type,weeks,not_returning)  %>%
  left_join(expected_daily_earnings, by = c("age_group","setting")) %>%
  mutate(interim = case_when(
    weeks == min(return_to_work_COLLATED$weeks) ~ 7*daily_earning*min(return_to_work_COLLATED$weeks) * not_returning,
    weeks == max(return_to_work_COLLATED$weeks) ~ 7*daily_earning*(52-max(return_to_work_COLLATED$weeks + 1)) * not_returning,
    TRUE ~ 7*daily_earning*not_returning
  ))
# ggplot(first_year_loss[first_year_loss$setting == "IDN" & !(first_year_loss$age_group %in% c("0 to 4","5 to 9","10 to 17","70 to 100") ),]) +
#   geom_line(aes(x=weeks,y=interim,color=as.factor(patient_type))) +
#   facet_grid(age_group ~.) +
#   xlim(5,24) +
#   ylim(0,150)

first_year_loss <- first_year_loss %>%
  group_by(setting,age_group,patient_type) %>%
  summarise(first_year = sum(interim))
# ggplot(first_year_loss) + 
#   geom_col(aes(x=age_group,y=first_year,fill=as.factor(patient_type)),position = "dodge") + 
#   facet_grid(setting ~.)
#_______________________________________________________________________________


### Step five: calculate loss in subsequent years of life
expected_yearly_earnings <- expected_daily_earnings %>%
  left_join(UN_lifeExpect_est, by = c("age_group","setting")) %>%
  mutate(yearly_earning = daily_earning * 365)

workshop = data.frame()
for (this_setting in unique(expected_yearly_earnings$setting)){
  for (this_age in unique(expected_yearly_earnings$age)){
    
    this_age_group = unique(UN_lifeExpect_est$age_group[UN_lifeExpect_est$age == this_age])
    
    #how many years of life left at this age in this setting?
    this_workshop <- expected_yearly_earnings %>% 
      filter(setting == this_setting &
               age == this_age)
    
    #what proportion will 'never' return to work at this age?
    this_RTW <- return_to_work_COLLATED %>%
      filter(setting == this_setting &
               weeks == max(return_to_work_COLLATED$weeks) &
               age_group == this_age_group) %>%
      select(setting,patient_type,not_returning)
    
    #create window of ages we are looking at
    this_workshop_window <- expected_yearly_earnings %>% 
      filter(setting == this_setting &
               age > this_age &
               age <= (this_age + ceiling(this_workshop$life_expectancy))) 
    
    this_workshop_window <- this_workshop_window %>%
      left_join(this_RTW, by = "setting",
                relationship = "many-to-many") %>% # many to many as many outcomes (ICU, inpatient, not admitted)
      mutate(yearly_earning = yearly_earning * not_returning)

    for (toggle_discounting_rate in discounting_rate_list){
      #apply discounting (if>0) to each subsequent year of life
      if (toggle_discounting_rate>0){
        this_workshop_window_2 <- this_workshop_window %>%
          mutate(discounting_year = age - this_age,
                 discounting_multiplier = 1/(1+toggle_discounting_rate)^discounting_year,
                 yearly_earning = yearly_earning*discounting_multiplier) %>%
          select(-discounting_year,-discounting_multiplier)
        
      } else{
        this_workshop_window_2 <- this_workshop_window 
      }
      
      #integer component of years of life left, e.g., if life expectancy 68.8 years -> 68 years
      full <- this_workshop_window_2 %>% filter(setting == this_setting &
                                               age > this_age &
                                               age <= (this_age + floor(this_workshop$life_expectancy))) %>%
        group_by(patient_type) %>%
        summarise(full = sum(yearly_earning)) 
      
      #decimal component of years of life left, e.g., if life expectancy 68.8 years -> 0.8 years
      partial <- this_workshop_window_2 %>% filter(setting == this_setting &
                                                  age == (this_age + ceiling(this_workshop$life_expectancy)))%>%
        group_by(patient_type) %>%
        mutate(partial = yearly_earning*(this_workshop$life_expectancy - floor(this_workshop$life_expectancy)))  %>%
        select(patient_type,age,setting,life_expectancy,partial)
      if (nrow(partial) == length(unique(return_to_work_RAW$patient_type)) ){
        this_row <- full %>%
          left_join(partial, by = c("patient_type")) %>%
          mutate(productivity_lost = full+partial,
                 discounting_rate = toggle_discounting_rate,
                 age = this_age,
                 setting = this_setting) %>%
          select(-full,-partial)
      } else if (nrow(partial) == 0){
        this_row <- full  %>%
          mutate(productivity_lost = full,
                 discounting_rate = toggle_discounting_rate,
                 age = this_age,
                 setting = this_setting) %>%
          select(-full)
      }

      workshop <- bind_rows(workshop,this_row)
    }
  }
}

# calculate remaining_lifetime_earnings per model age group weighted by pop dn
pop_RAW <- UN_pop_est   %>%
  rename(age = AgeGrp) %>%
  select(ISO3_code,age,PopTotal) %>%
  filter(ISO3_code %in% unique(labour_force_participation$setting)) %>%
  mutate(agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  group_by(agegroup_MODEL,ISO3_code) %>%
  mutate(model_group_percent = PopTotal/sum(PopTotal)) %>%
  rename(setting = ISO3_code)

productivity_lost <- workshop %>%
  left_join(pop_RAW, by = c("age","setting")) %>%
  select(setting,age,discounting_rate,patient_type,productivity_lost,PopTotal) %>%
  mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
  group_by(setting,discounting_rate,patient_type,age_group) %>%
  mutate(group_percent = PopTotal/sum(PopTotal),
         interim = productivity_lost * group_percent) %>%
  summarise(productivity_lost = sum(interim),.groups="keep") %>%
  ## combine expected remaining and first year losses
  left_join(first_year_loss, by = c("setting","age_group","patient_type"), 
            relationship = "many-to-many") %>% #applying same relationship to multiple discounting rates
  mutate(productivity_lost = productivity_lost + first_year) %>%
  select(-first_year)


## visualise
#this_discounting = 0.0
# ggplot(productivity_lost[productivity_lost$discounting_rate == this_discounting,]) + 
#   geom_col(aes(x=age_group,y=productivity_lost,fill=as.factor(patient_type)),position = "dodge") + 
#   facet_grid(setting ~.)

#this_discounting = 0.05
# ggplot(productivity_lost[productivity_lost$discounting_rate == this_discounting,]) + 
#   geom_col(aes(x=age_group,y=productivity_lost,fill=as.factor(patient_type)),position = "dodge") + 
#   facet_grid(setting ~.)
################################################################################



#### Combine and save dataset
# colnames(remaining_lifetime_earnings)
# colnames(productivity_lost)

remaining_lifetime_earnings <- remaining_lifetime_earnings %>%
  rename(productivity_loss = remaining_lifetime_earnings) %>%
  mutate(outcome = "death")

productivity_lost <- productivity_lost %>%
  rename(productivity_loss = productivity_lost,
         outcome = patient_type) %>%
  #translating outcomes in this way to account for lower access to care in the Pacific/SE Asia compared to Denmark
  mutate(outcome = case_when(
    outcome == "ICU" ~ "critical_disease",
    outcome == "inpatient" ~ "severe_disease",
    outcome == "not admitted" ~ "mild"
    ))

productivity_loss_reference_df <- bind_rows(remaining_lifetime_earnings,productivity_lost)
save(productivity_loss_reference_df, file = "02_inputs/productivity_loss_reference_df.Rdata")

to_plot <- productivity_loss_reference_df %>%
  filter(setting == "IDN"
         #& outcome != "death"
         )
to_plot$age_group <- factor(to_plot$age_group,levels = age_group_labels)
ggplot(to_plot[to_plot$discounting_rate %in% c(0,0.03,0.05),]) +
  geom_col(aes(x=age_group,y=productivity_loss,fill=as.factor(discounting_rate)),position = "dodge") +
  facet_grid(outcome ~.)

print <- productivity_loss_reference_df %>% 
  filter(discounting_rate %in% c(0,0.03,0.05)) %>%
  pivot_wider(names_from = setting,values_from = productivity_loss)
write.csv(print, file = "x_results/Table_S2.csv")
################################################################################
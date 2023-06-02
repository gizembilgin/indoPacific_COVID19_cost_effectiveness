
load(file = paste(gsub("4_cost_effectiveness_analysis","",getwd()),"1_inputs/UN_world_population_prospects/UN_pop_est.Rdata",sep=''))
labour_force_participation_RAW <- read.csv("2_inputs/Labour force participation rate by age - ILO modelled estimates.csv",header=TRUE)
annual_output_per_worker_RAW <- read.csv("2_inputs/Output per worker (GDP constant 2015 US $).csv",header=TRUE)
#workshop_IMF from (mech shop) cost estimation



#"Labour force participation rate by sex and age -- ILO modelled estimates, Nov. 2022 (%)"
labour_force_participation <- labour_force_participation_RAW %>%
  filter(time == 2021 & #max year
           sex.label == "Sex: Total" &
           classif1.label %in% c("Age (Aggregate bands): 15-24", "Age (Aggregate bands): 25-54", "Age (Aggregate bands): 55-64","Age (Aggregate bands): 65+")) %>%
  mutate(ISO3_code = case_when(
    ref_area.label == "Fiji" ~ "FJI",
    ref_area.label == "Indonesia" ~ "IDN",
    ref_area.label == "Papua New Guinea" ~ "PNG",
    ref_area.label == "Timor-Leste" ~ "TLS"     
  )) %>%
  select(-ref_area.label, - obs_status.label,-sex.label,-source.label,-indicator.label,-time) %>%
  rename(agegroup_RAW = classif1.label,
         value = obs_value)

underlying_age_grouping <- c(15,24,54,64,110)

pop_RAW =  UN_pop_est   %>%
  rename(age = AgeGrp) %>%
  select(ISO3_code,age,PopTotal) %>%
  filter(ISO3_code %in% labour_force_participation$ISO3_code) %>%
  mutate(agegroup_RAW = cut(age,breaks = underlying_age_grouping, include.lowest = T, labels = unique(labour_force_participation$agegroup_RAW)),
         agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  group_by(agegroup_MODEL,ISO3_code) %>%
  mutate(model_group_percent = PopTotal/sum(PopTotal)) 

labour_force_participation = pop_RAW %>% 
  left_join(labour_force_participation,by = join_by(ISO3_code, agegroup_RAW)) %>% 
  mutate(interim = model_group_percent * value) %>%
  group_by(ISO3_code,agegroup_MODEL) %>%
  summarise(value = sum(interim,na.rm=TRUE), .groups = TRUE) %>%
  rename(age_group = agegroup_MODEL)



#"Output per worker (GDP constant 2015 US $) -- ILO modelled estimates, Nov. 2022"
annual_output_per_worker <- annual_output_per_worker_RAW %>%
  mutate(ISO3_code = case_when(
    ref_area.label == "Fiji" ~ "FJI",
    ref_area.label == "Indonesia" ~ "IDN",
    ref_area.label == "Papua New Guinea" ~ "PNG",
    ref_area.label == "Timor-Leste" ~ "TLS"     
  )) %>%
  filter(is.na(ISO3_code) == FALSE &
           time == 2022) %>%
  select(-indicator.label, -source.label, - ref_area.label)

adj_factor = workshop_IMF %>% 
  filter(year %in% c(2015,2022) & variable == "GDP_deflator") %>%
  mutate(year = paste("year",year,sep="_")) %>%
  pivot_wider(names_from = year,values_from = value) %>%
  mutate(adj = year_2022/year_2015) %>%
  select(ISO3_code,adj)
  
annual_output_per_worker = annual_output_per_worker %>% 
  left_join(adj_factor, by = "ISO3_code") %>%
  mutate(value = obs_value * adj) %>%
  select(ISO3_code,value)
  


#Expected daily earnings (2022 USD)
result_daily = annual_output_per_worker %>%
  mutate(daily_earning = value/365) %>%
  select(-value) %>%
  left_join(labour_force_participation, by = "ISO3_code") %>%
  mutate(daily_earning = daily_earning * value/100) %>%
  select(ISO3_code,age_group,daily_earning)



#Expected remaining lifetime earnings

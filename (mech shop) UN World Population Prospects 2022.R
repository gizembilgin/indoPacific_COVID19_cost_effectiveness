
### (1/3) Population by age (single years)
workshop <- read.csv(paste(rootpath,"inputs/UN_world_population_prospects/WPP2022_PopulationBySingleAgeSex_Medium_1950-2021.csv",sep=''), header = TRUE)
workshop2 <- read.csv(paste(rootpath,"inputs/UN_world_population_prospects/WPP2022_PopulationBySingleAgeSex_Medium_2022-2100.csv",sep=''), header = TRUE)
workshop = rbind(workshop,workshop2); rm(workshop2)
workshop = workshop %>%
  filter(LocTypeName == "Country/Area") %>% 
  filter(Time >= 2021 & Time<2025) %>%
  select(ISO3_code,Location,Time,AgeGrp,PopFemale,PopTotal) %>%
  mutate(PopFemale = PopFemale * 1000,
         PopTotal = PopTotal * 1000)
UN_pop_est = workshop
save(UN_pop_est,file = "1_inputs/UN_world_population_prospects/UN_pop_est.Rdata")


### (2/3) Life expectancy
workshop <- read.csv(paste(rootpath,"inputs/UN_world_population_prospects/WPP2022_Life_Table_Complete_Medium_Both_1950-2021.csv",sep=''), header = TRUE)
workshop2 <- read.csv(paste(rootpath,"inputs/UN_world_population_prospects/WPP2022_Life_Table_Complete_Medium_Both_2022-2100.csv",sep=''), header = TRUE)
workshop = rbind(workshop,workshop2); rm(workshop2)
workshop = workshop %>%
  filter(LocTypeName == "Country/Area") %>% 
  filter(Time >= 2021 & Time<2025) %>%
  select(ISO3_code,Location,Time,AgeGrpStart,ex) %>%
  rename(AgeGrp = AgeGrpStart)
UN_lifeExpect_est = workshop
save(UN_lifeExpect_est,file = "1_inputs/UN_world_population_prospects/UN_lifeExpect_est.Rdata")


### (3/3) ASFR
workshop <- read.csv(paste(rootpath,"inputs/UN_world_population_prospects/WPP2022_Fertility_by_Age1.csv",sep=''), header = TRUE)
workshop = workshop %>%
  filter(LocTypeName == "Country/Area") %>% 
  filter(Time >= 2021 & Time<2025) %>%
  filter(Variant ==  "Medium") %>%
  select(ISO3_code, Location, Time, AgeGrp, ASFR) %>%
  mutate(ASFR = ASFR / 1000)
UN_ASFR_est = workshop
save(UN_ASFR_est,file = "1_inputs/UN_world_population_prospects/UN_ASFR_est.Rdata")

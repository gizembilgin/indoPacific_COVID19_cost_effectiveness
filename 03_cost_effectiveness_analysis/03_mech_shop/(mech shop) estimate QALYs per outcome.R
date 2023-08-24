### This script creates QALY_estimates.Rdata - age-specific estimates of QALYs lost per mild, severe, critical, fatal episode of COVID-19

discounting_rate_list = seq(0,0.1,by = 0.01)
age_groups_num <- c(0,4,9,17,29,44,59,69,110)
age_group_labels <- c("0 to 4","5 to 9","10 to 17","18 to 29","30 to 44","45 to 59","60 to 69","70 to 100")
setting_list = c("FJI","IDN","PNG","TLS")


### PART ONE: loading QALY estimates############################################
## non-fatal QALYS 
# Step One: load population distribution by age
load(file = paste0(gsub("03_cost_effectiveness_analysis","01_underlying_transmission_model",getwd()),
                   "/01_inputs/UN_world_population_prospects/UN_pop_est.Rdata"))

pop_orig <- UN_pop_est %>% 
  rename(population = PopTotal,
         age = AgeGrp) %>%
  filter(ISO3_code %in% setting_list)
rm(UN_pop_est)
#_______________________________________________________________________________

# Step Two: load non-fatal QALYs by severity and age
raw <- read.csv("02_inputs/age_severity_specific_QALYs.csv",header=TRUE) 
#ggplot(raw) + geom_line(aes(x=age,y=QALYs)) + facet_grid(severity ~ ., scale = "free_y")
#_______________________________________________________________________________

# Step Three: calculate QALYs per model age group weighted by pop distribution
QALYs_nonFatal <- raw %>%
  left_join(pop_orig, by = c("age"), relationship = "many-to-many") %>%
  select(severity,ISO3_code,age,QALYs,population) %>%
  mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
  group_by(ISO3_code,severity,age_group) %>%
  mutate(group_percent = population/sum(population),
         interim = QALYs * group_percent) %>%
  summarise(QALYs = sum(interim),.groups="keep") 
#ggplot(QALYs_nonFatal) + geom_col(aes(x=age_group,y=QALYs,fill=as.factor(ISO3_code)),position="dodge") + facet_grid(severity ~ ., scale = "free_y")
#______________________________________________________________________________


## fatal QALYS ###########################################################
# Step One: load life expectancy at age X
#"The average number of remaining years of life expected by a hypothetical cohort of individuals alive at age x 
# who would be subject during the remaining of their lives to the mortality rates of a given period."
# https://population.un.org/wpp/Download/Standard/Mortality/
load(file = paste0(gsub("03_cost_effectiveness_analysis","01_underlying_transmission_model",getwd()),
                   "/01_inputs/UN_world_population_prospects/UN_lifeExpect_est.Rdata"))

UN_lifeExpect_est <- UN_lifeExpect_est %>%
  rename(life_expectancy = ex,
         age = AgeGrp)

# ggplot(UN_lifeExpect_est[UN_lifeExpect_est$ISO3_code %in% setting_list,]) +
#   geom_point(aes(x=age,y=life_expectancy,color=as.factor(ISO3_code)),position="dodge")
#_______________________________________________________________________________

# Step Two: convert life expectancy to QALYs using HRQoL estimates
raw <- read.csv("02_inputs/age_specific_HRQoL_v2.csv",header=TRUE) 

HRQoL <- raw %>%
  left_join(UN_lifeExpect_est, by = c("age")) %>%
  select(ISO3_code,age,HRQoL,life_expectancy) %>%
  filter(ISO3_code %in% setting_list)

workshop = data.frame()
for (this_setting in unique(HRQoL$ISO3_code)){ # for each setting
  for (this_age in unique(HRQoL$age)){ # for each age
    
    #how many years of life left at this age in this setting?
    this_workshop <- HRQoL %>% filter(ISO3_code == this_setting &
                                       age == this_age)
    
    #create window of ages we are looking at
    this_workshop_window <- HRQoL %>% 
      filter(ISO3_code == this_setting &
               age > this_age &
               age <= (this_age + ceiling(this_workshop$life_expectancy))) 
    
    for (toggle_discounting_rate in discounting_rate_list){
      #apply discounting (if>0) to each subsequent year of life
      if (toggle_discounting_rate>0){
        this_workshop_window <- this_workshop_window %>%
          mutate(discounting_year = age - this_age,
                 discounting_multiplier = 1/(1+toggle_discounting_rate)^discounting_year,
                 HRQoL = HRQoL*discounting_multiplier) %>%
          select(-discounting_year,-discounting_multiplier)
        
      }
      
      #integer component of years of life left, e.g., if life expectancy 68.8 years -> 68 years
      full <- this_workshop_window %>% filter(ISO3_code == this_setting &
                                               age > this_age &
                                               age <= (this_age + floor(this_workshop$life_expectancy)))
      full = sum(full$HRQoL)
      
      #decimal component of years of life left, e.g., if life expectancy 68.8 years -> 0.8 years
      partial <- this_workshop_window %>% filter(ISO3_code == this_setting &
                                                  age == (this_age + ceiling(this_workshop$life_expectancy)))
      partial = partial$HRQoL*(this_workshop$life_expectancy - floor(this_workshop$life_expectancy))
      if(length(partial) == 0){partial = 0}
      
      this_row <- data.frame(age=this_age,
                            ISO3_code = this_setting,
                            discounting_rate = toggle_discounting_rate,
                            life_expectancy = this_workshop$life_expectancy,
                            QALY = full+partial)
      
      workshop <- rbind(workshop,this_row)
    }
  }
}
ggplot(workshop[workshop$discounting_rate %in% c(0,0.03,0.05),]) + 
  geom_line(aes(x=age,y=life_expectancy,color=as.factor(ISO3_code))) +
  geom_line(aes(x=age,y=QALY,color=as.factor(ISO3_code)),linetype ="dashed") +
  facet_grid(discounting_rate ~.)
#_______________________________________________________________________________


# Step Three: calculate YLL per model age group weighted by pop dn
QALYs_fatal <- workshop %>%
  left_join(pop_orig, by = c("age","ISO3_code")) %>%
  select(ISO3_code,discounting_rate,age,QALY,population) %>%
  mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
  group_by(ISO3_code,discounting_rate,age_group) %>%
  mutate(group_percent = population/sum(population),
         interim = QALY * group_percent) %>%
  summarise(QALYs = sum(interim),.groups="keep") %>%
  mutate(severity = "fatal")
#ggplot(QALYs_fatal[QALYs_fatal$ISO3_code %in% setting_list,]) + geom_col(aes(x=age_group,y=YLL,fill=as.factor(ISO3_code)),position="dodge")
#_______________________________________________________________________________
################################################################################


## QALYs longCovid
prevalence_postSixMonths = 0.027
prevalence_upToSixMonths = 0.457
longCOVID_disability_weight = 0.051

QALYs_longCOVID <- QALYs_fatal %>%
  #for ongoing long COVID use fatal QALY estimates of YLL weighted by HRQoL and discounted in future years
  mutate(QALYs = QALYs * prevalence_postSixMonths*longCOVID_disability_weight,
         severity = "total_cases", #ASSUMPTION: all cases of COVID-19 can develop long COVID
         QALYs = QALYs + prevalence_upToSixMonths*longCOVID_disability_weight*0.5)

QALY_estimates <- rbind(QALYs_nonFatal,QALYs_fatal,QALYs_longCOVID) 
#_______________________________________________________________________________


## bring together QALYS ######################################################
QALY_estimates <- QALY_estimates %>%
  rename(setting = ISO3_code) %>%
  mutate(outcome = case_when(
    severity == "critical" ~ "critical_disease",
    severity ==  "mild" ~  "mild" ,
    severity == "severe" ~ "severe_disease",
    severity == "fatal"  ~ "death",
    TRUE ~ severity
  )) %>%
  ungroup() %>%
  select(-severity)
# ggplot(QALY_estimates) +
#   geom_col(aes(x=age_group,y=QALYs,fill=as.factor(setting)),position="dodge") +
#   theme_bw() +
#   labs(fill="")+
#   facet_grid(outcome ~ ., scale = "free_y")

save(QALY_estimates, file = "02_inputs/QALY_estimates.Rdata")
##############################################################################
##############################################################################
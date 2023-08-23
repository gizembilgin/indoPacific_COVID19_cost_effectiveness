### This (mech shop) creates point estimates of d = 3 (booster) heterologous combinations with Pfizer
# strains  = "omicron" (no booster doses till omicron in SLE)
# outcomes = "any_infection"       "death"               "severe_disease"      "symptomatic_disease"
# vaccine type =  "Pfizer"  (with previous primary doses in AZ, Sinovac or Pfizer)

### Creates: VE_booster_estimates
require(ggpubr); require(readr);require(ggplot2); require(tidyverse)



#####  Visualise estimates from IVAC living systematic review ##########4#########################################################################
VE_raw <- read.csv("1_inputs/VE_WHO_forest_plot.csv",header=TRUE)

#convert generic "mRNA" rows to Moderna and Pfizer
workshop = VE_raw  %>% filter(vaccine_type == 'mRNA')
moderna = workshop %>% mutate(vaccine_type = 'Moderna')
pfizer = workshop %>% mutate(vaccine_type = 'Pfizer')
VE_raw = rbind(VE_raw[VE_raw$vaccine_type != "mRNA",],moderna,pfizer) ; rm(moderna,pfizer)

workshop = VE_raw  %>% filter(primary_if_booster == 'mRNA')
moderna = workshop %>% mutate(primary_if_booster = 'Moderna')
pfizer = workshop %>% mutate(primary_if_booster = 'Pfizer')
VE_raw = rbind(VE_raw[VE_raw$primary_if_booster != "mRNA",],moderna,pfizer) ; rm(moderna,pfizer)

VE_raw = VE_raw %>% 
  mutate(primary_if_booster = case_when(dose<3 ~ vaccine_type,TRUE ~ primary_if_booster)) %>%
  select(strain, vaccine_type, primary_if_booster,dose, outcome,VE,lower_est,upper_est) 

#plot with confidence interval
this_strain = 'omicron'
dose_limit = 3
to_plot = VE_raw[VE_raw$strain == this_strain & VE_raw$dose >=dose_limit,]
plot_list = list()
for (i in 1:length(unique(to_plot$outcome))){
  outcome = unique(to_plot$outcome)[i]
  plot_list [[i]] <- ggplot(data=to_plot[to_plot$outcome==outcome,]) +
    #geom_pointrange(aes(x=VE,y=primary_if_booster_long,xmin=lower_est,xmax=upper_est,color=as.factor(vaccine_type),shape = as.factor(dose))) +
    geom_pointrange(aes(x=VE,y=primary_if_booster,xmin=lower_est,xmax=upper_est,color=as.factor(vaccine_type),shape = as.factor(dose))) +
    xlim(0,100) +
    theme_bw() +
    xlab("") +
    ylab("") +
    theme(text=element_text(size=10),
          plot.title=element_text(size=12))+
    labs(title=paste("VE against ",outcome,sep=""))
}
ggarrange(plot_list[[2]],plot_list[[1]],plot_list[[4]],plot_list[[3]],
                       common.legend = TRUE,
                       legend="bottom"
)
#_______________________________________________________________________________________________________________________________________________



#####  Clean estimates ##########################################################################################################################
#average across estimates from IVAC living systematic review
VE_estimates = VE_raw %>% 
  select(strain, vaccine_type, primary_if_booster,dose, outcome,VE) %>% 
  group_by(strain, vaccine_type, primary_if_booster, dose,outcome) %>%
  summarise(VE = sum(VE)/n())

#load dose 1 and 2, as previously decided
load(file = "1_inputs/VE_estimates_imputed.Rdata")
VE_estimates_imputed = VE_estimates_imputed %>% 
  select(strain, vaccine_type, dose, outcome,VE) %>%
  filter(dose<3)
VE_estimates = rbind(VE_estimates[VE_estimates$dose>2,],VE_estimates_imputed)  %>%
  mutate(primary_if_booster = case_when(dose<3 ~ vaccine_type,TRUE ~ primary_if_booster))

#force dose four >= dose three (if < likely due to eligible pop immunosuppressed)
VE_estimates = VE_estimates %>%
  pivot_wider(names_from = dose,
              names_prefix = "dose_",
              values_from = VE) %>%
  mutate(dose_4 = case_when((is.na(dose_4) == FALSE & is.na(dose_3) == FALSE & dose_4<dose_3) ~ dose_3, TRUE ~ dose_4),
         dose_3 = case_when((is.na(dose_3) == FALSE & is.na(dose_2) == FALSE & dose_3<dose_2) ~ dose_2, TRUE ~ dose_3)) %>%
  pivot_longer(
    cols = c('dose_1','dose_2','dose_3','dose_4'),
    names_to = "dose",
    names_prefix = "dose_*",
    values_to = "VE"
  )

this_strain = 'delta'
to_plot = VE_estimates[VE_estimates$strain == this_strain,]
plot_list = list()
for (i in 1:length(unique(to_plot$outcome))){
  outcome = unique(to_plot$outcome)[i]
  plot_list [[i]] <- ggplot(data=to_plot[to_plot$outcome==outcome,]) + 
    geom_point(aes(x=VE,y=primary_if_booster,color=as.factor(vaccine_type),shape = as.factor(dose))) +
    xlim(0,100) +
    theme_bw() + 
    xlab("") + 
    ylab("") + 
    theme(text=element_text(size=10), 
          plot.title=element_text(size=12))+
    labs(title=paste("VE against ",outcome,sep=""))
}
ggarrange(plot_list[[2]],plot_list[[1]],plot_list[[4]],plot_list[[3]],
          common.legend = TRUE,
          legend="bottom"
)
#plot_list
#checking 4>=3>=2>=1
#_______________________________________________________________________________________________________________________________________________



#####  Calculate ratios ################################################################################################################
booster_estimates = VE_estimates %>% filter(dose>2)

#(A/D) compare VE against death (where available) to VE against severe disease
death = booster_estimates %>%
  filter(outcome == "death") %>% 
  select(strain,vaccine_type,primary_if_booster,dose,VE) %>%
  rename(death = VE)
severe_disease = booster_estimates %>%
  filter(outcome == "severe_disease") %>% 
  select(strain,vaccine_type,primary_if_booster,dose,VE) %>%
  rename(severe_disease = VE)

workshop = death %>% 
  left_join(severe_disease, by = c("strain", "vaccine_type", "primary_if_booster","dose")) %>%
  mutate(ratio = severe_disease/death) %>% 
  filter(is.na(ratio) == FALSE)
workshop %>% ungroup() %>% summarise(mean = mean(ratio,na.rm=TRUE),
                                     sd = sd(ratio,na.rm=TRUE),
                                     count = sum(is.na(ratio) == FALSE))
# mean     sd count
#  0.959 0.0639     8
workshop %>% group_by(dose) %>%
  summarise(mean = mean(ratio,na.rm=TRUE),
            sd = sd(ratio,na.rm=TRUE),
            count = sum(is.na(ratio) == FALSE))
# dose  mean     sd
# 3     0.958  0.0688     7
# 4     0.972 NA          1

severe_disease_death_ratio = workshop %>% group_by(dose) %>%
  summarise(mean = mean(ratio,na.rm=TRUE))
#____________________

#(B/D) compare VE against any infection (where available) to symptomatic disease
symptomatic_disease = booster_estimates %>%
  filter(outcome == "symptomatic_disease") %>% 
  select(strain,vaccine_type,primary_if_booster,dose,VE) %>%
  rename(symptomatic_disease = VE)
any_infection = booster_estimates %>%
  filter(outcome == "any_infection") %>% 
  select(strain,vaccine_type,primary_if_booster,dose,VE) %>%
  rename(any_infection = VE)

workshop = symptomatic_disease %>% 
  left_join(any_infection, by = c("strain", "vaccine_type", "dose")) %>%
  mutate(ratio = any_infection/symptomatic_disease)
workshop %>% 
  ungroup() %>% 
  summarise(mean = mean(ratio,na.rm=TRUE),
                                     sd = sd(ratio,na.rm=TRUE),
                                     count = sum(is.na(ratio) == FALSE))
# mean    sd count
#  0.741 0.113     6
#all are dose 3 estimates for omicron

infection_sympt_ratio = workshop %>% 
  ungroup() %>% 
  summarise(mean = mean(ratio,na.rm=TRUE))
#_________________________


#(C/D) compare VE against omicron (where available) to delta
delta =  booster_estimates %>% ungroup() %>%
  filter(strain == "delta") %>% 
  select(outcome,vaccine_type,primary_if_booster,dose,VE)  %>%
  rename(delta = VE)
omicron =  booster_estimates %>% ungroup() %>%
  filter(strain == "omicron") %>% 
  select(outcome,vaccine_type,primary_if_booster,dose,VE)  %>%
  rename(omicron = VE)

workshop = delta %>% 
  left_join(omicron, by = c("outcome", "vaccine_type","primary_if_booster", "dose")) %>%
  mutate(ratio = omicron/delta)
workshop %>% ungroup() %>% summarise(mean = mean(ratio,na.rm=TRUE),
                       sd = sd(ratio,na.rm=TRUE))
# mean        sd
# 0.805 0.216
#only dose 3 estimates
workshop %>% group_by(outcome) %>%
  summarise(count = sum(is.na(ratio)==FALSE),
            mean = mean(ratio,na.rm=TRUE),
            sd = sd(ratio,na.rm=TRUE))
# outcome             count    mean      sd
# 1 any_infection           2   0.531  0.0374
# 2 death                   2   0.970  0.0540
# 3 severe_disease          2   0.913  0.0379
# 4 symptomatic_disease     0 NaN     NA    
delta_omicron_ratio = workshop %>% group_by(outcome) %>%
  summarise(mean = mean(ratio,na.rm=TRUE))
#NB: symptomatic disease missing
#_________________________


#(D/D) from same vaccine type (homologous combinations only)
dose_two = VE_estimates[VE_estimates$dose == 2,] %>% 
  select(strain,outcome,primary_if_booster,vaccine_type,VE) %>%
  rename(dose_two = VE)
dose_three = VE_estimates[VE_estimates$dose == 3,] %>% 
  select(strain,outcome,primary_if_booster,vaccine_type,VE) %>%
  rename(dose_three = VE)
dose_four = VE_estimates[VE_estimates$dose == 4,] %>% 
  select(strain,outcome,primary_if_booster,vaccine_type,VE) %>%
  rename(dose_four = VE)

#dose three to four
workshop = dose_three %>% 
  left_join(dose_four, by = c("strain", "outcome","primary_if_booster", "vaccine_type")) %>%
  mutate(ratio = dose_three/dose_four)
workshop %>% filter(is.na(ratio) == FALSE) %>%
  summarise(mean = mean(ratio,na.rm=TRUE),
            count = sum(is.na(ratio)),
                       sd = sd(ratio,na.rm=TRUE))
# strain  vaccine_type primary_if_booster  mean     sd
#   1 omicron Moderna      Moderna            0.853  0.158
# comparison only available for ONE! For some let's ASSUME first booster to second booster unless data available

# dose two to three
workshop = dose_three %>% 
  left_join(dose_two, by = c("strain", "outcome","primary_if_booster", "vaccine_type")) %>%
  mutate(ratio = dose_three/dose_two) %>% 
  filter(is.na(ratio) == FALSE)
workshop %>%
  summarise(mean = mean(ratio,na.rm=TRUE),
            sd = sd(ratio,na.rm=TRUE))
workshop %>% group_by(strain) %>%
  summarise(count = n(),
            mean = mean(ratio,na.rm=TRUE),
            sd = sd(ratio,na.rm=TRUE))
workshop %>% group_by(vaccine_type) %>%
  summarise(count = n(),
            mean = mean(ratio,na.rm=TRUE),
            sd = sd(ratio,na.rm=TRUE))
workshop %>% group_by(outcome) %>%
  summarise(count = n(),
            mean = mean(ratio,na.rm=TRUE),
            sd = sd(ratio,na.rm=TRUE))
dose_ratio = workshop %>%
  group_by(strain) %>%
  summarise(ratio = mean(ratio,na.rm=TRUE))
#_________________________
#_______________________________________________________________________________________________________________________________________________



#####  Impute missing estimates ################################################################################################################
#Step One: from outcome
#NB: only for omicron for sympt<->infect
VE_infect = VE_estimates %>%
  filter(outcome %in% c('any_infection','symptomatic_disease')) %>%
  pivot_wider(names_from = outcome,
              values_from = VE) %>%
  mutate(
    any_infection = case_when(
    is.na(any_infection) & is.na(symptomatic_disease) == FALSE & strain == 'omicron' ~ symptomatic_disease * infection_sympt_ratio$mean,
    TRUE ~ any_infection
  ),symptomatic_disease = case_when(
    is.na(symptomatic_disease) & is.na(any_infection) == FALSE & strain == 'omicron' ~ any_infection * 1/infection_sympt_ratio$mean,
    TRUE ~ symptomatic_disease
  )) %>%
  pivot_longer(
    cols = c('symptomatic_disease','any_infection'),
    names_to = "outcome",
    values_to = "VE"
  ) 


VE_severe_disease =  VE_estimates %>%
  filter(outcome %in% c('severe_disease','death')) %>%
  pivot_wider(names_from = outcome,
              values_from = VE) %>%
  left_join(severe_disease_death_ratio, by = 'dose') %>%
  mutate(
  severe_disease = case_when(
    is.na(severe_disease) & is.na(death) == FALSE  ~ death * mean,
    TRUE ~ severe_disease
  ),
  death = case_when(
    is.na(death) & is.na(severe_disease) == FALSE  ~ severe_disease * 1/mean,
    TRUE ~ death
  )) %>%
  pivot_longer(
    cols = c('death','severe_disease'),
    names_to = "outcome",
    values_to = "VE"
  ) %>% select(-mean)

VE_estimates = rbind(VE_severe_disease,VE_infect)


#Step Two: from strain
VE_estimates =  VE_estimates %>% 
  pivot_wider(names_from = strain,
              values_from = VE) %>%
  left_join(delta_omicron_ratio, by = 'outcome') %>%
  mutate(
  delta = case_when(
    is.na(delta) & is.na(omicron) == FALSE  & outcome != "symptomatic disease" & dose == 3  & (omicron * 1/mean)<100 ~ omicron * 1/mean,
    TRUE ~ delta
    ),
  omicron = case_when(
    is.na(omicron) & is.na(delta) == FALSE  & outcome != "symptomatic disease" & dose == 3 ~ delta * mean,
    TRUE ~ omicron
  )) %>%
  pivot_longer(
    cols = c('omicron','delta'),
    names_to = "strain",
    values_to = "VE"
  ) %>% 
  select(-mean)



#Step Three: from dose 2
VE_estimates = VE_estimates %>%
  left_join(dose_ratio, by = "strain") %>%
  pivot_wider(names_from = dose,
              names_prefix = "dose_",
              values_from = VE) %>%
  mutate(
    dose_3 = case_when(
      is.na(dose_3) == TRUE & (dose_2 * ratio < 100)~ dose_2 * ratio,
      TRUE ~ dose_3
    ),
    dose_4 = case_when(
      is.na(dose_4) == TRUE ~ dose_3,
      TRUE ~ dose_4
    )
  ) %>%
  pivot_longer(
    cols = c('dose_1','dose_2','dose_3','dose_4'),
    names_to = "dose",
    names_prefix = "dose_*",
    values_to = "VE"
  ) %>% 
  select(-ratio)
#_______________________________________________________________________________________________________________________________________________




#####  Save point estimate for booster doses ################################################################################
generate_source = na.omit(VE_raw) %>% mutate(combination = paste(strain,vaccine_type,primary_if_booster,dose,outcome)) %>% select(combination)

VE_booster_estimates = VE_estimates %>%
  filter(dose > 2) %>%
  mutate(
    vaccine_type_long = case_when(
      vaccine_type == "AstraZeneca" ~ "AstraZeneca Vaxzevria (ChAdOx1)",
      vaccine_type == "Johnson & Johnson" ~ "Johnson & Johnson Janssen (Ad26.COV2.S)",
      vaccine_type == "Moderna" ~ "Moderna Spikevax (mRNA-1273)",
      vaccine_type == "Pfizer" ~ "Pfizer-BioNTech Comirnaty (BNT162b2)",
      vaccine_type == "Sinopharm" ~ "Sinopharm BIBP vaccine",
      vaccine_type == "Sinovac" ~ "Sinovac Biotech CoronaVac"
    ),
    primary_if_booster_long = case_when(
      primary_if_booster == "AstraZeneca" ~ "AstraZeneca Vaxzevria (ChAdOx1)",
      primary_if_booster == "Johnson & Johnson" ~ "Johnson & Johnson Janssen (Ad26.COV2.S)",
      primary_if_booster == "Moderna" ~ "Moderna Spikevax (mRNA-1273)",
      primary_if_booster == "Pfizer" ~ "Pfizer-BioNTech Comirnaty (BNT162b2)",
      primary_if_booster == "Sinopharm" ~ "Sinopharm BIBP vaccine",
      primary_if_booster == "Sinovac" ~ "Sinovac Biotech CoronaVac"
    ),
    vaccine_mode = case_when(
      vaccine_type == 'Pfizer' ~ 'mRNA',
      vaccine_type == 'Moderna' ~ 'mRNA',
      vaccine_type == 'AstraZeneca' ~ 'viral_vector',
      vaccine_type == 'Sinopharm' ~ 'viral_inactivated',
      vaccine_type == 'Sinovac' ~ 'viral_inactivated',
      vaccine_type == 'Johnson & Johnson' ~ 'viral_vector'
    ),
    outcome_family = case_when(
      outcome %in% c('any_infection', 'symptomatic_disease') ~ 'acquisition',
      outcome %in% c('severe_disease', 'death') ~ 'severe_outcome'
      
  )) %>%
  select(strain,vaccine_type,primary_if_booster,outcome,dose,VE,vaccine_type_long,vaccine_mode,outcome_family) %>% 
  mutate(combination = paste(strain,vaccine_type,primary_if_booster,dose,outcome),
         source = case_when(combination %in% generate_source$combination ~ 'real',
                            TRUE ~ 'imputed')) %>%
  select(-combination)

if (nrow(VE_booster_estimates[VE_booster_estimates$VE>100,])>0){stop('VE booster dose pt est > 100%!')}


this_strain = 'omicron'
this_dose = 3
to_plot = VE_booster_estimates[VE_booster_estimates$strain == this_strain & VE_booster_estimates$dose == this_dose ,]
plot_list = list()
for (i in 1:length(unique(to_plot$outcome))){
  outcome = unique(to_plot$outcome)[i]
  plot_list [[i]] <- ggplot(data=to_plot[to_plot$outcome==outcome,]) +
    geom_point(aes(x=VE,y=primary_if_booster,color=as.factor(vaccine_type),shape  = as.factor(source))) +
    xlim(0,100) +
    scale_shape_manual(values = c(1,16))+
    theme_bw() +
    xlab("VE (%)") +
    ylab("primary schedule") + 
    labs(color="booster vaccine type", shape = "source") +
    theme(text=element_text(size=10),
          plot.title=element_text(size=12))+
    labs(title=paste("VE against ",outcome,sep=""))
}
ggarrange(plot_list[[2]],plot_list[[1]],plot_list[[4]],plot_list[[3]],
                       common.legend = TRUE,
                       legend="bottom"
)

save(VE_booster_estimates,file = "1_inputs/VE_booster_estimates.Rdata")




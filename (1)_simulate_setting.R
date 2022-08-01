### This program will configure the model to the study population
### i.e. for this study to either Sierra Leone (SL) or Papua New Guinea (PNG)
###
### (1) Age structure of population
### (2) Contact patterns
### (3) Live updates of # active cases/recovered
### (4) Live updates of # vaccinated (if useful, otherwise .csv files)
### (5) Proxy estimate of TTIQ/PHSM i.e. NPI

### COMEBACK - still need importation
if (setting == "SLE"){setting_long = "Sierra Leone"}
#______________________________________________________________________________________________________________________________________

### (1/5) Age structure of population
##(A/B) Without risk groups
#NOTE: this program has been configured so that the age_groups can be modified on a whim,
# however, some other programs aren't as flexible
age_groups = c(0,4,17,29,44,59,69,110)
age_group_labels = c('0 to 4','5 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100')

num_age_groups = J = length(age_group_labels)          
age_group_order = data.frame(age_group = age_group_labels, age_group_num = seq(1:J))

pop_orig <- read.csv(paste(rootpath,"inputs/pop_estimates.csv",sep=''), header=TRUE)
pop_setting_orig <- pop_orig[pop_orig$country == setting,]
pop_setting <- pop_setting_orig %>%
  mutate(agegroup = cut(age,breaks = age_groups, include.lowest = T,labels = age_group_labels)) 
pop_setting <- aggregate(pop_setting$population, by=list(category=pop_setting$agegroup), FUN=sum)
colnames(pop_setting) <-c('age_group','pop')
pop <- pop_setting$pop
#write.csv(pop_setting,file = 'x_results/pop_scrap.csv')
#_______________________________________________________


##(B/B) With risk groups
pop_risk_group_dn = pop_setting %>% mutate(risk_group = 'general_public')
risk_group_labels = c('general_public')

if (num_risk_groups>1){
  #COMEBACK_RISK these are DUMMY VALUES (~DHS fertility rate)
  risk_dn = read.csv('1_inputs/risk_group.csv')
  
  if(risk_group_name %in% c('adults_with_comorbidities')){
    risk_dn = risk_dn[risk_dn$risk_group_name == risk_group_name,]
  } else if (risk_group_name %in% c('pregnant_women')){
    load(file = "1_inputs/prevalence_pregnancy.Rdata")
    risk_dn = prevalence_pregnancy
  } else {stop('risk_group_name not a valid value')}
  
  risk_dn = risk_dn %>%
    select(age_group,prop)
  risk_group_labels[2] = risk_group_name
  
  pop_high_risk = pop_setting %>% left_join(risk_dn, by = "age_group") %>%
    mutate(risk_group = risk_group_name,
           pop = round(pop*prop)) %>% 
    select(risk_group,age_group,pop)
  pop_general_public   = pop_setting %>% left_join(risk_dn, by = "age_group") %>%
    mutate(risk_group = 'general_public',
           pop = round(pop*(1-prop))) %>% 
    select(risk_group,age_group,pop)
  pop_risk_group_dn = rbind(pop_general_public,pop_high_risk)
  
  pop_risk_group = aggregate(pop_risk_group_dn$pop, by = list(pop_risk_group_dn$risk_group), FUN = sum)
  colnames(pop_risk_group) = c('risk_group','pop')
} else{
  risk_dn = crossing(age_group=age_group_labels,
                     prop = 1)
}
if (round(sum(pop_risk_group_dn$pop)) != sum(pop)){stop('(1) simulate setting line 69')}


#______________________________________________________________________________________________________________________________________



### (2/5) Contact patterns of population
#CONFIRMATION FROM MARK JIT: .Rdata files are more up to date on GitHub (Prem et al. 2021 paper)
#(A/C) load contact matrix
load(file = paste(rootpath,"inputs/contact_all.Rdata",sep=''))
contact_matrix_setting <- contact_all[[setting]]
Prem_et_al_age_list <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)
colnames(contact_matrix_setting) <- Prem_et_al_age_list; rownames(contact_matrix_setting) <- Prem_et_al_age_list

#(B/C) calculate age weightings
Prem_et_al_age_num <- c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,110)
pop_Prem <- pop_setting_orig %>%
  mutate(agegroup_PREM = cut(age,breaks = Prem_et_al_age_num, include.lowest = T, labels = Prem_et_al_age_list),
         agegroup_MODEL = cut(age,breaks = age_groups, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  group_by(agegroup_MODEL) %>%
  mutate(model_group_percent = population/sum(population)) %>%
  ungroup() %>%
  group_by(agegroup_PREM) %>%
  mutate(prem_group_percent = population/sum(population)) %>%
  select(age,agegroup_PREM,agegroup_MODEL,model_group_percent,prem_group_percent)
sum_1 = aggregate(pop_Prem$model_group_percent, by=list(category= pop_Prem$agegroup_MODEL,pop_Prem$agegroup_PREM), FUN=sum)
colnames(sum_1) = c('agegroup_MODEL','agegroup_PREM','model_group_percentage')
sum_2 = aggregate(pop_Prem$prem_group_percent, by=list(category= pop_Prem$agegroup_MODEL,pop_Prem$agegroup_PREM), FUN=sum)
colnames(sum_2) = c('agegroup_MODEL','agegroup_PREM','prem_group_percentage')
pop_Prem = sum_1 %>% left_join(sum_2, by = c("agegroup_MODEL", "agegroup_PREM"))
#View(pop_Prem)


#(C/C) construct contact matrix by our age groups
#create skeleton
workshop_cm1  <- data.frame(matrix(0,ncol = 16,nrow=length(age_groups)-1))
rownames(workshop_cm1) <- age_group_labels
colnames(workshop_cm1) <- Prem_et_al_age_list

workshop_cm2 <- data.frame(matrix(0,ncol = length(age_group_labels),nrow=length(age_group_labels)))
rownames(workshop_cm2) <- age_group_labels
colnames(workshop_cm2) <- age_group_labels

#age weighting of contacts
for (i in 1:(length(age_group_labels))){
  workshop = pop_Prem[pop_Prem$agegroup_MODEL == age_group_labels[i],]
  for (j in 1:nrow(workshop)){
    workshop_cm1[i,] = workshop_cm1[i,]  + contact_matrix_setting[row.names(contact_matrix_setting) == workshop$agegroup_PREM[j]] * workshop$model_group_percentage[j]
  }
} 
  
#sum across collapsed age bands
for (i in 1:J){
  workshop = pop_Prem[pop_Prem$agegroup_MODEL == age_group_labels[i],]
  for (j in 1:nrow(workshop)){
    workshop_cm2[,i] = workshop_cm2[,i]  + workshop_cm1[colnames(workshop_cm1) == workshop$agegroup_PREM[j]]  * workshop$prem_group_percentage[j]
  }
}  
#CHECK:rowSums(workshop_cm1) == rowSums(workshop_cm2)

contact_matrix = workshop_cm2

rm( contact_all, contact_matrix_setting, sum_1, sum_2,
   Prem_et_al_age_list, Prem_et_al_age_num, pop_Prem,
   workshop_cm2, workshop_cm1,i, j)
#______________________________________________________________________________________________________________________________________



###(3/5) Live updates of cases
# 
# setting="IDN"
# if (setting == "PNG"){setting_long = "Papua New Guinea"
# } else if (setting == "SLE"){setting_long = "Sierra Leone"
# } else if (setting == "IDN"){setting_long = "Indonesia"}

workshop_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
workshop_cases = workshop_cases[workshop_cases$'Country/Region' == setting_long,]
workshop_cases <- workshop_cases %>%
  pivot_longer(
    cols = 5:ncol(workshop_cases) ,
    names_to = 'date',
    values_to = 'cases'
  ) 
workshop_cases <- workshop_cases %>%
  mutate(date =as.Date(workshop_cases$date, "%m/%d/%y"))

# workshop_deaths <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
# workshop_deaths = workshop_deaths[workshop_deaths$'Country/Region' == setting_long,]
# workshop_deaths <- workshop_deaths %>%
#   pivot_longer(
#     cols = 5:ncol(workshop_deaths) ,
#     names_to = 'date',
#     values_to = 'deaths'
#   )
# workshop_deaths <- workshop_deaths %>%
#   mutate(date =as.Date(workshop_deaths$date, "%m/%d/%y"))

workshop <- workshop_cases %>%
  #left_join(workshop_deaths) %>%
  #select(date, cases,deaths) %>%
  mutate(new = cases - lag(cases),
         #case_fatality_ratio = deaths/cases,
         rolling_average = (new + lag(new,default=0) + lag(new,n=2,default=0)+lag(new,n=3,default=0)
                            +lag(new,n=4,default=0)+lag(new,n=5,default=0)+lag(new,n=6,default=0))/7)

ggplot() + 
  geom_point(data=workshop,aes(x=date,y=rolling_average),na.rm=TRUE) + 
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("daily cases") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

# ggplot() + 
#   geom_point(data=workshop,aes(x=date,y=case_fatality_ratio),na.rm=TRUE) + 
#   xlab("") + 
#   scale_x_date(date_breaks="1 month", date_labels="%b") +
#   ylab("case fatality (%)") +
#   theme_bw() + 
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         panel.border = element_blank(),
#         axis.line = element_line(color = 'black'))

#ASSUMPTION: cases remain active for 14 days
case_history = workshop %>%
  mutate(active = cases - lag(cases,n=14,default=0),
         recovered = cases - active)

rm(workshop, workshop_cases)
#NB: will have to make assumptions about age-distribution of cases
#______________________________________________________________________________________________________________________________________



###(4/5) Live updates of vaccination
##(i/iii) Load and clean data _________________________________________________
#Take John Hopkins reporting of vaccination coverage, a collation of data from WHO, CDC and Our World in Data
workshop <- readr::read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv")
workshop = workshop[workshop$'Country_Region' == setting_long,]
#NOTE: true doses_admin NE People_partially_vaccinated  + People_fully_vaccinated DUE TO JOHNSTON & JOHNSTON SINGLE DOSE VACCINE
# hence, this calculated variable is full
# CONFIRMED 12/01 PNG gov reports 383,407 doses admin, here total = 510,910 - 383,407 = J&J doses

vaccination_history = workshop[,c(2:5)]
vaccination_history = vaccination_history[,c('Date','People_partially_vaccinated','People_fully_vaccinated')]
colnames(vaccination_history) <- c('date','partial','full')
vaccination_history <- vaccination_history %>%
  pivot_longer(
    cols='partial':'full',
    names_to='dose_charac',
    values_to = 'num'
  )

#COMEBACK - some manual processing here due to lack of data
#last updated 12/01/2022 using:
# PNG - https://covid19.info.gov.pg/files/Situation%20Report/PNG%20COVID-19%20Health%20Situation%20Report%20105.pdf
# SLE - https://mohs.gov.sl/download/sl_-covax-esmf-sierra-leone-final_june-22-2021-updated-docx/

setting_vaccine <- read.csv("1_inputs/vaccine_setting_history.csv",header=TRUE)
setting_vaccine <- setting_vaccine[setting_vaccine$setting == setting,]

if ("Johnson & Johnson" %in% unique(setting_vaccine$vaccine_type)){
  setting_vaccine <- setting_vaccine %>%
    mutate(
      full = case_when(
        vaccine_type == "Johnson & Johnson" ~ 2*doses/(sum(setting_vaccine$doses)+setting_vaccine$doses[setting_vaccine$vaccine_type == "Johnson & Johnson"]),
        vaccine_type != "Johnson & Johnson" ~ doses/(sum(setting_vaccine$doses)+setting_vaccine$doses[setting_vaccine$vaccine_type == "Johnson & Johnson"])
          ),
      partial = case_when(
        vaccine_type == "Johnson & Johnson" ~ 0,
        vaccine_type != "Johnson & Johnson" ~ doses/(sum(setting_vaccine$doses)- setting_vaccine$doses[setting_vaccine$vaccine_type == "Johnson & Johnson"])
      ))
  
} else {
  setting_vaccine <- setting_vaccine %>%
    mutate(full=doses/sum(setting_vaccine$doses),
           partial=doses/sum(setting_vaccine$doses))
}
#CHECKED by hand for PNG and SLE 12/01/2021

setting_vaccine_2 <- setting_vaccine[,c('vaccine_type','full','partial')]
setting_vaccine_2 <- setting_vaccine_2 %>%
  pivot_longer(
    cols='full':'partial',
    names_to='dose_charac',
    values_to = 'prop'
  )

vaccination_history_2 <- vaccination_history %>%
  left_join(setting_vaccine_2, by = "dose_charac") %>%
  select(date, vaccine_type, dose_charac, num, prop) %>%
  mutate(coverage_this_date_num = round(num*prop),
         coverage_this_date = 100 * coverage_this_date_num / sum(pop))

vaccination_history_2 <- vaccination_history_2 %>%
  group_by(dose_charac,vaccine_type) %>%
  arrange(date) %>%
  mutate(doses_delivered_this_date = coverage_this_date_num - lag(coverage_this_date_num))

vaccination_history_3 <- vaccination_history_2 %>%
  mutate(dose = case_when(
    vaccine_type == 'Johnson & Johnson' & dose_charac == 'full' ~ 1,
    vaccine_type == 'Johnson & Johnson' & dose_charac == 'partial' ~ 0,
    dose_charac == 'full' ~ 2,
    dose_charac == 'partial' ~ 1
  ))  %>%
  mutate(vaccine_mode = case_when(
    vaccine_type == 'Pfizer' ~ 'mRNA',
    vaccine_type == 'Moderna' ~ 'mRNA',
    vaccine_type == 'AstraZeneca' ~ 'viral',
    vaccine_type == 'Sinopharm' ~ 'viral',
    vaccine_type == 'Sinovac' ~ 'viral',
    vaccine_type == 'Johnson & Johnson' ~ 'viral'
  ))

vaccination_history_3 <- vaccination_history_3[vaccination_history_3$dose !=0, ] # nrows = 1638-546/2 = 1365
vaccination_history_3 <- na.omit(vaccination_history_3) # nrows = 1365-5 = 1360

vaccination_history_POP <- vaccination_history_3[,c('date','vaccine_type','vaccine_mode','dose','coverage_this_date','doses_delivered_this_date')] %>%
  arrange(date,vaccine_type,dose)
##_____________________________________________________________________________


##(ii/iii) Adjust to model format _____________________________________________
#Split daily doses by age and risk
#COMEBACK - currently only one risk group at a time!
vaccination_history_TRUE = data.frame() 

if (risk_group_toggle == "off"){
  
  age_split =  pop/sum(pop[3:num_age_groups]); age_split[1:2] = 0 #COMEBACK - uniform assumption in ages 18+
  
  for (j in 1:num_age_groups){
    workshop = vaccination_history_POP
    workshop <- workshop %>% mutate(
      age_group = age_group_labels[j],
      doses_delivered_this_date = doses_delivered_this_date*age_split[j])
    vaccination_history_TRUE = rbind(vaccination_history_TRUE,workshop)
  }
  vaccination_history_TRUE$risk_group = "general_public"
  
} else if (risk_group_toggle == "on"){
  
  adult_pop = sum(pop[3:num_age_groups])
  age_risk_split =  pop_risk_group_dn %>%
    mutate(split = case_when(
      age_group %in% c('0 to 4','5 to 17') ~ 0,
      TRUE ~ pop/adult_pop
    ))
  
  if (is.na(risk_group_prioritisation_to_date) == FALSE){

    workshop = age_risk_split
    age_list = unique(workshop$age_group)[!(unique(workshop$age_group) %in% workshop$age_group[workshop$split == 0])]
    
    for (i in 1:length(age_list)){
      age_risk_split$split[ age_risk_split$risk_group == risk_group_name &  age_risk_split$age_group == age_list[i]] =  
        sum(workshop$split[ age_risk_split$age_group == age_list[i]]) * risk_group_prioritisation_to_date 
      
      age_risk_split$split[ age_risk_split$risk_group == 'general_public' &  age_risk_split$age_group == age_list[i]] =  
        sum(workshop$split[ age_risk_split$age_group == age_list[i]]) * (1-risk_group_prioritisation_to_date) 
    }
  }
  
  if (sum(age_risk_split$split) != 1){stop('(1) simulate setting line 290: dn of doses >1')}
  
  for (r in 1:num_risk_groups){
    for (j in 1:num_age_groups){
    workshop = vaccination_history_POP
    workshop <- workshop %>% mutate(
      age_group = age_group_labels[j],
      risk_group = risk_group_labels[r],
      doses_delivered_this_date = doses_delivered_this_date*age_risk_split$split[age_risk_split$age_group == age_group_labels[j] & age_risk_split$risk_group == risk_group_labels[r]])
    vaccination_history_TRUE = rbind(vaccination_history_TRUE,workshop)
    }
  }
  if (sum(vaccination_history_POP$doses_delivered_this_date) != sum(vaccination_history_TRUE$doses_delivered_this_date)){stop('(1) simulate setting line 305')}

} 

#Let's recalculate coverage_this_date here
vaccination_history_TRUE = vaccination_history_TRUE %>% 
  left_join(pop_risk_group_dn, by = c("age_group", "risk_group")) %>%
  group_by(risk_group,age_group,vaccine_type,dose) %>%
  mutate(coverage_this_date = case_when(
    pop > 0 ~ cumsum(doses_delivered_this_date)/pop,
    TRUE ~ 0)) %>%
  select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date,age_group,risk_group)

##_____________________________________________________________________________


##(iii/iii) Plot _______________________________________________________________
#COMEBACK: doses delivered very jagged, should we do seven day rolling average?
vaccination_history <- vaccination_history %>%
  mutate(coverage_this_date = 100 * num / sum(pop))

ggplot() + 
  geom_point(data=vaccination_history,aes(x=date,y=coverage_this_date,color=dose_charac),na.rm=TRUE) + 
  labs(title=setting_long) +
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("vaccine coverage (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

rm(workshop)
#NB: need to make assuptions about age-distribution of vaccinated
# SLE: https://www.unicef.org/sierraleone/stories/increased-supplies-covid-19-vaccines-help-taking-vaccines-closer-people (avaliable to 18+ in March/2021)
# but this is different to COVAX prioritisation plan https://mohs.gov.sl/download/sl_-covax-esmf-sierra-leone-final_june-22-2021-updated-docx/
# PNG: (anecodotally from Kamalini) also 18+?


#COMEBACK: need to assign full vaccinated + partial vaccinated - total doses delivered -> J&J and other TBC or if known to those vaccines

#______________________________________________________________________________________________________________________________________



###(5/5) TTIQ/PHSM i.e. NPI proxy - stringency index
#Government Response Stringency Index: composite measure based on 9 response indicators including 
# school closures, workplace closures, and travel bans, rescaled to a value from 0 to 100 (100 = strictest response)
#https://www.bsg.ox.ac.uk/research/research-projects/covid-19-government-response-tracker
#https://github.com/OxCGRT/covid-policy-tracker/tree/master/data
#https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md

### Static toggles
NPI_toggle = 'contain_health'   #options: contain_health, stringency

if (NPI_toggle == 'stringency'){
  workshop <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/stringency_index_avg.csv")
  workshop <- workshop[workshop$country_code == setting,]%>%
    pivot_longer(
      cols = 7:ncol(workshop) ,
      names_to = 'date',
      values_to = 'NPI'
    ) 
  
  NPI_estimates <- workshop[,c('date','NPI')] %>%
    mutate(date =as.Date(workshop$date, "%d%b%Y"))
  
} else if (NPI_toggle == 'contain_health'){
  workshop <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/containment_health_index_avg.csv")
  workshop <- workshop[workshop$country_code == setting,]%>%
    pivot_longer(
      cols = 7:ncol(workshop) ,
      names_to = 'date',
      values_to = 'NPI'
    ) 
  
  NPI_estimates <- workshop[,c('date','NPI')] %>%
    mutate(date =as.Date(workshop$date, "%d%b%Y"))
}
NPI_estimates = na.omit(NPI_estimates) #removing last two weeks where hasn't yet been calculated
#two metrics to give a go!


rm(workshop)

#Ox_tracker_complete <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")
#Ox_tracker_complete <- Ox_tracker_complete[Ox_tracker_complete$CountryCode == setting,]

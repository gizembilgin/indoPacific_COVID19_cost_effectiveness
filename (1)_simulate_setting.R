### This program will configure the model to the study population
### i.e. for this study to either Sierra Leone (SL) or Papua New Guinea (PNG)
###
### (1) Age structure of population
### (2) Contact patterns
### (3) Live updates of # active cases/recovered
### (4) Live updates of # vaccinated (if useful, otherwise .csv files)
### (5) Proxy estimate of TTIQ/PHSM i.e. NPI

### Still need: importation
#______________________________________________________________________________________________________________________________________

### Static toggles
NPI_toggle = 'contain_health'   #options: contain_health, stringency


### (1/5) Age structure of population
#NOTE: this program has been configured so that the age_groups can be modified on a whim,
# however, some other programs aren't as flexible
age_groups = c(0,4,19,29,39,49,59,110)
age_group_labels = c('0-4','5-19','20-29','30-39','40-49','50-59','60-100')
num_age_groups = J = length(age_group_labels)           # 0-4,5-11,12-15,16-29,30-59,60+

pop_orig <- read.csv("C:/Users/gizem/Documents/PhD/Research/2_scarce_COVID_vaccine_supply/4_code/inputs/pop_estimates.csv", header=TRUE)
pop_setting_orig <- pop_orig[pop_orig$country == setting,]
pop_setting <- pop_setting_orig %>%
  mutate(agegroup = cut(age,breaks = age_groups, include.lowest = T,labels = age_group_labels)) 
pop_setting <- aggregate(pop_setting$population, by=list(category=pop_setting$agegroup), FUN=sum)
colnames(pop_setting) <-c('agegroup','pop')
pop <- pop_setting$pop
#write.csv(pop_setting,file = 'x_results/pop_scrap.csv')

rm (pop_orig) #keep pop_setting_orig for contact matrix weighting
#______________________________________________________________________________________________________________________________________



### (2/5) Contact patterns of population
#CONFIRMATION FROM MARK JIT: .Rdata files are more up to date on GitHub (Prem et al. 2021 paper)
#(A/C) load contact matrix
load(file = "~/PhD/Research/2_scarce_COVID_vaccine_supply/4_code/inputs/contact_all.Rdata")
contact_matrix_setting <- contact_all[[setting]]
Prem_et_al_age_list <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)
colnames(contact_matrix_setting) <- Prem_et_al_age_list; rownames(contact_matrix_setting) <- Prem_et_al_age_list

#(B/C) calculate age weightings
Prem_et_al_age_num <- c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,110)
pop_Prem <- pop_setting_orig %>%
  mutate(agegroup = cut(age,breaks = Prem_et_al_age_num, include.lowest = T, labels = Prem_et_al_age_list)) 
pop_Prem <- aggregate(pop_Prem$population, by=list(category=pop_Prem$agegroup), FUN=sum)
colnames(pop_Prem) <-c('agegroup','pop')
pop_Prem <- pop_Prem$pop

#(C/C) construct contact matrix by our age groups
#create skeleton
workshop_interim  <- data.frame(matrix(ncol = 16,nrow=length(age_groups)-1))
rownames(workshop_interim) <- age_group_labels
colnames(workshop_interim) <- Prem_et_al_age_list

workshop <- data.frame(matrix(ncol = length(age_group_labels),nrow=length(age_group_labels)))
rownames(workshop) <- age_group_labels
colnames(workshop) <- age_group_labels

#age weighting of contacts
for (i in 1:(length(age_group_labels))){
  j = round((age_groups[i+1] - age_groups[i]) / 5) #how many age bands need to be collapsed?
  k = round(age_groups[i]/5)+1                     #where is the starting point?
  
  if (j+k>nrow(contact_matrix_setting)){j=nrow(contact_matrix_setting) - k +1} #caveat as 75+ doesn't align with age groups up to 110
  if (j==1){workshop_interim[i,] = contact_matrix_setting[k,]
  } else if (j>1){
    workshop_pop <- pop_Prem[k:(k+(j-1))]
    workshop_pop <- workshop_pop/sum(workshop_pop)
    workshop_interim[i,] = colSums( contact_matrix_setting[k:(k+(j-1)),] * workshop_pop)
  } 
}

#sum across collapsed age bands
for (i in 1:length(age_group_labels)){
  j = round((age_groups[i+1] - age_groups[i]) / 5) #how many age bands need to be collapsed?
  k = round(age_groups[i]/5)+1                     #where is the starting point?
  
  if (j+k>ncol(contact_matrix_setting)){j=ncol(contact_matrix_setting) - k +1} #caveat as 75+ doesn't align with age groups up to 110
  if (j>1){workshop[,i] = rowSums(workshop_interim[,k:(k+(j-1))])
  } else if (j == 1){workshop[,i] = workshop_interim[,k:(k+(j-1))]}
}

contact_matrix = workshop

rm(pop_setting_orig, contact_all, contact_matrix_setting,
   Prem_et_al_age_list, Prem_et_al_age_num, pop_Prem,
   workshop, workshop_interim, workshop_pop,i, j, k)
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

workshop_deaths <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
workshop_deaths = workshop_deaths[workshop_deaths$'Country/Region' == setting_long,]
workshop_deaths <- workshop_deaths %>%
  pivot_longer(
    cols = 5:ncol(workshop_deaths) ,
    names_to = 'date',
    values_to = 'deaths'
  )
workshop_deaths <- workshop_deaths %>%
  mutate(date =as.Date(workshop_deaths$date, "%m/%d/%y"))

workshop <- workshop_cases %>%
  left_join(workshop_deaths) %>%
  select(date, cases,deaths) %>%
  mutate(new = cases - lag(cases),
         case_fatality_ratio = deaths/cases,
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

ggplot() + 
  geom_point(data=workshop,aes(x=date,y=case_fatality_ratio),na.rm=TRUE) + 
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("case fatality (%)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

#ASSUMPTION: cases remain active for 14 days
case_history = workshop %>%
  mutate(active = cases - lag(cases,n=14,default=0),
         recovered = cases - active)

rm(workshop, workshop_cases, workshop_deaths)
#NB: will have to make assumptions about age-distribution of cases
#______________________________________________________________________________________________________________________________________



###(4/5) Live updates of vaccination
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

#some manual processing here due to lack of data
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
  left_join(setting_vaccine_2) %>%
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


#Split daily doses by age
vaccination_history_TRUE = data.frame() 
age_split =  pop/sum(pop[3:num_age_groups]); age_split[1:2] = 0 #COMEBACK - uniform assumption in ages 20+

for (j in 1:num_age_groups){
  workshop = vaccination_history_POP
  workshop <- workshop %>% mutate(
    age_group = age_group_labels[j],
    doses_delivered_this_date = doses_delivered_this_date*age_split[j])
  vaccination_history_TRUE = rbind(vaccination_history_TRUE,workshop)
}




## for overarching plotting
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

workshop <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/stringency_index.csv")
workshop <- workshop[workshop$country_code == setting,]%>%
  pivot_longer(
    cols = 4:ncol(workshop) ,
    names_to = 'date',
    values_to = 'stringency'
  ) 

workshop <- workshop[,c('date','stringency')] %>%
  mutate(date =as.Date(workshop$date, "%d%b%Y"))

workshop2 <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/containment_health_index.csv")
workshop2 <- workshop2[workshop2$country_code == setting,]%>%
  pivot_longer(
    cols = 4:ncol(workshop2) ,
    names_to = 'date',
    values_to = 'contain_health'
  ) 
workshop2 <- workshop2[,c('date','contain_health')] %>%
  mutate(date =as.Date(workshop2$date, "%d%b%Y"))

NPI_estimates_full <- workshop %>%
  left_join(workshop2)
NPI_estimates_full = na.omit(NPI_estimates_full) #removing last two weeks where hasn't yet been calculated
#two metrics to give a go!



rm(workshop, workshop2)

Ox_tracker_complete <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")
Ox_tracker_complete <- Ox_tracker_complete[Ox_tracker_complete$CountryCode == setting,]

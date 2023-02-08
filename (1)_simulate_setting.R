### This script configures the model to the study population
###
### (1) Age structure of population
### (2) Contact patterns
### (3) Live updates of reported cases
### (4) Live updates of # vaccinated 
### (5) Estimate of NPI
###
### Dependencies: setting, num_risk_groups, risk_group_name
### Creates: pop_*, contact_matrix, vaccination_history_*, NPI_estimates


if (exists("rootpath") == FALSE){rootpath = str_replace(getwd(), "GitHub_vaxAllocation","")}
if (exists("num_risk_groups") == FALSE){num_risk_groups = 2}
if (exists("fitting") == FALSE){ fitting = "off" }
#______________________________________________________________________________________________________________________________________


### (1/5) Age structure of population
##(A/B) Without risk groups
#This program has been configured so that the age_groups can be modified on a whim
#HOWEVER, you will have to update age_group labels manually
#AND rerun most (mech shop) scripts

age_groups_num = c(0,4,9,17,29,44,59,69,110)
age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100')

num_age_groups = J = length(age_group_labels)          
age_group_order = data.frame(age_group = age_group_labels, age_group_num = seq(1:J))

load(file = "1_inputs/UN_world_population_prospects/UN_pop_est.Rdata")

pop_orig <- UN_pop_est %>% 
  rename(country = ISO3_code,
         country_long = Location,
         population = PopTotal,
         population_female = PopFemale,
         age = AgeGrp)
rm(UN_pop_est)

pop_setting_orig <- pop_orig %>%
  filter(country == setting) 

setting_long = unique(pop_setting_orig$country_long)

pop_setting <- pop_setting_orig %>%
  mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
  group_by(age_group) %>%
  summarise(pop = as.numeric(sum(population)))

pop <- pop_setting$pop
#_______________________________________________________


##(B/B) With risk groups
pop_risk_group_dn = pop_setting %>% 
  mutate(risk_group = 'general_public')

if (num_risk_groups>1){
 
  if(risk_group_name %in% c('adults_with_comorbidities')){
    if (setting == "SLE"){
      risk_dn = read.csv('1_inputs/risk_group_distribution.csv')
      risk_dn = risk_dn[risk_dn$risk_group_name == risk_group_name,]
    } else{
      
      workshop <- read.csv('1_inputs/risk_group_distribution_Clarke_et_al_2020.csv')
      workshop = workshop %>% 
        filter(age_group_charac != 'all ages') %>%
        filter(country == setting) %>%
        rename(agegroup_RAW = age_group_charac,
               value = high_risk) # CHOICE between high_risk and increased_risk 

      underlying_age_grouping <- c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,110)
      
      pop_RAW =  pop_orig %>%
        filter(country == setting) %>%
        mutate(agegroup_RAW = cut(age,breaks = underlying_age_grouping, include.lowest = T, labels = unique(workshop$agegroup_RAW)),
               agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
        ungroup() %>%
        group_by(agegroup_MODEL) %>%
        mutate(model_group_percent = population/sum(population))
      
      toggle_upper_cut_off = 60  # CHOICE
      
      risk_dn = pop_RAW %>% 
        left_join(workshop, by = c("country", "agegroup_RAW")) %>% 
        mutate(value = case_when(
          age_group_num >= toggle_upper_cut_off ~ 1,
          TRUE ~ value
        )) %>%
        mutate(interim = model_group_percent * value) %>%
        group_by(agegroup_MODEL) %>%
        summarise(prop = sum(interim)) %>%
        rename(age_group = agegroup_MODEL)
      
      
    }
  } else if (risk_group_name %in% c('pregnant_women')){
    load(file = "1_inputs/prevalence_pregnancy.Rdata")
    risk_dn = prevalence_pregnancy %>%
      filter(country == setting)
  } else {
    stop('risk_group_name not a valid value')
  }
  
  risk_dn = risk_dn %>%
    select(age_group,prop)
  
  pop_high_risk = pop_setting %>% 
    left_join(risk_dn, by = "age_group") %>%
    mutate(risk_group = risk_group_name,
           pop = round(pop*prop)) %>% 
    select(risk_group,age_group,pop)
  
  pop_general_public   = pop_setting %>% 
    left_join(risk_dn, by = "age_group") %>%
    mutate(risk_group = 'general_public',
           pop = round(pop*(1-prop))) %>% 
    select(risk_group,age_group,pop)
  
  pop_risk_group_dn = rbind(pop_general_public,pop_high_risk)
  
  pop_risk_group = pop_risk_group_dn %>%
    group_by(risk_group) %>%
    summarise(pop = sum(pop))
} else{
  risk_dn = crossing(age_group=age_group_labels,
                     prop = 1)
}
if (round(sum(pop_risk_group_dn$pop)) != sum(pop)){stop('population by risk group group does not match total population!')}

risk_group_labels = unique(pop_risk_group_dn$risk_group)
#______________________________________________________________________________________________________________________________________



### (2/5) Contact patterns of population
#CONFIRMATION FROM MARK JIT: .Rdata files are more up to date on GitHub (Prem et al. 2021 paper)
#(A/C) load contact matrix
load(file = "1_inputs/contact_all.Rdata")
contact_matrix_setting <- contact_all[[setting]]
Prem_et_al_age_list <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)
colnames(contact_matrix_setting) <- Prem_et_al_age_list; rownames(contact_matrix_setting) <- Prem_et_al_age_list


#(B/C) calculate age weightings
Prem_et_al_age_num <- c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,110)
pop_Prem <- pop_setting_orig %>%
  mutate(agegroup_PREM = cut(age,breaks = Prem_et_al_age_num, include.lowest = T, labels = Prem_et_al_age_list),
         agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  group_by(agegroup_MODEL) %>%
  mutate(model_group_percent = population/sum(population)) %>%
  ungroup() %>%
  group_by(agegroup_PREM) %>%
  mutate(prem_group_percent = population/sum(population)) %>%
  select(age,agegroup_PREM,agegroup_MODEL,model_group_percent,prem_group_percent)
sum_1 = pop_Prem %>%
  group_by(agegroup_MODEL,agegroup_PREM) %>%
  summarise(model_group_percentage = sum(model_group_percent),.groups = "keep")
sum_2 = pop_Prem %>%
  group_by(agegroup_MODEL,agegroup_PREM) %>%
  summarise(prem_group_percentage = sum(prem_group_percent),.groups = "keep") 
pop_Prem = sum_1 %>% left_join(sum_2, by = c("agegroup_MODEL", "agegroup_PREM"))


#(C/C) construct contact matrix by our age groups
#create skeleton
workshop_cm1  <- data.frame(matrix(0,ncol = 16,nrow=length(age_groups_num)-1))
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

rm(contact_all, contact_matrix_setting, sum_1, sum_2,
   Prem_et_al_age_list, Prem_et_al_age_num, pop_Prem,
   workshop_cm2, workshop_cm1,i, j)
#______________________________________________________________________________________________________________________________________



###(3/5) Live updates of cases
if (file.exists(paste("1_inputs/live_updates/case_history",this_setting,Sys.Date(),".Rdata",sep='')) == TRUE){
  load(file = paste("1_inputs/live_updates/case_history",this_setting,Sys.Date(),".Rdata",sep=''))
} else {
  workshop_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  workshop_cases = workshop_cases[workshop_cases$'Country/Region' == setting_long,]
  workshop_cases <- workshop_cases %>%
    pivot_longer(
      cols = 5:ncol(workshop_cases) ,
      names_to = 'date',
      values_to = 'cases'
    )
  workshop_cases$date = as.Date(workshop_cases$date, "%m/%d/%y")
  
  case_history <- workshop_cases %>%
    mutate(new = cases - lag(cases),
           rolling_average = (new + lag(new,default=0) + lag(new,n=2,default=0)+lag(new,n=3,default=0)
                              +lag(new,n=4,default=0)+lag(new,n=5,default=0)+lag(new,n=6,default=0))/7)
  
  # ggplot() +
  #   geom_point(data=case_history,aes(x=date,y=rolling_average),na.rm=TRUE) +
  #   xlab("") +
  #   scale_x_date(date_breaks="1 month", date_labels="%b") +
  #   ylab("daily cases") +
  #   theme_bw() +
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         panel.border = element_blank(),
  #         axis.line = element_line(color = 'black'))
  
  rm(workshop_cases)
  
  save(case_history, file = paste("1_inputs/live_updates/case_history",this_setting,Sys.Date(),".Rdata",sep=''))

}
#______________________________________________________________________________________________________________________________________



###(4/5) Live updates of vaccination
#SETUP: Delay & Interval ____________________________________
vaxCovDelay = crossing(dose = seq(1,4),delay = 0) #delay from vaccination to protection
vaxCovDelay = vaxCovDelay %>%
  mutate(delay = case_when(
    dose == 1 ~ 21,
    TRUE ~ 14
  ))
#_________________________________________________


##(i/iii) Load and clean data _________________________________________________
if (fitting %in% c("on","wave_three") & file.exists(paste("1_inputs/fit/vaccination_history_TRUE",this_setting,risk_group_name,Sys.Date(),".Rdata",sep='')) == TRUE){
  load(file = paste("1_inputs/live_updates/vaccination_history_TRUE",this_setting,Sys.Date(),".Rdata",sep=''))
} else {
  if (setting != "SLE"){source(paste(getwd(),"/(silho) doses to dose_number.R",sep=""))}
  source(paste(getwd(),"/(silho)_",setting,"_vax.R",sep=""))
  
  if (fitting %in% c("on","wave_three")){
    save(vaccination_history_TRUE, file = paste("1_inputs/live_updates/vaccination_history_TRUE",this_setting,risk_group_name,Sys.Date(),".Rdata",sep=''))
  }
}

#project forward expected continuation of existing program
if (exists("antiviral_setup") == FALSE){antiviral_setup ="off"}
if(antiviral_setup == "on"){
  # to_plot = vaccination_history_TRUE %>%
  #   group_by(date,age_group,dose) %>%
  #   summarise(doses_delivered_this_date = sum(doses_delivered_this_date))%>%
  #   left_join(pop_setting, by = c("age_group")) %>%
  #   group_by(age_group,dose) %>%
  #   mutate(coverage_this_date = case_when(pop > 0 ~ cumsum(doses_delivered_this_date) /pop,
  #                                         TRUE ~ 0))
  # ggplot(to_plot) + geom_point(aes(x=date,y=coverage_this_date,color=as.factor(age_group)))+
  #   plot_standard +
  #   facet_grid(dose ~ .)
  
  proj_dates = seq(max(vaccination_history_TRUE$date) + 1, as.Date('2024-01-01'),by="days")
  proj_dates = crossing(date = proj_dates,
                        age_group = age_group_labels)
  #proj_dates = seq(max(vaccination_history_TRUE$date) + 1, date_start + 7*model_weeks,by="days")
  
  if(setting == "PNG"){
    future_vaccine_type = "Johnson & Johnson"
    dose_projection_floor = 1
  } else if (setting == "IDN"){
    future_vaccine_type = "Pfizer"
    dose_projection_floor = 1
  } else if (setting == "FJI"){
    future_vaccine_type = "Moderna"
    dose_projection_floor = 3
  }
    interval_previous = 3*52/12 * 7     #average behaviour over last three months

    primary_program_proj = vaccination_history_TRUE %>%
      filter(dose>= dose_projection_floor) %>%
      group_by(date, dose, age_group,risk_group) %>%
      summarise(doses_delivered_this_date = sum(doses_delivered_this_date), .groups = "keep") %>%
      filter(date > (max(vaccination_history_TRUE$date) - interval_previous)) %>%
      group_by(age_group,dose,risk_group) %>%
      summarise(doses_delivered_this_date = sum(doses_delivered_this_date)/interval_previous, .groups = "keep") %>%
      left_join(proj_dates,by='age_group') %>%
      mutate(vaccine_type = future_vaccine_type)

    to_plot = primary_program_proj %>% group_by(dose,date) %>% summarise(sum=sum(doses_delivered_this_date)) %>% group_by(dose) %>% mutate(cumsum = cumsum(sum))
    # ggplot(to_plot) + geom_point(aes(x=date,y=cumsum))+
    #        #plot_standard +
    #        facet_grid(dose ~ .)

    #split proj doses over prev dose type
    workshop = data.frame() 
    for (this_dose in unique(primary_program_proj$dose)){
      if (this_dose == 1){
        workshop_this_dose_pool = crossing(age_group = age_group_labels,
                                           risk_group = risk_group_labels,
                                           FROM_vaccine_type = future_vaccine_type,
                                           prop = 1)
      } else{
        workshop_this_dose_pool =  vaccination_history_TRUE %>% 
          filter(dose == this_dose - 1) %>%
          group_by(age_group,risk_group,vaccine_type) %>%
          summarise(total = sum(doses_delivered_this_date), .groups = "keep") %>%
          group_by(age_group,risk_group) %>%
          mutate(prop = total/sum(total)) %>%
          select(-total) %>%
          rename(FROM_vaccine_type = vaccine_type)
      }
      
      workshop_this_dose = primary_program_proj %>% 
        filter(dose == this_dose) %>%
        left_join(workshop_this_dose_pool,by=c("age_group","risk_group")) %>%
        mutate(doses_delivered_this_date = doses_delivered_this_date*prop)
      
      workshop = rbind(workshop,workshop_this_dose); rm(workshop_this_dose,workshop_this_dose_pool)
    }
    primary_program_proj = workshop
    
    vaccination_history_TRUE = bind_rows(vaccination_history_TRUE,primary_program_proj) %>%
      mutate(
        dose = as.numeric(dose),
        vaccine_mode = case_when(
          vaccine_type == 'Pfizer' ~ 'mRNA',
          vaccine_type == 'Moderna' ~ 'mRNA',
          vaccine_type == 'AstraZeneca' ~ 'viral_vector',
          vaccine_type == 'Sinopharm' ~ 'viral_inactivated',
          vaccine_type == 'Sinovac' ~ 'viral_inactivated',
          vaccine_type == 'Johnson & Johnson' ~ 'viral_vector'
        ),
        FROM_vaccine_type = case_when(
          is.na(FROM_vaccine_type) ~ vaccine_type,
          TRUE ~ FROM_vaccine_type),
        FROM_dose = dose - 1
      ) %>%
      group_by(date,vaccine_type,vaccine_mode,dose,age_group,risk_group,FROM_vaccine_type,FROM_dose) %>%
      summarise(doses_delivered_this_date = sum(doses_delivered_this_date), .groups = "keep") %>%
      left_join(pop_risk_group_dn, by = c("age_group", "risk_group")) %>%
      group_by(risk_group, age_group, vaccine_type, dose) %>%
      mutate(coverage_this_date = case_when(pop > 0 ~ cumsum(doses_delivered_this_date) /pop,
                                            TRUE ~ 0)) %>%
      select(date,vaccine_type,vaccine_mode,dose,coverage_this_date,doses_delivered_this_date,age_group,risk_group,FROM_vaccine_type,FROM_dose)

    to_plot = vaccination_history_TRUE %>%
      group_by(date,age_group,dose) %>%
      summarise(doses_delivered_this_date = sum(doses_delivered_this_date), .groups = "keep")%>%
      left_join(pop_setting, by = c("age_group")) %>%
      group_by(age_group,dose) %>%
      mutate(coverage_this_date = case_when(pop > 0 ~ cumsum(doses_delivered_this_date) /pop,
                                            TRUE ~ 0))
    # ggplot(to_plot) + geom_point(aes(x=date,y=coverage_this_date,color=as.factor(age_group)))+
    #   plot_standard +
    #   facet_grid(dose ~ .)
    rm(primary_program_proj)
  
}

#______________________________________________________________________________________________________________________________________



###(5/5) NPI proxy - stringency index
#Government Response Stringency Index: composite measure based on 9 response indicators including 
# school closures, workplace closures, and travel bans, rescaled to a value from 0 to 100 (100 = strictest response)
#https://www.bsg.ox.ac.uk/research/research-projects/covid-19-government-response-tracker
#https://github.com/OxCGRT/covid-policy-tracker/tree/master/data
#https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md

### Static toggles
NPI_toggle = 'contain_health'   #choice of NPI metric: contain_health, stringency

if (file.exists(paste("1_inputs/live_updates/NPI_estimates",this_setting,Sys.Date(),".Rdata",sep='')) == TRUE){
  load(file = paste("1_inputs/live_updates/NPI_estimates",this_setting,Sys.Date(),".Rdata",sep=''))
} else {
  if (NPI_toggle == 'stringency'){
    workshop <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/stringency_index_avg.csv")
    workshop <- workshop[workshop$country_code == setting,]%>%
      pivot_longer(
        cols = 7:ncol(workshop) ,
        names_to = 'date',
        values_to = 'NPI'
      ) 
  } else if (NPI_toggle == 'contain_health'){
    workshop <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/containment_health_index_avg.csv")
    workshop <- workshop[workshop$country_code == setting,]%>%
      pivot_longer(
        cols = 7:ncol(workshop) ,
        names_to = 'date',
        values_to = 'NPI'
      ) 
  }
  
  NPI_estimates <- workshop[,c('date','NPI')] %>%
    mutate(date =as.Date(workshop$date, "%d%b%Y"))
  NPI_estimates = na.omit(NPI_estimates) #removing last two weeks where hasn't yet been calculated
  rm(workshop,NPI_toggle)
  
  save(NPI_estimates, file = paste("1_inputs/live_updates/NPI_estimates",this_setting,Sys.Date(),".Rdata",sep=''))
}
#______________________________________________________________________________________________________________________________________



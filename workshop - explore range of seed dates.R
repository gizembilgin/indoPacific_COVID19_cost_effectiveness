###This program helps find the initial conditions for fitting


## Introduction of three waves _________________________________________________________________________________________________
num_1 = 7
num_2 = 3
num_3 = 7
attempts = data.frame(first_date = c(covid19_waves$date[1],
                                    covid19_waves$date[1]-num_1,covid19_waves$date[1]+num_1,
                                    covid19_waves$date[1],covid19_waves$date[1],
                                    covid19_waves$date[1],covid19_waves$date[1]),

                     second_date = c(covid19_waves$date[2],
                                     covid19_waves$date[2],covid19_waves$date[2],
                                     covid19_waves$date[2]-num_2,covid19_waves$date[2]+num_2,
                                     covid19_waves$date[2],covid19_waves$date[2]),

                     third_date = c(covid19_waves$date[3],
                                    covid19_waves$date[3],covid19_waves$date[3],
                                    covid19_waves$date[3],covid19_waves$date[3],
                                    covid19_waves$date[3]-num_3,covid19_waves$date[3]+num_3))

search_tracker = data.frame()
for (attempt_num in 1:nrow(attempts)){

  covid19_waves = data.frame(date = c(attempts$first_date[attempt_num],attempts$second_date[attempt_num],attempts$third_date[attempt_num]),
                             strain = c('delta','omicron','omicron'))

  source(paste(getwd(),"/CommandDeck.R",sep=""))

  incidence_log = incidence_log %>% mutate(first_date = attempts$first_date[attempt_num],
                                           second_date = attempts$second_date[attempt_num],
                                           third_date = attempts$third_date[attempt_num])
  search_tracker = rbind(search_tracker,incidence_log)

}



coeff <- 1/20

this_plot = search_tracker %>% filter(second_date == covid19_waves$date[2]  & third_date == covid19_waves$date[3])
ggplot() +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=this_plot,aes(x=date,y=rolling_average,color = as.factor(first_date))) +
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+
  plot_standard

this_plot = search_tracker %>% filter(second_date == covid19_waves$date[2]  & first_date == covid19_waves$date[1])
ggplot() +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=this_plot,aes(x=date,y=rolling_average,color = as.factor(third_date))) +
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+
  plot_standard

this_plot = search_tracker %>% filter(first_date == covid19_waves$date[1] & third_date == covid19_waves$date[3])
ggplot() +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=this_plot,aes(x=date,y=rolling_average,color = as.factor(second_date))) +
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+
  plot_standard


##different take
strain_inital = strain_now = 'WT'
baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-01-01'),
                                                             as.Date('2021-07-05')-21,
                                                             as.Date('2021-12-20')-21),
                                                    strain = c('WT','delta','omicron'))

model_weeks = as.numeric((Sys.Date()+1-date_start)/7)

workshop_incidence_log_tracker = data.frame()
final_state_tracker = data.frame()
nov21_seroprev_tracker = data.frame()
nov21_seroprev_AGE_tracker = data.frame()

for (this_shift in c(-30,0,30)){
  for (this_param in c(0.9,1,1.1,1.2)){
    
    fitting_beta = rep(this_param,3)
    covid19_waves = baseline_covid19_waves %>%
      mutate(date = date + this_shift)
    
    date_start = covid19_waves$date[1] - 2
    
    source(paste(getwd(),"/CommandDeck.R",sep=""))
    
    incidence_log = incidence_log %>%
      mutate(beta_mod = this_param,
             shift = this_shift)
    
    final_state = next_state %>%
      mutate(beta_mod = this_param,
             shift = this_shift)
    
    nov21_seroprev = data.frame(
      seroprev = sum(next_state_FIT$pop[next_state_FIT$class == "R"])/sum(next_state_FIT$pop),
      beta_mod = this_param,
      shift = this_shift)
    
    nov21_seroprev_AGE = next_state_FIT %>%
      filter(class == 'R') %>%
      group_by(age_group) %>%
      summarise(pop = sum(pop)) %>%
      rename(recovered = pop) %>%
      left_join(pop_setting,by='age_group') %>%
      mutate(seroprev= recovered/pop) %>%
      mutate(beta_mod = this_param,
             shift = this_shift)
    
    
    final_state_tracker = rbind(final_state_tracker,final_state)
    workshop_incidence_log_tracker = rbind(workshop_incidence_log_tracker,incidence_log)
    nov21_seroprev_tracker = rbind(nov21_seroprev_tracker,nov21_seroprev)
    nov21_seroprev_AGE_tracker = rbind(nov21_seroprev_AGE_tracker,nov21_seroprev_AGE)
  }
}

coeff <- 1/150
ggplot() +
  geom_point(data=case_history[case_history$date>min(workshop_incidence_log_tracker$date) & case_history$date<max(workshop_incidence_log_tracker$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=workshop_incidence_log_tracker,aes(x=date,y=rolling_average,color=as.factor(beta_mod))) + 
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+ 
  plot_standard + 
  facet_grid(shift ~ .) 

nov21_seroprev_tracker %>%
  dplyr::summarise(average_seroprev = mean(seroprev ), 
                   sd = sd(seroprev )) %>%
  mutate(UCI = average_seroprev + qnorm(0.975)*sd, 
                   LCI = average_seroprev + qnorm(0.025)*sd) %>%
  select(-sd)

nov21_seroprev_AGE_tracker %>%
  group_by(age_group) %>%
  dplyr::summarise(average_seroprev = mean(seroprev  ), 
                   sd = sd(seroprev  )) %>%
  mutate(UCI = average_seroprev + qnorm(0.975)*sd, 
         LCI = average_seroprev + qnorm(0.025)*sd) %>%
  select(-sd)

final_state_tracker %>%
  group_by(beta_mod,shift) %>%
  filter(class == 'R') %>%
  group_by(beta_mod,shift,age_group) %>%
  summarise(pop = sum(pop)) %>%
  rename(recovered = pop) %>%
  left_join(pop_setting,by='age_group') %>%
  mutate(seroprev= recovered/pop) %>%
  mutate(beta_mod = this_param,
         shift = this_shift)  %>%
  ungroup() %>%
  dplyr::summarise(average_seroprev = mean(seroprev  ), 
                   sd = sd(seroprev  )) %>%
  mutate(UCI = average_seroprev + qnorm(0.975)*sd, 
         LCI = average_seroprev + qnorm(0.025)*sd) %>%
  select(average_seroprev,UCI,LCI)
#______________________________________________________________________________________________________________



#WAVE ONE
#(1/3) seed date
if (this_setting == "FJI"){
  first_wave_strain = 'delta'
  search_list = c(as.Date("2021-10-12"),as.Date("2021-10-19"),as.Date("2021-10-26"),as.Date("2021-11-01"),as.Date("2021-11-08"),as.Date("2021-11-15"))
} else if (this_setting == "PNG"){
  first_wave_strain = 'WT'
  search_list = c(as.Date('2020-06-01'),as.Date('2020-07-01'),as.Date('2020-08-01'),as.Date('2020-12-01'),as.Date('2021-01-01'),as.Date('2021-02-01'))
}


plot_tracker = data.frame()
fit_tracker = data.frame()
for(counter in 1:length(search_list)){
  
  this_param = search_list[counter]
  
  strain_inital = strain_now = 'WT' 
  date_start = as.Date(this_param) - 2
  model_weeks =as.numeric((fit_cutoff_dates[1]-date_start)/7)
  
  fitting_beta = 1.2
  
  under_reporting_est = 100
  covid19_waves = data.frame(date = c(this_param),
                             strain = c(first_wave_strain))
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  #ggplot(incidence_log) + geom_line(aes(x=date,y=rolling_average))
  
  workshop = case_history %>%
    filter(date>=date_start) %>%
    select(date,rolling_average) %>%
    mutate(rolling_average =  rolling_average * under_reporting_est) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2 ,
           parameter_mod =  this_param)
  
  to_plot = workshop %>% filter(date>date_start & date<=(date_start+model_weeks*7))
  ggplot() +
    geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
    geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
    plot_standard
  
  fit_statistic = data.frame(fit = sum(workshop$fit_statistic,
                                       na.rm=TRUE),
                             parameter_mod = this_param)
  
  
  plot_tracker = rbind(plot_tracker,workshop)
  fit_tracker = rbind(fit_tracker,fit_statistic)
}
seed_date_fit_tracker = fit_tracker; seed_date_fit_tracker
seed_date_tracker = plot_tracker %>%
  filter(date<=(date_start+model_weeks*7) & date>=search_list[1])

ggplot() +
  geom_line(data=seed_date_tracker,aes(x=date,y=rolling_average,color=as.factor(parameter_mod)),na.rm=TRUE) +
  geom_point(data=seed_date_tracker,aes(x=date,y=adjusted_reported)) +
  plot_standard + 
  labs(color = 'seed date')

if(this_setting == "FJI"){best_seed_date_1 = as.Date('2021-06-09')}
if(this_setting == "PNG"){
  best_seed_date_1 = as.Date('2021-01-15')
  date_start = best_seed_date_1 - 2
}


### Explore range of beta modifiers
#first wave
if (this_setting == "FJI"){
  search_list = c(0.5,0.8,0.9,1,1.1,1.2,1.5,2)
} else if (this_setting == "PNG"){
  search_list = c(1,1.15,1.25,1.35,1.45)
}

search_beta_tracker = data.frame()
fit_beta_tracker = data.frame()
for(counter in 1:length(search_list)){
  
  this_param = search_list[counter]
  
  strain_inital = strain_now = 'WT' 
  model_weeks = as.numeric((fit_cutoff_dates[1]-date_start-1)/7)
  
  covid19_waves = data.frame(date = best_seed_date_1, strain = first_wave_strain)
  fitting_beta= c(this_param)  
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  workshop = case_history %>% 
    select(date,rolling_average) %>%
    rename(real_reported_average = rolling_average) %>%
    mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
      adjusted_reported = real_reported_average * under_reporting_est) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2) %>%
    mutate(beta_mod = this_param)
  
  to_plot = workshop %>% filter(date>date_start & date<=(date_start+model_weeks*7))
  ggplot() +
    geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
    geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
    plot_standard
  
  fit_statistic = data.frame(fit = sum(workshop$fit_statistic,
                                       na.rm=TRUE),
                             parameter_mod = this_param)
  
  search_beta_tracker = rbind(search_beta_tracker,workshop)
  fit_beta_tracker = rbind(fit_beta_tracker,fit_statistic)
}

to_plot = search_beta_tracker %>% filter(date>date_start & date<=(date_start+model_weeks*7))
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(beta_mod)),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported*3)) +
  plot_standard
#________________



#WAVE TWO
#(1/3) beta mod
search_list = c(0.5,0.8,1,1.2,1.5,2)

plot_tracker = data.frame()
fit_tracker = data.frame()

for(this_param in search_list){
  
  strain_inital = strain_now = 'WT' 
  model_weeks = as.numeric((fit_cutoff_dates[2]-date_start)/7)
  
  under_reporting_est = 40
  fitting_beta= c(first_wave_fit$par[3],
                  this_param)
  covid19_waves = data.frame(date = c(as.Date('2021-06-09')+round(first_wave_fit$par[1]),
                                      as.Date('2021-10-19')),
                             strain = c('delta','omicron'))
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  workshop = case_history %>%
    select(date,rolling_average) %>%
    mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
      rolling_average = case_when(
        date > fit_cutoff_dates[1] ~ rolling_average * under_reporting_est,
        date <= fit_cutoff_dates[1] ~ rolling_average * first_wave_fit$par[2])) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2 ,
           parameter_mod =  this_param)
  
  fit_statistic = data.frame(fit = sum(workshop$fit_statistic[workshop$date> fit_cutoff_dates[1]], #fit only after first wave
                      na.rm=TRUE),
                      parameter_mod = this_param)
  
  plot_tracker = rbind(plot_tracker,workshop)
  fit_tracker = rbind(fit_tracker,fit_statistic)
}
beta_fit_tracker = fit_tracker; beta_fit_tracker
beta_tracker = plot_tracker %>%
  filter(date<=(date_start+model_weeks*7) & date>=date_start)
ggplot() +
  geom_line(data=beta_tracker,aes(x=date,y=rolling_average,color=as.factor(parameter_mod)),na.rm=TRUE) +
  geom_point(data=beta_tracker,aes(x=date,y=adjusted_reported)) +
  plot_standard + 
  labs(color = 'beta modifier')



#(2/3) seed date
search_list = c(as.Date('2021-10-15'),covid19_waves$date[2],as.Date('2021-11-15'),as.Date('2021-12-01'))

plot_tracker = data.frame()
fit_tracker = data.frame()
for(this_param in search_list){
  
  strain_inital = strain_now = 'WT' 
  model_weeks =as.numeric((fit_cutoff_dates[2]-date_start)/7)
  
  under_reporting_est = 40
  fitting_beta= c(first_wave_fit$par[3],
                  1.2)
  covid19_waves = data.frame(date = c(as.Date('2021-06-09')+round(first_wave_fit$par[1]),
                                      this_param),
                             strain = c('delta','omicron'))
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  workshop = case_history %>%
    select(date,rolling_average) %>%
    mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
      rolling_average = case_when(
        date > fit_cutoff_dates[1] ~ rolling_average * under_reporting_est,
        date <= fit_cutoff_dates[1] ~ rolling_average * first_wave_fit$par[2])) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2 ,
           parameter_mod =this_param)
  
  fit_statistic = data.frame(fit = sum(workshop$fit_statistic[workshop$date> fit_cutoff_dates[1]], #fit only after first wave
                                       na.rm=TRUE),
                             parameter_mod = this_param)
  

  plot_tracker = rbind(plot_tracker,workshop)
  fit_tracker = rbind(fit_tracker,fit_statistic)
}
seed_date_fit_tracker = fit_tracker; seed_date_fit_tracker
seed_date_tracker = plot_tracker %>%
  filter(date<=(date_start+model_weeks*7) & date>=date_start) %>%
  mutate(parameter_mod = case_when(
    parameter_mod == unique(seed_date_tracker$parameter_mod)[1] ~ search_list[1],
    parameter_mod == unique(seed_date_tracker$parameter_mod)[2] ~ search_list[2],
    parameter_mod == unique(seed_date_tracker$parameter_mod)[3] ~ search_list[3],
    parameter_mod == unique(seed_date_tracker$parameter_mod)[4] ~ search_list[4]
  ))
ggplot() +
  geom_line(data=seed_date_tracker,aes(x=date,y=rolling_average,color=as.factor(parameter_mod)),na.rm=TRUE) +
  geom_point(data=seed_date_tracker,aes(x=date,y=adjusted_reported)) +
  plot_standard + 
  labs(color = 'seed date')
#best fit in middle two dates


#(3/3) under reporting
strain_inital = strain_now = 'WT' 
under_reporting_est = 40
fitting_beta= c(first_wave_fit$par[3],
                1.2)
covid19_waves = data.frame(date = c(as.Date('2021-06-09')+round(first_wave_fit$par[1]),
<<<<<<< Updated upstream
                                    as.Date('2021-10-15')),
=======
                                    covid19_waves$date[2]),
>>>>>>> Stashed changes
                           strain = c('delta','omicron'))
source(paste(getwd(),"/CommandDeck.R",sep=""))


search_list = c(15,20,25,30,35,40,45,50)

plot_tracker = data.frame()
fit_tracker = data.frame()
for(this_param in search_list){
  under_reporting_est = this_param
  
  workshop = case_history %>%
    select(date,rolling_average) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(rolling_average =  case_when(
      date > fit_cutoff_dates[1] ~ rolling_average * (1/under_reporting_est),
      date <= fit_cutoff_dates[1] ~ rolling_average * (1/first_wave_fit$par[2])),
           fit_statistic = abs(rolling_average - adjusted_reported) ^ 2,
           parameter_mod =  this_param)
  
  fit_statistic = data.frame(
    fit = sum(workshop$fit_statistic[workshop$date> fit_cutoff_dates[1]], #fit only after first wave
              na.rm=TRUE),
    parameter_mod = this_param)
  
  plot_tracker = rbind(plot_tracker,workshop)
  fit_tracker = rbind(fit_tracker,fit_statistic)
}
underreporting_tracker = fit_tracker; underreporting_tracker
underreporting_tracker = plot_tracker %>%
  filter(date<=(date_start+model_weeks*7) & date>=date_start)
ggplot() +
  geom_line(data=underreporting_tracker,aes(x=date,y=rolling_average,color=as.factor(parameter_mod)),na.rm=TRUE) +
  geom_point(data=underreporting_tracker,aes(x=date,y=adjusted_reported)) +
  plot_standard + 
  labs(color = 'underreporting')

#better fit with higher under reporting due to entry too early
#_______________________________________________________________________________



#WAVE THREE
#(1/3) beta mod
search_list = c(0.5,0.8,1,1.2,1.5,2)

plot_tracker = data.frame()
fit_tracker = data.frame()

for(this_param in search_list){
  
  strain_inital = strain_now = 'WT' 
  model_weeks = as.numeric((Sys.Date()+1-date_start)/7)
  
  under_reporting_est = 40
  fitting_beta= c(first_wave_fit$par[3],
                  second_wave_fit$par[3],
                  this_param)
  covid19_waves = data.frame(date = c(as.Date('2021-06-09')+round(first_wave_fit$par[1]),
                                      as.Date('2021-10-15')+round(second_wave_fit$par[1]),
                                      as.Date('2022-02-01')),
                             strain = c('delta','omicron','omicron'))
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  workshop = case_history %>%
    select(date,rolling_average) %>%
    mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
      rolling_average = case_when(
        date > fit_cutoff_dates[2] ~ rolling_average * under_reporting_est,
        date > fit_cutoff_dates[1] ~ rolling_average * second_wave_fit$par[2],
        date <= fit_cutoff_dates[1] ~ rolling_average * first_wave_fit$par[2])) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2 ,
           parameter_mod =  this_param)
  
  fit_statistic = data.frame(fit = sum(workshop$fit_statistic[workshop$date> fit_cutoff_dates[2]], #fit only after first wave
                                       na.rm=TRUE),
                             parameter_mod = this_param)
  
  plot_tracker = rbind(plot_tracker,workshop)
  fit_tracker = rbind(fit_tracker,fit_statistic)
}
beta_fit_tracker = fit_tracker; beta_fit_tracker
beta_tracker = plot_tracker %>%
  filter(date<=(date_start+model_weeks*7) & date>=date_start)
ggplot() +
  geom_line(data=beta_tracker,aes(x=date,y=rolling_average,color=as.factor(parameter_mod)),na.rm=TRUE) +
  geom_point(data=beta_tracker,aes(x=date,y=adjusted_reported)) +
  plot_standard + 
  labs(color = 'beta modifier')



#(2/3) seed date
search_list = c(('2022-01-01'),('2022-01-15'),('2022-02-01'),('2022-02-15'),('2022-03-01'))

plot_tracker = data.frame()
fit_tracker = data.frame()
for(this_param in search_list){
  
  strain_inital = strain_now = 'WT' 
  model_weeks = as.numeric((Sys.Date()+1-date_start)/7)
  
  under_reporting_est = 40
  fitting_beta= c(first_wave_fit$par[3],
                  second_wave_fit$par[3],
                  1)
  covid19_waves = data.frame(date = c(as.Date('2021-06-09')+round(first_wave_fit$par[1]),
                                      as.Date('2021-10-15')+round(second_wave_fit$par[1]),
                                      this_param),
                             strain = c('delta','omicron','omicron'))
 
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  workshop = case_history %>%
    select(date,rolling_average) %>%
    mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
      rolling_average = case_when(
        date > fit_cutoff_dates[2] ~ rolling_average * under_reporting_est,
        date > fit_cutoff_dates[1] ~ rolling_average * second_wave_fit$par[2],
        date <= fit_cutoff_dates[1] ~ rolling_average * first_wave_fit$par[2])) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2 ,
           parameter_mod =  this_param)
  
  fit_statistic = data.frame(fit = sum(workshop$fit_statistic[workshop$date> fit_cutoff_dates[2]], #fit only after first wave
                                       na.rm=TRUE),
                             parameter_mod = this_param)
  
  
  plot_tracker = rbind(plot_tracker,workshop)
  fit_tracker = rbind(fit_tracker,fit_statistic)
}
seed_date_fit_tracker = fit_tracker; seed_date_fit_tracker
seed_date_tracker = plot_tracker %>%
  filter(date<=(date_start+model_weeks*7) & date>=date_start) %>%
  mutate(parameter_mod = case_when(
    parameter_mod == unique(seed_date_tracker$parameter_mod)[1] ~ search_list[1],
    parameter_mod == unique(seed_date_tracker$parameter_mod)[2] ~ search_list[2],
    parameter_mod == unique(seed_date_tracker$parameter_mod)[3] ~ search_list[3],
    parameter_mod == unique(seed_date_tracker$parameter_mod)[4] ~ search_list[4]
  ))
ggplot() +
  geom_line(data=seed_date_tracker,aes(x=date,y=rolling_average,color=as.factor(parameter_mod)),na.rm=TRUE) +
  geom_point(data=seed_date_tracker,aes(x=date,y=adjusted_reported)) +
  plot_standard + 
  labs(color = 'seed date')
#best fit in middle two dates


#(3/3) under reporting
strain_inital = strain_now = 'WT' 
under_reporting_est = 40
fitting_beta= c(first_wave_fit$par[3],
                1.2)
covid19_waves = data.frame(date = c(as.Date('2021-06-09')+round(first_wave_fit$par[1]),
                                    as.Date('2021-10-15')),
                           strain = c('delta','omicron'))
source(paste(getwd(),"/CommandDeck.R",sep=""))


search_list = c(15,20,25,30,35,40,45,50)

plot_tracker = data.frame()
fit_tracker = data.frame()
for(this_param in search_list){
  under_reporting_est = this_param
  
  workshop = case_history %>%
    select(date,rolling_average) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(rolling_average =  case_when(
      date > fit_cutoff_dates[1] ~ rolling_average * (1/under_reporting_est),
      date <= fit_cutoff_dates[1] ~ rolling_average * (1/first_wave_fit$par[2])),
      fit_statistic = abs(rolling_average - adjusted_reported) ^ 2,
      parameter_mod =  this_param)
  
  fit_statistic = data.frame(
    fit = sum(workshop$fit_statistic[workshop$date> fit_cutoff_dates[1]], #fit only after first wave
              na.rm=TRUE),
    parameter_mod = this_param)
  
  plot_tracker = rbind(plot_tracker,workshop)
  fit_tracker = rbind(fit_tracker,fit_statistic)
}
underreporting_tracker = fit_tracker; underreporting_tracker
underreporting_tracker = plot_tracker %>%
  filter(date<=(date_start+model_weeks*7) & date>=date_start)
ggplot() +
  geom_line(data=underreporting_tracker,aes(x=date,y=rolling_average,color=as.factor(parameter_mod)),na.rm=TRUE) +
  geom_point(data=underreporting_tracker,aes(x=date,y=adjusted_reported)) +
  plot_standard + 
  labs(color = 'underreporting')

#better fit with higher under reporting due to entry too early
#_______________________________________________________________________________
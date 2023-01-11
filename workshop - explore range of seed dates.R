###This program helps find the initial conditions for fitting


## Introduction of three waves _________________________________________________________________________________________________
num_1 = 3
num_2 = 7
num_3 = 7
attempts = data.frame(first_date = c(as.Date('2021-06-15'),
                                    as.Date('2021-06-15')-num_1,as.Date('2021-06-15')+num_1,
                                    as.Date('2021-06-15'),as.Date('2021-06-15'),
                                    as.Date('2021-06-15'),as.Date('2021-06-15')),

                     second_date = c(as.Date('2021-11-01'),
                                     as.Date('2021-11-01'),as.Date('2021-11-01'),
                                     as.Date('2021-11-01')-num_2,as.Date('2021-11-01')+num_2,
                                     as.Date('2021-11-01'),as.Date('2021-11-01')),

                     third_date = c(as.Date('2022-02-14'),
                                    as.Date('2022-02-14'),as.Date('2022-02-14'),
                                    as.Date('2022-02-14'),as.Date('2022-02-14'),
                                    as.Date('2022-02-14')-num_3,as.Date('2022-02-14')+num_3))

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

this_plot = search_tracker %>% filter(second_date == as.Date('2021-11-01')  & third_date == as.Date('2022-02-14'))
ggplot() +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=this_plot,aes(x=date,y=rolling_average,color = as.factor(first_date))) +
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+
  plot_standard

this_plot = search_tracker %>% filter(second_date == as.Date('2021-11-01')  & first_date == as.Date('2021-06-15'))
ggplot() +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=this_plot,aes(x=date,y=rolling_average,color = as.factor(third_date))) +
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+
  plot_standard

this_plot = search_tracker %>% filter(first_date == as.Date('2021-06-15') & third_date == as.Date('2022-02-14'))
ggplot() +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=this_plot,aes(x=date,y=rolling_average,color = as.factor(second_date))) +
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+
  plot_standard
#______________________________________________________________________________________________________________



#WAVE ONE
### Explore range of seed dates
seed_date_list = c(as.Date("2021-10-12"),as.Date("2021-10-19"),as.Date("2021-10-26"),as.Date("2021-11-01"),as.Date("2021-11-08"),as.Date("2021-11-15"))
seed_date_list = c(as.Date('2021-11-15'),as.Date('2021-11-22'),as.Date('2021-11-29'),as.Date('2021-12-05'))

search_dates_tracker = data.frame()
fit_dates_tracker = data.frame()
for (this_seed_date in seed_date_list){
  seed_date_est = round(par[1])
  under_reporting_est = par[2]
  fitting_beta= c(first_wave_fit$par[3],par[3])
  
  strain_inital = strain_now = 'WT' 
  model_weeks = as.numeric((fit_cutoff_dates[2]-date_start)/7)
  
  covid19_waves = data.frame(date = c(as.Date('2021-06-09')+round(first_wave_fit$par[1]),
                                      this_seed_date),
                             strain = c('delta','omicron'))
  
  
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  workshop = reported_cases %>%
    filter(date %in% unique(incidence_log$date) &
             date> fit_cutoff_dates[1]) %>% #where first is fit till
    mutate(
      rolling_average = rolling_average * under_reporting_est) %>%
    rename(adjusted_reported = rolling_average) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2) %>%
    mutate(seed_date = this_seed_date)
  
  fit_statistic = sum(workshop$fit_statistic,na.rm=TRUE)
  
  search_dates_tracker = rbind(search_dates_tracker,workshop)
  fit_dates_tracker = rbind(fit_dates_tracker,fit_statistic)
}
ggplot() +
  geom_line(data=search_dates_tracker,aes(x=date,y=rolling_average,color=as.factor(seed_date)),na.rm=TRUE) +
  geom_point(data=search_dates_tracker,aes(x=date,y=adjusted_reported)) +
  plot_standard


### Explore range of beta modifiers
#first wave
beta_mod_list = c(0.5,0.8,0.9,1,1.1,1.2,1.5,2)

search_beta_tracker = data.frame()
fit_beta_tracker = data.frame()
for (this_beta_mod in beta_mod_list){
  
  strain_inital = strain_now = 'WT' 
  model_weeks = as.numeric((fit_cutoff_dates[1]-date_start-1)/7)
  covid19_waves = data.frame(date = as.Date('2021-06-09'), strain = 'delta')
  under_reporting_est = first_wave_fit$par[2]
  fitting_beta= c(this_beta_mod)  
  
  source(paste(getwd(),"/CommandDeck.R",sep=""))
  
  workshop = case_history %>% 
    select(date,rolling_average) %>%
    rename(real_reported_average = rolling_average) %>%
    mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
      adjusted_reported = real_reported_average * under_reporting_est) %>%
    left_join(incidence_log, by = "date") %>%
    mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2) %>%
    mutate(beta_mod = this_beta_mod)
  
  fit_statistic = sum(workshop$fit_statistic,na.rm=TRUE) 
  
  search_beta_tracker = rbind(search_beta_tracker,workshop)
  fit_beta_tracker = rbind(fit_beta_tracker,fit_statistic)
}

to_plot = search_beta_tracker %>% filter(date>date_start & date<=(date_start+model_weeks*7))
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(beta_mod)),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
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
search_list = c(as.Date('2021-10-15'),as.Date('2021-11-01'),as.Date('2021-11-15'),as.Date('2021-12-01'))

plot_tracker = data.frame()
fit_tracker = data.frame()
for(this_param in search_list){
  
  strain_inital = strain_now = 'WT' 
  model_weeks =as.numeric((fit_cutoff_dates[2]-date_start)/7)
  
  under_reporting_est = 40
  fitting_beta= c(first_wave_fit$par[3],
                  1)
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
           parameter_mod =  as.Date(this_param))
  
  fit_statistic = data.frame(fit = sum(workshop$fit_statistic[workshop$date> fit_cutoff_dates[1]], #fit only after first wave
                                       na.rm=TRUE),
                             parameter_mod = as.Date(this_param))
  

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
                1)
covid19_waves = data.frame(date = c(as.Date('2021-06-09')+round(first_wave_fit$par[1]),
                                    as.Date('2021-11-01')),
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

###This program helps find the initial conditions for fitting


## Introduction of three waves _________________________________________________________________________________________________
#initial search range of seed dates
if (this_setting == "FJI"){
  date_start = as.Date('2021-04-30')
  strain_inital = strain_now = 'WT' 
  
  covid19_waves = baseline_covid19_waves = data.frame(date = #c(as.Date('2021-06-06'),as.Date('2021-10-21'),as.Date('2022-01-15')), # initial best guess!
                                                        c(as.Date('2021-06-09'),as.Date('2021-10-15'),as.Date('2022-02-01')), # previous best guess
                                                      strain = c('delta','omicron','omicron'))
} else if (this_setting == "PNG"){
  
  strain_inital = strain_now = 'WT'
  
  baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-01-15'),as.Date('2021-09-01'),as.Date('2021-12-01')),
                                                      strain = c('WT','delta','omicron'))
  
  date_start = covid19_waves$date[1] - 2
} else if (this_setting == "TLS"){
  
  strain_inital = strain_now = 'WT'
  
  baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-03-01'),as.Date('2021-05-01'),as.Date('2022-01-01')),
                                                      strain = c('WT','delta','omicron'))
  
  date_start = covid19_waves$date[1] - 2
}
model_weeks = as.numeric((Sys.Date()+1-date_start)/7)

#risk group toggles
risk_group_prioritisation_to_date = NA
risk_group_lower_cov_ratio = NA
risk_group_toggle = "on"

risk_group_name = 'adults_with_comorbidities'
RR_estimate = 1.95

#run once!
system.time(source(paste(getwd(),"/CommandDeck.R",sep="")))

coeff <- 1/45
ggplot() +
  geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
             aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
  geom_line(data=incidence_log,aes(x=date,y=rolling_average)) + 
  scale_y_continuous(
    name = "Model projections",
    sec.axis = sec_axis(~.*coeff, name="Reported cases")
  )+ 
  plot_standard
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
                                    covid19_waves$date[2]),
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
##different take
model_weeks = as.numeric((Sys.Date()+1-date_start)/7)

under_reporting_est = 40

baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-06-09')+round(first_wave_fit$par[1]),
                                    as.Date('2021-10-15')+round(second_wave_fit$par[1]),
                                    as.Date('2022-02-01')),
                           strain = c('delta','omicron','omicron'))

plot_tracker = data.frame()
fit_tracker = data.frame()
workshop_incidence_log_tracker = data.frame()

# for (this_beta_mod in c(0.9, 1, 1.2, 1.5)) {
#   for (this_shift in c(0, 30, 60, 90, 180, -30)) {
    
for (this_beta_mod in c(1, 1.1,1.2,1.35,1.5)) {
  for (this_shift in c(0,15,30,45)) {
    if(nrow(plot_tracker[plot_tracker$shift == this_shift & plot_tracker$beta_mod == this_beta_mod,])>0){
      #skip
    } else{
      strain_inital = strain_now = 'WT' 
      
      fitting_beta= c(first_wave_fit$par[3],
                      second_wave_fit$par[3],
                      this_beta_mod)
      
      covid19_waves = baseline_covid19_waves 
      covid19_waves$date[3] = covid19_waves$date[3] + this_shift
      
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
               beta_mod = this_beta_mod, shift = this_shift)
      
      fit_statistic = data.frame(fit = sum(workshop$fit_statistic[workshop$date> fit_cutoff_dates[2]], #fit only after first wave
                                           na.rm=TRUE),
                                 beta_mod = this_beta_mod, shift = this_shift)
      
      incidence_log = incidence_log %>%
        mutate(beta_mod = this_beta_mod, shift = this_shift)
      
      plot_tracker = rbind(plot_tracker,workshop)
      fit_tracker = rbind(fit_tracker,fit_statistic)
      
      workshop_incidence_log_tracker = rbind(workshop_incidence_log_tracker,incidence_log)
    }
  }
}
beta_fit_tracker = fit_tracker; beta_fit_tracker
beta_tracker = plot_tracker %>%
  filter(date<=(date_start+model_weeks*7) & date>=date_start) %>%
  filter(beta_mod %in% c(1, 1.1,1.2,1.35,1.5) & shift %in% c(0,15,30,45))
ggplot() +
  geom_line(data=beta_tracker,aes(x=date,y=rolling_average,color=as.factor(beta_mod)),na.rm=TRUE) +
  geom_point(data=beta_tracker,aes(x=date,y=adjusted_reported)) +
  plot_standard + 
  labs(color = 'beta modifier')+ 
  facet_grid(shift ~ .) 








#(3/3) under reporting
strain_inital = strain_now = 'WT' 

fitting_beta= c(first_wave_fit$par[3],
                second_wave_fit$par[3],
                1.2)

covid19_waves = baseline_covid19_waves 
covid19_waves$date[3] = covid19_waves$date[3] + 15

source(paste(getwd(),"/CommandDeck.R",sep=""))


search_list = c(40,60,80,100,120)

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




### TLS - need to fit first two waves together!
setting = this_setting = "TLS"

baseline_covid19_waves = covid19_waves = data.frame(date = c(as.Date('2021-03-01'),as.Date('2021-05-01'),as.Date('2022-01-01')),
                                                      strain = c('WT','delta','omicron'))
date_start = covid19_waves$date[1] - 2
fitting_beta = c(1,1,1)
model_weeks = as.numeric((as.Date('2022-01-01')-date_start)/7)
under_reporting_est = 45

plot_tracker = data.frame()
fit_tracker = data.frame()
workshop_incidence_log_tracker = data.frame()

# for (this_beta_mod in c(0.9, 1, 1.2, 1.5)) {
#   for (this_shift in c(0, 30, 60, 90, 180, -30)) {

for (this_beta1 in c(1,1.1)) {
  for (this_beta2 in c(1,1.5,2)) {
    for (this_shift1 in c(-100,-90,-60,-30,-15)) {
      for (this_shift2 in c(-60,-45,-30,-15,0,15,30)) {
        if (nrow(plot_tracker[plot_tracker$beta1 == this_beta1 &
                              plot_tracker$beta2 == this_beta2 &
                              plot_tracker$shift1 == this_shift1 &
                              plot_tracker$shift2 == this_shift2, ]) > 0) {
          #skip
        } else{
          strain_inital = strain_now = 'WT'
          
          fitting_beta = c(this_beta1,
                           this_beta2,
                           1)
          
          covid19_waves = baseline_covid19_waves
          covid19_waves$date[1] = covid19_waves$date[1] + this_shift1
          covid19_waves$date[2] = covid19_waves$date[2] + this_shift2
          date_start = covid19_waves$date[1] - 2
          
          source(paste(getwd(), "/CommandDeck.R", sep = ""))
          
          workshop = case_history %>%
            select(date, rolling_average) %>%
            mutate(rolling_average =  rolling_average * under_reporting_est) %>%
            rename(adjusted_reported = rolling_average) %>%
            left_join(incidence_log, by = "date") %>%
            mutate(
              fit_statistic = abs(rolling_average - adjusted_reported) ^ 2 ,
              beta1 = this_beta1,
              beta2 = this_beta2,
              shift1 = this_shift1,
              shift2 = this_shift2
            )
          
          fit_statistic = data.frame(
            fit = sum(workshop$fit_statistic[workshop$date > fit_cutoff_dates[2]], #fit only after first wave
                      na.rm = TRUE),
            beta1 = this_beta1,
            beta2 = this_beta2,
            shift1 = this_shift1,
            shift2 = this_shift2
          )
          
          incidence_log = incidence_log %>%
            mutate(
                   beta1 = this_beta1,
                   beta2 = this_beta2,
                   shift1 = this_shift1,
                   shift2 = this_shift2
            )
          
          plot_tracker = rbind(plot_tracker, workshop)
          fit_tracker = rbind(fit_tracker, fit_statistic)
          
          workshop_incidence_log_tracker = rbind(workshop_incidence_log_tracker, incidence_log)
        }
      }
    }
  }
}
TLS_fit_tracker = fit_tracker; TLS_fit_tracker
TLS_tracker = plot_tracker %>%
  filter(date<= max(TLS_tracker$date[is.na(TLS_tracker$rolling_average) == FALSE]) & 
           date>= min(TLS_tracker$date[is.na(TLS_tracker$rolling_average) == FALSE])) #%>%
  #filter(beta_mod %in% c(1, 1.1,1.2,1.35,1.5) & shift %in% c(0,15,30,45))


to_plot = TLS_tracker  %>% filter(beta2 == 2  & !(beta1 %in% c(0.7,0.8,2,1.5,0.9,1.2))& shift2 == 30)
ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average,color=as.factor(beta1),linetype = as.factor(beta2)),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=adjusted_reported)) +
  plot_standard + 
  labs(color = 'beta1',linetype = 'beta2')+ 
  facet_grid(shift1 ~ .) 
save(TLS_tracker, file = paste('1_inputs/fit/TLS_tracker.Rdata',sep=''))




### WAVE ONE - IDN
strain_inital = strain_now = 'WT' 

fitting_beta= rep(1,length(reported_peaks))
under_reporting_est = 150

baseline_covid19_waves = data.frame(date = c(as.Date('2020-04-01'),as.Date('2021-04-01'),as.Date('2021-12-01')),
                                    strain = c('WT','delta','omicron'))

plot_tracker = data.frame()
fit_tracker = data.frame()
workshop_incidence_log_tracker = data.frame()

for (this_beta_mod in c(1,1.05,1.1)) {
  for (this_shift in c(135,142,150,157,165)) {
    if(nrow(plot_tracker[plot_tracker$shift == this_shift & plot_tracker$beta_mod == this_beta_mod,])>0){
      #skip
    } else{
      strain_inital = strain_now = 'WT' 
      
      fitting_beta[1]= this_beta_mod
      covid19_waves = baseline_covid19_waves 
      covid19_waves$date[1] = covid19_waves$date[1] + this_shift
      date_start = covid19_waves$date[1] - 7
      model_weeks = as.numeric((fit_cutoff_dates[1]-date_start)/7)
      
      source(paste(getwd(),"/CommandDeck.R",sep=""))
      
      workshop = case_history %>%
        select(date,rolling_average) %>%
        mutate(#under_reporting_est = coeff1 + coeff2*as.numeric(date - date_start), #linear
          rolling_average =  rolling_average * under_reporting_est) %>%
        rename(adjusted_reported = rolling_average) %>%
        left_join(incidence_log, by = "date") %>%
        mutate(fit_statistic = abs(rolling_average - adjusted_reported)^2 ,
               beta_mod = this_beta_mod, shift = this_shift)
      
      fit_statistic = data.frame(fit = sum(workshop$fit_statistic, #fit only after first wave
                                           na.rm=TRUE),
                                 beta_mod = this_beta_mod, shift = this_shift)
      
      incidence_log = incidence_log %>%
        mutate(beta_mod = this_beta_mod, shift = this_shift)
      
      plot_tracker = rbind(plot_tracker,workshop)
      fit_tracker = rbind(fit_tracker,fit_statistic)
      
      workshop_incidence_log_tracker = rbind(workshop_incidence_log_tracker,incidence_log)
    }
  }
}
beta_fit_tracker = fit_tracker; beta_fit_tracker
beta_tracker = plot_tracker %>%
  filter(date<= max(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE]) & 
           date>= min(plot_tracker$date[is.na(plot_tracker$rolling_average) == FALSE])) %>%
  filter(!(shift %in% c(120,180,195)) & beta_mod != 0.9)
ggplot() +
  geom_line(data=beta_tracker,aes(x=date,y=rolling_average,color=as.factor(beta_mod)),na.rm=TRUE) +
  geom_point(data=case_history[case_history$date<=max(beta_tracker$date),],aes(x=date,y=rolling_average*150)) +
  plot_standard + 
  labs(color = 'beta modifier')+ 
  facet_grid(shift ~ .) 








#(3/3) under reporting
strain_inital = strain_now = 'WT' 

fitting_beta= rep(1,length(reported_peaks))

covid19_waves = baseline_covid19_waves 

source(paste(getwd(),"/CommandDeck.R",sep=""))


search_list = c(40,60,80,100,120)

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

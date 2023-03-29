fitting = "on"


### LOAD WAVES
#Load first wave
list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("first_wave_fit",this_setting,"*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste('1_inputs/fit/',latest_file,sep=''))

#Load second wave
list_poss_Rdata = list.files(path="1_inputs/fit/",pattern = paste("second_wave_fit",this_setting,"*",sep=""))
list_poss_Rdata_details = double()
for (i in 1:length(list_poss_Rdata)){
  list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                  file.info(paste("1_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
}
latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
load(file = paste('1_inputs/fit/',latest_file,sep=''))
#_____________________________________________________


### RUN 
strain_inital = strain_now = 'WT' 

covid19_waves = baseline_covid19_waves
covid19_waves$date[1] = baseline_covid19_waves$date[1]+round(first_wave_fit$par[1])
covid19_waves$date[2] = baseline_covid19_waves$date[2]+round(second_wave_fit$optim$bestmem[1])

date_start = covid19_waves$date[1] - 7
model_weeks =as.numeric(as.Date('2022-12-31')-date_start)/7

fitting_beta= c(first_wave_fit$par[3],second_wave_fit$optim$bestmem[3])

source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
#_____________________________________________________


### PLOT
workshop = case_history %>%
  select(date,rolling_average) %>%
  rename(reported_cases = rolling_average) %>%
  right_join(incidence_log, by = "date") %>%
  left_join(omicron_shift, by = "date") %>%
  rename(omicron = percentage) %>%
  mutate(
    rolling_average = case_when(
      date>= min(omicron_shift$date) & is.na(omicron) == FALSE ~ rolling_average * (1/second_wave_fit$optim$bestmem[2]*omicron + 1/first_wave_fit$par[2]*(1-omicron)),
      date>= min(omicron_shift$date) ~ rolling_average * 1/second_wave_fit$optim$bestmem[2],
      date< min(omicron_shift$date) ~ rolling_average * 1/first_wave_fit$par[2])
    ) 
to_plot = workshop %>% 
  filter(date>date_start )#& date<=(date_start+model_weeks*7))

ggplot() +
  geom_line(data=to_plot,aes(x=date,y=rolling_average),na.rm=TRUE) +
  geom_point(data=to_plot,aes(x=date,y=reported_cases)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("")
require(beepr)
beep()
#_____________________________________________________


### SAVE
incidence_log = incidence_log %>% select(date,daily_cases)

fitted_results = list(
  FR_parameters = parameters,
  FR_next_state = next_state,
  FR_incidence_log_tidy = incidence_log_tidy,
  FR_incidence_log = incidence_log,
  FR_covid19_waves = covid19_waves,
  FR_fitting_beta = fitting_beta
)
save(fitted_results, file = paste("1_inputs/fit/fitted_results_",this_setting,Sys.Date(),".Rdata",sep=""))
#_____________________________________________________
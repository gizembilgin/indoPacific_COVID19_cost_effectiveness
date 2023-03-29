

seed_dates_list = list(#as.Date('2022-10-01'),
  as.Date('2023-01-01'),as.Date('2023-04-01'),as.Date('2023-07-01'),as.Date('2023-10-01'))



TLS_waves_tracker = data.frame()

#☺for (ticket in 1:2){ 
for (ticket in 1:length(seed_dates_list)){
  additional_seed_date = data.frame(date = seed_dates_list[[ticket]],
                                    strain = 'omicron')
  
  source(paste(getwd(),"/CommandDeck.R",sep=""),local=TRUE)
  
  incidence_log = incidence_log %>%
    mutate(additional_seed_date = seed_dates_list[[ticket]])
  
  TLS_waves_tracker = rbind(TLS_waves_tracker,incidence_log)
  
}

ggplot() + geom_line(data=TLS_waves_tracker,aes(x=date,y=rolling_average,color=as.factor(additional_seed_date)))

workshop <- read.csv(file = "01_inputs/vaccination/daily-covid-vaccination-doses-per-capita.csv", header=TRUE)
colnames(workshop) = c("setting_long","setting","date","daily_percentage_pop")

unique(workshop$setting_long)
workshop = workshop %>% 
  #filter(setting_long %in% c("Lower middle income", "Upper middle income" ,"Timor","Papua New Guinea","Indonesia","World","Oceania" )) %>%
  filter(setting_long %in% c("Lower middle income", "Upper middle income" ,"Oceania" )) %>%
  mutate(date = as.Date(date,format="%d/%m/%Y"))

ggplot(workshop) + geom_line(aes(x=date,y=daily_percentage_pop,color=as.factor(setting_long)))

workshop %>%
  group_by(setting_long) %>%
  #filter(date >= as.Date('2022-07-01') & date<as.Date('2023-01-01')) %>%
  filter(date >= as.Date('2022-01-01') & date<as.Date('2023-01-01')) %>%
  #filter(date >= as.Date('2021-01-01') & date<as.Date('2022-01-01')) %>%
  summarise(mean = mean(daily_percentage_pop))

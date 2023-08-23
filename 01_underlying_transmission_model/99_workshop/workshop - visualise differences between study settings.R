

#### CONTACT MATRICES
WPR_settings = c("PNG","TLS","IDN","FJI")
WPR_settings_long = c("Fiji","Indonesia","Papua New Guinea","Timor-Leste")

plot_list = list()
for (i in 1:length(WPR_settings)){
  this_setting = WPR_settings[i]
  
  contact_matrix_setting <- data.frame(contact_all[[this_setting]])
  
  visualise = contact_matrix_setting %>%
    rownames_to_column() %>%
    gather(contacts,value,-rowname) %>%
    rename(age_group = rowname)
  
  plot_list[[i]] = ggplot(visualise, aes(x = age_group, y = contacts, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low="white", high="blue")+
    labs(title=paste(this_setting))
  
}
plot_list



### RISK GROUP DISTRIBUTIONS
workshop <- read.csv('1_inputs/risk_group_distribution_Clarke_et_al_2020.csv')
workshop = workshop %>% filter(country %in% WPR_settings)
all_ages <- workshop %>% filter(age_group_charac == 'all ages')
ggplot() + geom_col(data = all_ages,aes(x=country,y=high_risk)) 
ggplot() + geom_col(data = all_ages,aes(x=country,y=increased_risk)) 

age_specific <- workshop %>% filter(age_group_charac != 'all ages')
plot1 = ggplot() + geom_line(data = age_specific,aes(x=age_group_num,y=100*high_risk,color=as.factor(country_long)),size=1) + 
  ylab("high risk (%)") +
  xlab("age in years") +
  labs(color="") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))
plot2 = ggplot() + geom_line(data = age_specific,aes(x=age_group_num,y=100*increased_risk,color=as.factor(country_long)),size=1) + 
  ylab("increased risk (%)") +
  xlab("age in years") +
  labs(color="") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))
ggarrange(plot1,plot2,
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = 2) 


#### PREVIOUS TRANSMISSION
workshop_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
workshop_cases = workshop_cases[workshop_cases$'Country/Region' %in% WPR_settings_long,]
workshop_cases <- workshop_cases %>%
  pivot_longer(
    cols = 5:ncol(workshop_cases) ,
    names_to = 'date',
    values_to = 'cases'
  ) %>%
  select(-Lat,-Long)
workshop_cases$date = as.Date(workshop_cases$date, "%m/%d/%y")
colnames(workshop_cases) = c('state','country','date','cases')

pop_est = UN_pop_est %>% 
  filter(ISO3_code %in% WPR_settings)%>%
  group_by(Location ) %>%
  summarise(pop = sum(PopTotal)) %>%
  rename(country = Location )

case_history <- workshop_cases %>%
  group_by(country) %>%
  mutate(new = cases - lag(cases),
         rolling_average = (new + lag(new,default=0) + lag(new,n=2,default=0)+lag(new,n=3,default=0)
                            +lag(new,n=4,default=0)+lag(new,n=5,default=0)+lag(new,n=6,default=0))/7) %>%
  left_join(pop_est, by = 'country') %>%
  mutate(rolling_average_percentage = rolling_average*100/pop)

plot_list = list()
for (i in 1:length(unique(case_history$country))){
  this_country = unique(case_history$country)[i]
  plot_list[[i]] = ggplot() +
    geom_point(data = case_history[case_history$country == this_country, ],
               aes(x = date, y = rolling_average),
               na.rm = TRUE) +
    labs(title = paste(this_country)) +
    xlab("") +
    scale_x_date(date_breaks = "3 month", date_labels = "%b") +
    ylab("daily cases") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = 'black')
    )
}
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],plot_list[[5]],plot_list[[6]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 2,
          nrow = 3) 

rm(workshop_cases)


### This script loads the data and function needed for dynamic infection-derived protection against reinfection.
### Dynamic infection-derived protection (rho) requires:
# (1) the shape of waning rho
# (2) tracking of days since infection of people in Recovered class
# (3) tracking of the introduction of new immune escape variants
# LIMITATION: our model does not track multiple infections, and hence assumes no additional immunity from repeat infections



### (1) Shape of waning ###############################################################################
#Exponential fit separately, see Supplementary Material for data source
rho_dn = crossing(type = c('new_to_new','prev_to_new'),
                  outcome = c('symptomatic_disease','severe_outcome'),
                  days = seq(0,365*2)
)
rho_dn = rho_dn %>% mutate(
  protection = case_when(
    outcome == 'symptomatic_disease' & type == 'prev_to_new' ~ 0.9932*exp(-0.004*days),
    outcome == 'symptomatic_disease' & type == 'new_to_new' ~ 0.9654*exp(-0.0002*days),
    outcome == 'severe_outcome' ~ 0.88 
  )
)

rho_est_plot = ggplot() + 
  geom_line(data = rho_dn[rho_dn$outcome == 'symptomatic_disease',],aes(days,protection,color=as.factor(type))) + 
  ylim(0,1) +labs(title = 'symptomatic_disease') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())   

rho_dn_wide = rho_dn %>% 
  filter(outcome == 'symptomatic_disease') %>%
  pivot_wider(
    id_cols = days,
    names_from = type,
    values_from = protection
  )
#######################################################################################################



### (2) Shape of introduction ###############################################################################
if (setting %in% c('SLE')){
  intro_raw <- read.csv("1_inputs/GISAID_omicron_intro_Africa.csv", header=TRUE)
} else if (setting %in% c('IDN','FJI','PHL','PNG','SLB','TLS')){
  intro_raw <- read.csv("1_inputs/GISAID_Oceania.csv", header=TRUE)
}

synthetic_strain_shift = data.frame()
plot_list = list()
for (i in 1:length(unique(intro_raw$strain))){
  
  this_strain = unique(intro_raw$strain)[i]
  this_raw = intro_raw %>% filter(strain == this_strain)
  if(this_strain == 'delta'){df_this_strain = 7}
  if(this_strain == 'omicron'){df_this_strain = 9}
  
  this_raw$date = as.Date(this_raw$date, '%d/%m/%Y')
  
  this_raw = this_raw %>% 
    mutate(days = as.numeric(date - min(this_raw$date)),
           percentage = percentage/100)
  if(exists("TOGGLE_delta_truncation_factor")){this_raw$days = this_raw$days * TOGGLE_delta_truncation_factor}
  
  ggplot(this_raw) + geom_point(aes(x=days,y=percentage))
  
  days_to_predict = seq(0,365)
  smoothed_spline <- smooth.spline(x = this_raw$days, y = this_raw$percentage, df = df_this_strain)
  fitted.results = predict(smoothed_spline,days_to_predict,deriv = 0)
  fit = data.frame(fitted.results)
  colnames(fit) = c('days','percentage')
  
  ggplot() + geom_point(data = this_raw, aes(x=days,y=percentage)) +
    geom_line(data = fit, aes(x=days,y=percentage)) 
  
  this_synthetic_strain_shift = fit %>% filter(percentage >=0 & percentage <=1) 
  real_days_removed = min(this_synthetic_strain_shift$days)
  this_synthetic_strain_shift$days = this_synthetic_strain_shift$days - min(this_synthetic_strain_shift$days)
  this_synthetic_strain_shift$strain = this_strain
  
  plot_list[[i]] = ggplot() + geom_point(data = this_raw, aes(x=days-real_days_removed,y=percentage)) +
    geom_line(data = this_synthetic_strain_shift, aes(x=days,y=percentage)) +
    ylab('percentage of circulating strains') +
    xlab('days since introduction')+
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(color = 'black')) + 
    labs(title = paste(this_strain))
  
  synthetic_strain_shift = rbind(synthetic_strain_shift,this_synthetic_strain_shift)
}
#grid.arrange(plot_list[[1]],plot_list[[2]])

if ("omicron" %in% unique(covid19_waves$strain)){
  omicron_shift = data.frame()
  for (this_wave in 1:nrow(covid19_waves[covid19_waves$strain == 'omicron',])){
    #ASSUMPTION: all subsequent waves of omicron out compete at same rates as omicron out competed delta
    workshop =  synthetic_strain_shift %>% 
      filter(strain == "omicron") %>% 
      select(days,percentage) %>%
      mutate(date = days + covid19_waves$date[covid19_waves$strain == "omicron"][this_wave],
             wave = this_wave) %>%
      select(-days)
    workshop$date = as.Date(workshop$date, format =  '%Y-%m-%d')
    
    if(this_wave>1){omicron_shift = omicron_shift %>% filter(date<min(workshop$date))}
    omicron_shift = rbind(omicron_shift,workshop)
  }
}
if ("delta" %in% unique(covid19_waves$strain) & !(covid19_waves$strain[1] == "delta")){
  delta_shift =  synthetic_strain_shift %>% filter(strain == "delta") %>% 
    select(days,percentage) %>%
    mutate(date = days + min(covid19_waves$date[covid19_waves$strain == "delta"])) %>%
    select(-days)
  delta_shift$date = as.Date(delta_shift$date, format =  '%Y-%m-%d')
} else{
  delta_shift = data.frame()
}


rm(intro_raw,this_strain,this_raw,df_this_strain,days_to_predict,smoothed_spline,fitted.results,fit,this_synthetic_strain_shift,real_days_removed,plot_list)
#######################################################################################################



### (3) Tracking of days since infection (proxy) for people in Recovered class ########################
rho_acq = rho_dn %>% filter(outcome == 'symptomatic_disease')

# This function calculates the population-level infection-derived protection against reinfection at any given time by age_group
rho_time_step <- function(date_now){
  
  #create data set of historical cases by day in recovery class
  if (exists("incidence_log") == FALSE){ incidence_log = data.frame() }
  if (nrow(incidence_log)>0){
    workshop = incidence_log %>% select(date,daily_cases)
    workshop = rbind(workshop,hist_cases)
  } else{
    workshop = hist_cases
  }
  #______________________
  
  
  #calculate proportion of individuals in recovery class by day since recovery
  workshop = workshop %>%
    mutate(date = date + round(AverageSymptomaticPeriod)) %>%
    filter(date <= date_now & date > (date_now - 1/omega)) %>%
    mutate(days = round(as.numeric(date_now - date)))
  if (sum(workshop$daily_cases) == 0){return(0)} #if no recovered cases, then rho = 0!
  
  workshop = workshop %>% mutate(prop_window = daily_cases/sum(workshop$daily_cases))  
  workshop$date = as.Date(workshop$date, '%Y-%m-%d')
  if (round(sum(workshop$prop_window),digits=5) != 1){stop('error in rho_time_step')}
  
  ggplot(workshop) + geom_line(aes(date,prop_window)) 
  #______________________
  
  
  #calculate protection to latest versus previous strains
  if (nrow(covid19_waves[covid19_waves$date<date_now & covid19_waves$strain == "omicron",])>0){ # do we have any immune escape variants?
    
    num_variants_introduced = nrow(covid19_waves[covid19_waves$date<date_now & covid19_waves$strain == "omicron",])
    
    #rho = how protected are people today from the circulating strains today?
    #    = %new circulating today * protection to new today + %old circulating today * protection to old today
    
    for (variant_num in 1:num_variants_introduced){
      
      this_variant_introduction = covid19_waves$date[covid19_waves$strain == "omicron"][variant_num]
      
      this_variant_shift = synthetic_strain_shift %>% filter(strain == covid19_waves$strain[covid19_waves$date == this_variant_introduction]) %>% 
        select(days,percentage) %>%
        mutate(date = days + this_variant_introduction) %>%
        select(-days)
      this_variant_shift$date = as.Date(this_variant_shift$date, format =  '%Y-%m-%d')
      
      if (variant_num == 1){
        protection_to_old = workshop %>% 
          left_join(rho_acq[rho_acq$type == 'new_to_new',], by = "days") %>% 
          mutate(interim = protection * prop_window)
        protection_to_old = sum(protection_to_old$interim,na.rm=TRUE)
      } else{
        protection_to_old = protection
      }
      
      protection_to_new = workshop %>%
        left_join(rho_dn_wide, by = "days") %>% 
        left_join(this_variant_shift,by="date") %>%
        mutate(interim = case_when(
          date < this_variant_introduction ~ prev_to_new * prop_window,
          date >= this_variant_introduction & date <= max(this_variant_shift$date)~(prev_to_new*(1-percentage)+new_to_new*percentage)*prop_window,
          date > max(this_variant_shift$date) ~ new_to_new*prop_window
        ))
      protection_to_new = sum(protection_to_new$interim,na.rm=TRUE)
      
      if (date_now %in% this_variant_shift$date){
        percentage_new = this_variant_shift$percentage[this_variant_shift$date == date_now]
      } else{
        percentage_new = this_variant_shift$percentage[this_variant_shift$date == max(this_variant_shift$date)]
      }
      
      protection = percentage_new * protection_to_new + (1-percentage_new) * protection_to_old
    }
    
    function_result = protection
    
  } else{
    workshop = workshop %>% 
      left_join(rho_acq[rho_acq$type == 'new_to_new',], by = "days") %>%
      mutate(interim = protection * prop_window)
    
    function_result = sum(workshop$interim,na.rm=TRUE) 
  }
  
  return(function_result)
}

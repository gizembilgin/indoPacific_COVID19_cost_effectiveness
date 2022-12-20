### This script runs the model from the first known point of seroprevalence until today's date {Sys.Date()}.
### Today's date is then saved as the latest fitted date of the model and used as date_start in modelling scenarios.
###
### Dependencies: nil
### Creates: fitted_results, fitted_max_date

#clear the field!
rm(list=ls())



### Setup ___________________________________________________________________________________________
FR_parameters = FR_next_state = FR_fitted_incidence_log_tidy = FR_fitted_incidence_log = data.frame()
#for (this_setting in c("SLE","PNG","TLS","IDN","FJI","SLB","PHL")){
for (this_setting in c("FJI")){ 
  #fitting toggles
  new_variant_check = "off" #include new variant now
  sensitivity_analysis_2022 = "off"
  
  #general toggles
  fitting = "on"
  fitting_details = "on"
  plotting = "on"
  outbreak_timing = "off" #i.e., no new outbreak if =="after" than new VOC after last vaccine delivery date, if == 'during" new VOC introduced one week from now
  vax_strategy_toggle = "off" #no additional vax
  vax_risk_strategy_toggle = "off"
  sensitivity_analysis_toggles = list()
  waning_toggle_acqusition = TRUE
  waning_toggle_severe_outcome = FALSE #let's save some time, this is not used in the modelling scenarios
  waning_toggle_rho_acqusition = TRUE
  
  #setting toggles
  setting = this_setting
  
  #UPDATE TO INCLUDE FIT TO STUDY SETTINGS HERE
  if (setting == "SLE"){
    date_start = as.Date('2021-03-31')
    strain_inital = strain_now = 'WT' 
    
    covid19_waves =  data.frame(date = c(as.Date('2021-04-25'),as.Date('2021-09-01')),
                                strain = c('delta','omicron'))
  } else if (setting == "FJI"){
    date_start = as.Date('2021-04-30')
    strain_inital = strain_now = 'WT' 
    
    covid19_waves = data.frame(date = c(as.Date('2021-06-15'),as.Date('2021-12-01'),as.Date('2022-04-01')),
                               strain = c('delta','omicron','omicron'))
  }
  
  model_weeks = as.numeric((Sys.Date()+1-date_start)/7)
  #model_weeks = 10
  
  if (sensitivity_analysis_2022 == 'on'){
    model_weeks = as.numeric((as.Date('2022-01-01')+1-date_start)/7)
  }
  if (new_variant_check == "on"){
    #add a new variant
    row = data.frame(date = Sys.Date(), strain = "omicron")
    covid19_waves = rbind(covid19_waves,row)
    
    model_weeks = model_weeks + 52 #to see expected trajectory
  }
  
  plot_standard = theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = 'black'))
  #______________________________________________________________________________________________________________
  
  
  
  ### Fit without risk group ____________________________________________________________________________________
  if (setting == "SLE"){
    risk_group_toggle = "off" 
    risk_group_name = 'general_public'
    
    source(paste(getwd(),"/CommandDeck.R",sep=""))
    grid.arrange(plot1,plot2,plot3,plot4,plot5, layout_matrix = lay)
    
    workshop_fitted_incidence_log_tidy = incidence_log_tidy %>% 
      mutate(country = setting, fitted_risk_group_scenario = "no risk group")
    workshop_fitted_incidence_log = incidence_log %>% select(date, daily_cases) %>% 
      mutate(country = setting, fitted_risk_group_scenario = "no risk group")
    workshop_parameters = parameters %>% 
      mutate(country = setting, fitted_risk_group_scenario = "no risk group")
    workshop_next_state = next_state  %>% 
      mutate(country = setting, fitted_risk_group_scenario = "no risk group")
    
    FR_fitted_incidence_log_tidy = rbind(FR_fitted_incidence_log_tidy,workshop_fitted_incidence_log_tidy)
    FR_fitted_incidence_log      = rbind(FR_fitted_incidence_log     ,workshop_fitted_incidence_log)
    FR_parameters                = rbind(FR_parameters               ,workshop_parameters)
    FR_next_state                = rbind(FR_next_state               ,workshop_next_state)
    
    
    
    #UPDATE TO INCLUDE FIT TO STUDY SETTINGS HERE
    #CHECK: rough growth advantage of Omicron over Delta
    first_omicron_date = covid19_waves$date[covid19_waves$date == min(covid19_waves$date[covid19_waves$strain == "omicron"])]

    wOmicron = Reff_tracker %>% filter(date>=first_omicron_date & date<(first_omicron_date+3*30))
    wOmicron = mean(wOmicron$Reff)
    
    wDelta = Reff_tracker %>% filter(date<first_omicron_date & date>=(first_omicron_date-1*30))
    wDelta = mean(wDelta$Reff)
    
    wOmicron/wDelta # 1.666128, aligns with 1.64 (WHO 2022 -  https://apps.who.int/iris/handle/10665/352390)
    rm(first_omicron_date)
    
    
    if (new_variant_check == "on"){
      wNew = Reff_tracker %>% filter(date>=max(covid19_waves$date) & date<(max(covid19_waves$date)+3*30))
      wNew = mean(wNew$Reff)
    
      wOmicron = Reff_tracker %>% filter(date<max(covid19_waves$date) & date>=(max(covid19_waves$date)-1*30))
      wOmicron = mean(wOmicron$Reff)
    
      wNew/wOmicron
      
      incidence_log_outbreak = incidence_log
    } else{
      incidence_log_fit = incidence_log
    }
  }
  #______________________________________________________________________________________________________________
  
  
  
  ### Fit with risk group _______________________________________________________________________________________
  if (new_variant_check == "off"){
    risk_group_prioritisation_to_date = NA
    risk_group_lower_cov_ratio = NA
    
    risk_group_toggle = "on"
    if (setting == "SLE"){
      risk_group_name_list = c('pregnant_women', 'adults_with_comorbidities')
      risk_group_RR_list = c(2.4,1.95)
    } else{
      risk_group_name_list = c('adults_with_comorbidities')
      risk_group_RR_list = c(1.95)
    }

    fitting_plot_list = list()
    
    for (fit_ticket in 1:length(risk_group_name_list)){
      risk_group_name = risk_group_name_list[fit_ticket]
      RR_estimate = risk_group_RR_list[fit_ticket]
      
      source(paste(getwd(),"/CommandDeck.R",sep=""))
      
      fitting_plot_list[[fit_ticket]] = list(plot1,plot2,plot3,plot4,plot5)
      
      workshop_fitted_incidence_log_tidy = incidence_log_tidy %>% 
        mutate(country = setting, fitted_risk_group_scenario = risk_group_name)
      workshop_fitted_incidence_log = incidence_log %>% select(date, daily_cases) %>% 
        mutate(country = setting, fitted_risk_group_scenario = risk_group_name)
      workshop_parameters = parameters %>% 
        mutate(country = setting, fitted_risk_group_scenario = risk_group_name)
      workshop_next_state = next_state  %>% 
        mutate(country = setting, fitted_risk_group_scenario = risk_group_name)
      
      FR_fitted_incidence_log_tidy = rbind(FR_fitted_incidence_log_tidy,workshop_fitted_incidence_log_tidy)
      FR_fitted_incidence_log      = rbind(FR_fitted_incidence_log     ,workshop_fitted_incidence_log)
      FR_parameters                = rbind(FR_parameters               ,workshop_parameters)
      FR_next_state                = rbind(FR_next_state               ,workshop_next_state)
      
    }
    grid.arrange(fitting_plot_list[[1]][[1]],fitting_plot_list[[1]][[2]],fitting_plot_list[[1]][[3]],fitting_plot_list[[1]][[4]],fitting_plot_list[[1]][[5]], layout_matrix = lay)
    grid.arrange(fitting_plot_list[[2]][[1]],fitting_plot_list[[2]][[2]],fitting_plot_list[[2]][[3]],fitting_plot_list[[2]][[4]],fitting_plot_list[[2]][[5]], layout_matrix = lay)
  }
  #______________________________________________________________________________________________________________
  
  ### CHECK _____________________________________________________________________________________________________
  #seroprevalence estimates
  workshop = next_state_FIT #November 2022
  workshop = next_state     #steady state in August 2022
  
  sum(workshop$pop[workshop$class == "R"])/sum(workshop$pop)
  
  workshop %>%
    filter(class == 'R') %>%
    group_by(age_group) %>%
    summarise(pop = sum(pop)) %>%
    rename(recovered = pop) %>%
    left_join(pop_setting,by='age_group') %>%
    mutate(seroprev= recovered/pop)
  
  #plot shape of outbreak compared to reported cases
  coeff <- 1/2000
  coeff <- 1/20
  
  ggplot() +
    geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
               aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
    geom_line(data=incidence_log,aes(x=date,y=rolling_average)) + 
    scale_y_continuous(
      name = "Model projections",
      sec.axis = sec_axis(~.*coeff, name="Reported cases")
    )+ 
    plot_standard
  
  if (new_variant_check == "on"){
    if (max(incidence_log_fit$date) == max(incidence_log_outbreak$date)){
      ggplot() +
        geom_point(data=case_history[case_history$date>date_start & case_history$date <max(incidence_log$date),],
                   aes(x=date,y=rolling_average/coeff),na.rm=TRUE) +
        geom_line(data=incidence_log_outbreak,aes(x=date,y=rolling_average)) + 
        geom_line(data = incidence_log_fit,aes(x=date,y=rolling_average),linetype = "dashed") +
        scale_y_continuous(
          name = "Model projections",
          sec.axis = sec_axis(~.*coeff, name="Reported cases")
        ) + 
        plot_standard
      
    }
  }
  #______________________________________________________________________________________________________________
}




### Save fitted results ______________________________________________________________________________________
fitted_results = list(
  FR_parameters,
  FR_next_state,
  FR_fitted_incidence_log_tidy,
  FR_fitted_incidence_log
)


if (new_variant_check == "off"){
  if (! Sys.Date() == date_now-1 & sensitivity_analysis_2022 == "off"){
    warning('fitted date not equal to current date')
    if (Sys.Date() > date_now){stop('fitted date less than current date, may cause problems with real vaccines not being delivered!')}
  }
  
  if (sensitivity_analysis_2022 == 'on'){
    save(fitted_results, file = '1_inputs/fitted_results_SA_2022.Rdata')
  } else{
    fitted_max_date = date_now-1  #incidence log always missed in first day of model
    save(fitted_max_date,file = '1_inputs/last_fit_date.Rdata')
    save(fitted_results, file = '1_inputs/fitted_results.Rdata')
  }
}
#______________________________________________________________________________________________________________






###TURN OFF FITTING ___________________________________________________________________________________________
fitting = "off"
#______________________________________________________________________________________________________________

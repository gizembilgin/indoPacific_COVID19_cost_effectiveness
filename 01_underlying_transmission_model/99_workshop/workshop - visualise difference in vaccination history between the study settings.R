
require(tidyverse);require(ggplot2)
plot_list = list()
for (this_setting in c("TLS","FJI","PNG","IDN")){
  
  ###LOAD THIS SETTING'S VACCINATION HISTORY TRUE
  list_poss_Rdata = list.files(path="01_inputs/live_updates/",pattern = paste("vaccination_history_TRUE",this_setting,"adults_with_comorbidities","*",sep=""))
  if (length(list_poss_Rdata)>0){
    list_poss_Rdata_details = double()
    for (j in 1:length(list_poss_Rdata)){
      list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                      file.info(paste('01_inputs/live_updates/',list_poss_Rdata[[j]],sep=''))$mtime)
    }
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste("01_inputs/live_updates/",latest_file,sep=''))
  #______________________________________________________________
  
  
  
  ###LOAD THIS SETTING'S pop risk distribution
  load(file = "01_inputs/UN_world_population_prospects/UN_pop_est.Rdata")
  workshop <- read.csv('01_inputs/risk_group_distribution_Clarke_et_al_2020.csv')
  workshop = workshop %>% 
    filter(age_group_charac != 'all ages' & 
             country == this_setting) %>%
    rename(agegroup_RAW = age_group_charac,
           value = high_risk)  
  
  underlying_age_grouping <- c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,110)
  
  pop_RAW =  UN_pop_est %>% 
    rename(country = ISO3_code,
           population = PopTotal,
           age = AgeGrp) %>%
    filter(country == this_setting) %>%
    mutate(agegroup_RAW = cut(age,breaks = underlying_age_grouping, include.lowest = T, labels = unique(workshop$agegroup_RAW)),
           agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
    ungroup() %>%
    group_by(agegroup_MODEL) %>%
    mutate(model_group_percent = population/sum(population))
  pop_setting = UN_pop_est %>% 
    filter(ISO3_code == this_setting) %>%
    mutate(age_group = cut(AgeGrp,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
    group_by(age_group) %>%
    summarise(pop = as.numeric(sum(PopTotal)))
  
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
    rename(age_group = agegroup_MODEL)  %>%
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
  #______________________________________________________________
  
  #by risk group
  # to_plot <- vaccination_history_TRUE %>%
  #   group_by(date,dose,risk_group) %>%
  #   summarise(doses_delivered_this_date = sum(doses_delivered_this_date), .groups = "keep") %>%
  #   left_join(pop_risk_group, by = "risk_group") %>%
  #   ungroup() %>%
  #   group_by(dose,risk_group) %>%
  #   mutate(coverage_this_date = case_when(
  #     pop > 0 ~ cumsum(doses_delivered_this_date)/pop,
  #     TRUE ~ 0
  #   ))
  # plot_list[[length(plot_list)+1]] <- ggplot(to_plot) + geom_point(aes(x=date,y=coverage_this_date, color = as.factor(risk_group))) + 
  #   facet_grid(dose ~.) +
  #   labs(title = this_setting)
  
  to_plot <- vaccination_history_TRUE %>%
    filter(age_group %in% c("18 to 29","30 to 44","45 to 59","60 to 69", "70 to 100")) %>%
    group_by(date,dose,age_group) %>%
    summarise(doses_delivered_this_date = sum(doses_delivered_this_date), .groups = "keep") %>%
    left_join(pop_setting, by = "age_group") %>%
    ungroup() %>%
    group_by(dose,age_group) %>%
    mutate(coverage_this_date = case_when(
      pop > 0 ~ cumsum(doses_delivered_this_date)/pop,
      TRUE ~ 0
    ))
  plot_list[[length(plot_list)+1]] <- ggplot(to_plot) + geom_point(aes(x=date,y=coverage_this_date, color = as.factor(age_group))) + 
    facet_grid(dose ~.) +
    labs(title = this_setting)
  plot_list[[length(plot_list)+1]] <- ggplot(to_plot) + geom_point(aes(x=date,y=coverage_this_date, color = as.factor(dose))) + 
    facet_grid(age_group ~.) +
    labs(title = this_setting)

  
}

plot_list
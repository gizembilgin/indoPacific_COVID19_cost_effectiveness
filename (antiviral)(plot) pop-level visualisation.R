


### eligible population by setting
settings_to_plot = c("PNG","FJI","TLS","IDN")
risk_group_name = "adults_with_comorbidities"
age_groups_num = c(0,4,9,17,29,44,59,69,110)
age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100')

#Step 1: load whole pop
num_age_groups = J = length(age_group_labels)          
age_group_order = data.frame(age_group = age_group_labels, age_group_num = seq(1:J))

load(file = "1_inputs/UN_world_population_prospects/UN_pop_est.Rdata")

pop_raw <- UN_pop_est %>% 
  filter(ISO3_code %in% settings_to_plot) %>%
  rename(country = ISO3_code,
         country_long = Location,
         population = PopTotal,
         population_female = PopFemale,
         age = AgeGrp) 
pop_orig = pop_raw %>%
  mutate(age_group = cut(age,breaks = age_groups_num, include.lowest = T,labels = age_group_labels)) %>%
  group_by(age_group,country) %>%
  summarise(pop = as.numeric(sum(population)))

#Step 2: separate into risk groups
if (risk_group_name %in% c('adults_with_comorbidities')) {
  workshop <-  read.csv('1_inputs/risk_group_distribution_Clarke_et_al_2020.csv')
  workshop = workshop %>%
    filter(age_group_charac != 'all ages') %>%
    filter(country %in% settings_to_plot) %>%
    rename(agegroup_RAW = age_group_charac,
           value = high_risk) # CHOICE between high_risk and increased_risk 
      
    underlying_age_grouping <- c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,110)
      
    pop_RAW =  pop_raw %>% 
        mutate(agegroup_RAW = cut(age,breaks = underlying_age_grouping, include.lowest = T, labels = unique(workshop$agegroup_RAW)),
               agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
        ungroup() %>%
        group_by(country,agegroup_MODEL) %>%
        mutate(model_group_percent = population/sum(population))
      
    toggle_upper_cut_off = 60  # CHOICE
    toggle_lower_cut_off = 18
      
    risk_dn = pop_RAW %>% 
        left_join(workshop, by = c("country", "agegroup_RAW")) %>% 
        mutate(value = case_when(
          #age_group_num >= toggle_upper_cut_off ~ 1,
          age_group_num < toggle_lower_cut_off ~ 0,
          TRUE ~ value
        )) %>%
        mutate(interim = model_group_percent * value) %>%
        group_by(country,agegroup_MODEL) %>%
        summarise(prop = sum(interim)) %>%
        rename(age_group = agegroup_MODEL)
      
} else if (risk_group_name %in% c('pregnant_women')){
    load(file = "1_inputs/prevalence_pregnancy.Rdata")
    risk_dn = prevalence_pregnancy %>%
      filter(country %in% settings_to_plot)
} else {
    stop('risk_group_name not a valid value')
}

  
pop_high_risk = pop_orig %>%
  left_join(risk_dn, by = c("age_group","country")) %>%
  mutate(risk_group = case_when(
    age_group %in% c("60 to 69","70 to 100") ~ 'adults aged 60+ with comorbidities',
    TRUE ~ risk_group_name),
         pop = round(pop * prop)) %>%
  select(country,risk_group, age_group, pop)

pop_general_public   = pop_orig %>%
  left_join(risk_dn, by = c("age_group","country")) %>%
  mutate(risk_group = case_when(
    age_group %in% c("60 to 69","70 to 100") ~ 'adults aged 60+',
    TRUE ~ 'general_public'),
         pop = round(pop * (1 - prop))) %>%
  select(country,risk_group, age_group, pop)

pop_risk_group_dn = rbind(pop_general_public, pop_high_risk) 

pop_risk_group = pop_risk_group_dn %>%
  group_by(country,risk_group) %>%
  summarise(pop = sum(pop)) %>%
  group_by(country) %>%
  mutate(prop = pop/sum(pop)) %>%
  filter(risk_group != 'general_public')

ggplot(pop_risk_group) + 
  geom_col(aes(x=country,y=prop*100,fill=as.factor(risk_group))) +
  xlab("") +
  ylab("% of total population") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(fill="")


pop_orig %>% 
  group_by(country) %>%
  mutate(prop = pop/sum(pop)) %>%
  filter(age_group %in% c('60 to 69','70 to 100')) %>%
  summarise(prop_aged_over_60 = sum(prop)) 
#_____________________________________________________________




### impact by coverage
workshop = RECORD_antiviral_model_simulations %>% 
  filter(vax_scenario_risk_group == "adults_with_comorbidities") %>%
  filter(antiviral_type == "nirmatrelvir_ritonavir" | intervention == 'vaccine') %>%
  filter(antiviral_target_group == 'adults_with_comorbidities' | intervention == 'vaccine') %>%
  filter( evaluation_group == 'pop_level') %>% #change eval group here to change from high-risk to pop-level plot
  filter(result %in% c("percentage")) %>%
  mutate(intervention = case_when(
    intervention == 'vaccine' ~ paste('booster dose starting 2023-03-01'),
    TRUE ~ intervention
  )) %>% group_by(setting_beta,intervention,outcome,antiviral_type,antiviral_target_group,evaluation_group,vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
  summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75))

options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]

  plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_pointrange(aes(x=median,y=vax_scenario_short,
                        xmin=LQ,xmax=UQ,
                        color=as.factor(intervention),
                        shape = as.factor(setting_beta)
    ))  +
    labs(title = paste(this_outcome),
         color = 'intervention',
         shape = "setting") +
    ylab('')+
    xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
    xlab('pop-level % outcomes prevented') +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical")
  
}
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = length(LIST_outcomes)) 
#ggsave(paste(rootpath,"x_results/plot_VaxVsAntivirals_",time,".png",sep=''), width = 9.6, height = 5.7)
options(warn = 0)


#remove booster doses and include other antiviral eligiblity groups
workshop = RECORD_antiviral_model_simulations %>% 
  filter(intervention == "antiviral 2023-01-01") %>%
  filter(antiviral_target_group != "unvaccinated_adults_AND_adults_with_comorbidities") %>%
  filter(vax_scenario_risk_group == "adults_with_comorbidities") %>%
  filter(antiviral_type == "nirmatrelvir_ritonavir") %>%
  filter(evaluation_group == 'pop_level') %>% #change eval group here to change from high-risk to pop-level plot
  filter(result %in% c("percentage")) %>% 
  group_by(setting_beta,intervention,outcome,antiviral_type,antiviral_target_group,evaluation_group,vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
  summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75))

options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_pointrange(aes(x=median,y=vax_scenario_short,
                        xmin=LQ,xmax=UQ,
                        color=as.factor(antiviral_target_group),
                        shape = as.factor(setting_beta)
    ))  +
    labs(title = paste(this_outcome),
         color = 'eligiblity group',
         shape = "setting") +
    ylab('')+
    xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
    xlab('pop-level % outcomes prevented') +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical")
  
}
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = length(LIST_outcomes)) 
#ggsave(paste(rootpath,"x_results/plot_VaxVsAntivirals_",time,".png",sep=''), width = 9.6, height = 5.7)
options(warn = 0)


#subset to no booster dose and look at comparing countries

workshop = RECORD_antiviral_model_simulations %>% 
  filter(intervention == "antiviral 2023-01-01") %>%
  filter(vax_scenario_short == 'no booster') %>%
  filter(antiviral_target_group != "unvaccinated_adults_AND_adults_with_comorbidities") %>%
  filter(vax_scenario_risk_group == "adults_with_comorbidities") %>%
  filter(antiviral_type == "nirmatrelvir_ritonavir") %>%
  filter(evaluation_group == 'pop_level') %>% #change eval group here to change from high-risk to pop-level plot
  filter(result %in% c("percentage")) %>% 
  group_by(setting_beta,intervention,outcome,antiviral_type,antiviral_target_group,evaluation_group,vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
  summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75))

options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  plot_list[[a]] =ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_pointrange(aes(x=median,y=setting_beta,
                        xmin=LQ,xmax=UQ,
                        color=as.factor(antiviral_target_group)
    ))  +
    labs(title = paste(this_outcome),
         color = 'eligiblity group') +
    ylab('')+
    xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
    xlab('pop-level % outcomes prevented') +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical")
  
}
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], plot_list[[4]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = length(LIST_outcomes)) 
#ggsave(paste(rootpath,"x_results/plot_VaxVsAntivirals_",time,".png",sep=''), width = 9.6, height = 5.7)
options(warn = 0)


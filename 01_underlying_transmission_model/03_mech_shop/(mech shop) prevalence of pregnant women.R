### This (mech shop) calculates the % per age group of pregnant women.
### Creates: prevalence_pregnancy

#LIMITATION: the model takes a one year snapshot, if a longer snapshot is required women will have to cycle in and out of this group, then:
# *3/4 to get women currently pregnant?  or *1/4 for third trimester? or * ~ 2 for all lactating women?

setting_list = c("SLE","PNG","TLS","IDN","FJI","SLB","PHL")
prevalence_pregnancy = data.frame()
prevalence_pregnancy_summary = data.frame()

for (ticket in 1:length(setting_list)){
  this_setting = setting_list[ticket]
  
  ### read in data
  if (this_setting == "SLE"){ #use SLE DHS 2019 data
    ASFR = read.csv("01_inputs/DHS_ASFR.csv",header=TRUE)
    
    # add 10-14 pregnancy as reported in DHS 2019 with retrospective data
    row_10_14 = data.frame(' 10-14 ',4/1000,NA,NA)
    colnames(row_10_14) = colnames(ASFR)
    ASFR = rbind(row_10_14,ASFR)
    ASFR_labels = ASFR$AGE
    ASFR_breaks = c(10,14,19,24,29,34,39,44,49)
    
    ggplot(data=ASFR) + 
      geom_pointrange(aes(x=ASFR*100,y=AGE,xmin=LCI*100,xmax=UCI*100)) +
      # xlim(0,1) +
      xlab("Age-specific fertility ratio (%)") + 
      ylab("") + 
      labs(title="") +
      theme_bw() + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(color = 'black'))
    
  } else { # use UN estimates
    load(file = "01_inputs/UN_world_population_prospects/UN_ASFR_est.Rdata")
    
    ASFR = UN_ASFR_est %>%
      filter(ISO3_code == this_setting) %>%
      rename(age = AgeGrp) %>%
      select(age,ASFR)
    rm(UN_ASFR_est)

    ggplot(data=ASFR) + 
      geom_point(aes(x=ASFR*100,y=age)) +
      # xlim(0,1) +
      xlab("Age-specific fertility ratio (%)") + 
      ylab("") + 
      labs(title="") +
      theme_bw() + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(color = 'black'))
  }
  
  
  
  ### calculate and plot female ratio estimates
  pop_together = pop_orig %>%
    filter(country == this_setting)  %>% 
    mutate(female_prop = population_female/population) %>%
    select(-population_female)
  ggplot(data=pop_together) + 
    geom_point(aes(x=female_prop*100,y=age)) +
    xlim(0,100) +
    ylim(15,49)
  
  
  ### convert ASFR to whole-population values (apply female ratio estimates)
  if (this_setting == "SLE"){
    pop_together = pop_together %>%
      mutate(agegroup_ASFR = cut(age,breaks = ASFR_breaks, include.lowest = T, labels = ASFR_labels)) %>%
      ungroup() %>%
      group_by(agegroup_ASFR) %>%
      mutate(ASFR_group_percent = population/sum(population),
             interim = ASFR_group_percent * female_prop) 
    ASFR_group_ratios = pop_together %>%
      group_by(agegroup_ASFR) %>%
      summarise(female_prop = sum(interim))
    Pop_ASFR = ASFR %>%
      rename(agegroup_ASFR = AGE)%>% 
      left_join(ASFR_group_ratios) %>%
      mutate(ASFR = ASFR * female_prop) %>%
      select(agegroup_ASFR,ASFR) 
    
    ### adapt ASFR to model age groups         
    pop_conversion = pop_orig %>%
      filter(country == this_setting) %>%
      mutate(agegroup_ASFR = cut(age,breaks = ASFR_breaks, include.lowest = T, labels = ASFR_labels),
             agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
      left_join(Pop_ASFR) %>%
      select(-agegroup_ASFR) %>%
      ungroup() %>% group_by(agegroup_MODEL) %>%
      mutate(agegroup_percent = population/sum(population),
             interim = agegroup_percent * ASFR) 
    pop_conversion$interim[is.na(pop_conversion$interim)]=0
    model_pregnancy_agegroups = aggregate(pop_conversion$interim, 
                                          by=list(category= pop_conversion$agegroup_MODEL), FUN=sum)
    colnames(model_pregnancy_agegroups) = c('age_group','prop')   
    
  } else{

    Pop_ASFR = ASFR %>%
      left_join(pop_together) %>%
      mutate(ASFR = ASFR * female_prop) %>%
      select(age,ASFR) 
    
   ASFR_population = ASFR %>%
      left_join(pop_together) %>%
      mutate(prop = population/sum(population),
             interim = prop * ASFR) %>%
     group_by(country_long,Time) %>%
     summarise(ASFR = sum(interim))
   prevalence_pregnancy_summary = rbind(prevalence_pregnancy_summary,ASFR_population)
    
    ### adapt ASFR to model age groups         
    pop_conversion = pop_orig %>%
      filter(country == this_setting) %>%
      mutate(agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
      left_join(Pop_ASFR) %>%
      ungroup() %>% group_by(agegroup_MODEL) %>%
      mutate(agegroup_percent = population/sum(population),
             interim = agegroup_percent * ASFR) 
    pop_conversion$interim[is.na(pop_conversion$interim)]=0
    model_pregnancy_agegroups = aggregate(pop_conversion$interim, 
                                          by=list(category= pop_conversion$agegroup_MODEL), FUN=sum)
    colnames(model_pregnancy_agegroups) = c('age_group','prop')   
  }


  
  ### save
  #colnames: risk_group, age_group, prop, source
  if (this_setting == "SLE"){
    workshop = model_pregnancy_agegroups %>% 
      mutate(risk_group = 'pregnant_women',
             source = 'DHS analysis + UN Pop prospects female ratio',
             country = this_setting)
  } else{
    workshop = model_pregnancy_agegroups %>% 
      mutate(risk_group = 'pregnant_women',
             source = 'UN Pop ASFR',
             country = this_setting)
  }

  prevalence_pregnancy = rbind(prevalence_pregnancy,workshop)
  
  ggplot(data=workshop) + 
    geom_point(aes(x=prop*100,y=age_group)) +
    # xlim(0,1) +
    xlab("Age-specific fertility ratio (%)") + 
    ylab("model age groups") + 
    labs(title="") 
}

ggplot(data=prevalence_pregnancy[prevalence_pregnancy$country %in% c("FJI","IDN","PNG","TLS"),]) + 
  geom_point(aes(x=prop*100,y=age_group,color=as.factor(country))) +
  # xlim(0,1) +
  xlab("Age-specific fertility ratio (%)") + 
  ylab("model age groups") + 
  labs(title="") 

prevalence_pregnancy_summary
#save(prevalence_pregnancy, file = "01_inputs/prevalence_pregnancy.Rdata")

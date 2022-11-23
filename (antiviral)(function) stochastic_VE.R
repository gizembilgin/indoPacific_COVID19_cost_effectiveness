### This program includes uncertainty in our estimates of VE by sampling from each VE estimate confidence interval using a uniform distribution

load(file = '1_inputs/VE_WHO_est.Rdata')
load(file = '1_inputs/VE_severe_outcomes_waning_pt_est.Rdata')
load(file = "1_inputs/UN_world_population_prospects/UN_pop_est.Rdata")
#system.time({stochastic_VE()}) #1.28 sec

stochastic_VE <- function(
    age_groups_num = c(0,4,9,17,29,44,59,69,110),
    age_group_labels = c('0 to 4','5 to 9','10 to 17','18 to 29','30 to 44','45 to 59','60 to 69','70 to 100'),
    strain_now = 'omicron',
    toggle_sampling = "uniform" # options: "normal" or "uniform"
){

  ##### PART ONE: point estimates of primary schedule ####################################################################################
  ### (1/2) Inital estimates from IVAC living systematic review 
  raw_VE_point_est <- VE_WHO_est
  raw_VE_severe_outcomes <- VE_severe_outcomes_waning_pt_est
  
  ######SAMPLE HERE
  options(warn = -1 )   
  if (toggle_sampling == "normal"){ sampled_value = mapply(rnorm,1,raw_VE_point_est$VE, raw_VE_point_est$sd)
  } else if (toggle_sampling == "uniform"){ sampled_value = mapply(runif,1,raw_VE_point_est$lower_est, raw_VE_point_est$upper_est)}
  options(warn = 0)   
  sampled_value[is.nan(sampled_value)] = NA
  sampled_value[sampled_value>1] = 1
  sampled_value[sampled_value<0] = 0
  VE_estimates = cbind(raw_VE_point_est,sampled_value)
  VE_estimates$VE = VE_estimates$sampled_value
  #######
  
  #correct so J&J after Sinopharm primary = dose 2 (J&J booster)
  VE_estimates$dose[VE_estimates$vaccine_type == 'Johnson & Johnson' & VE_estimates$primary_if_booster == "Sinopharm" & VE_estimates$outcome %in% c('severe_disease','death')] = 2
  VE_estimates = VE_estimates  %>% select(strain, vaccine_type, dose, outcome,VE,lower_est,upper_est)
  
  #(A/D) compare VE against death (where available) to VE against severe disease
  #Not statistically significantly different to 1 in '(mech shop) VE primary doses point estimates'
  #____________________
  
  #(B/D) compare VE against any infection (where avaliable) to symptomatic disease
  symptomatic_disease = VE_estimates[VE_estimates$outcome == 'symptomatic_disease',] %>% 
    select(strain,vaccine_type,dose,VE) %>%
    rename(symptomatic_disease = VE)
  any_infection = VE_estimates[VE_estimates$outcome == 'any_infection',] %>% 
    select(strain,vaccine_type,dose,VE) %>%
    rename(any_infection = VE)
  infection_sympt_ratio = symptomatic_disease %>% 
    left_join(any_infection, by = c("strain", "vaccine_type", "dose")) %>%
    mutate(ratio = any_infection/symptomatic_disease) %>% group_by(strain) %>%
    summarise(count = sum(is.na(ratio)),
              mean = mean(ratio,na.rm=TRUE),
              sd = sd(ratio,na.rm=TRUE),.groups = "keep")
  #_________________________
  
  #(C/D) compare VE against omicron (where avaliable) to delta
  delta = VE_estimates[VE_estimates$strain == 'delta',] %>% 
    select(outcome,vaccine_type,dose,VE) %>%
    rename(delta = VE)
  omicron = VE_estimates[VE_estimates$strain == 'omicron',] %>% 
    select(outcome,vaccine_type,dose,VE) %>%
    rename(omicron = VE)
  delta_omicron_ratio = delta %>% 
    left_join(omicron, by = c("outcome", "vaccine_type", "dose")) %>%
    mutate(ratio = omicron/delta) %>% 
    group_by(outcome) %>%
    summarise(count = sum(is.na(ratio)),
              mean = mean(ratio,na.rm=TRUE),
              sd = sd(ratio,na.rm=TRUE),.groups = "keep")
  delta_omicron_ratio$mean[delta_omicron_ratio$outcome == 'death'] = delta_omicron_ratio$mean[delta_omicron_ratio$outcome == 'severe_disease'] #since death is missing
  #_________________________
  
  #(D/D) dose one to two
  dose_one = VE_estimates[VE_estimates$dose == 1,] %>% 
    select(strain,outcome,vaccine_type,VE) %>%
    rename(dose_one = VE)
  dose_two = VE_estimates[VE_estimates$dose == 2,] %>% 
    select(strain,outcome,vaccine_type,VE) %>%
    rename(dose_two = VE)
  dose_ratio =  dose_one %>% 
    left_join(dose_two, by = c("strain", "outcome", "vaccine_type")) %>%
    mutate(ratio = dose_one/dose_two) %>% 
    group_by(outcome) %>%
    summarise(count = sum(is.na(ratio)),
              mean = mean(ratio,na.rm=TRUE),
              sd = sd(ratio,na.rm=TRUE),.groups = "keep")
  #___________________________________________________
  
  
  ### (2/2) Impute  values based on calculated ratios 
  workshop = VE_estimates %>%
    mutate(source = case_when(
      is.na(VE) == FALSE ~ 'literature',
      TRUE ~ 'imputed'
    ))
  
  #Step One: estimate dose one from dose two
  for (s in unique(workshop$strain)){
    for (t in unique(workshop$vaccine_type)){
      for (o in unique(workshop$outcome)){
        workshop_rows = workshop[workshop$strain == s & workshop$vaccine_type == t & workshop$outcome == o, ]
        if (workshop_rows$source[workshop_rows$dose == 1] == "imputed" & 
            workshop_rows$source[workshop_rows$dose == 2] == "literature"){
          estimate = workshop_rows$VE[workshop_rows$dose == 2] * dose_ratio$mean[dose_ratio$outcome == o]
          workshop$VE[workshop$strain == s & workshop$vaccine_type == t & workshop$outcome == o & workshop$dose == 1] = estimate
          workshop$source_extend[workshop$strain == s & workshop$vaccine_type == t & workshop$outcome == o & workshop$dose == 1] = "dose one estimated from dose two"
        }
      }
    }
  }
  
  #Step Two: estimate severe_disease <-> death, and acquisition <-> symptomatic
  #(A/B) severe_disease <-> death
  #close enough to one, thus set equal
  for (s in unique(workshop$strain)){
    for (t in unique(workshop$vaccine_type)){  
      for (d in c(1,2)){
        workshop_rows = workshop[workshop$strain == s & workshop$vaccine_type == t & workshop$dose == d & workshop$outcome %in% c('death','severe_disease'), ]
        #severe_outcome -> death
        if (workshop_rows$source[workshop_rows$outcome == 'death'] == "imputed" &
            is.na(workshop_rows$source_extend[workshop_rows$outcome == 'death']) &
            is.na(workshop_rows$VE[workshop_rows$outcome == 'severe_disease']) == FALSE){ #NOTE ASSUMPTION - may be imputing from already imputed value!!!
          estimate = workshop_rows$VE[workshop_rows$outcome == 'severe_disease'] 
          workshop$VE[workshop$strain == s & workshop$vaccine_type == t & workshop$outcome == 'death' & workshop$dose == d] = estimate
          workshop$source_extend[workshop$strain == s & workshop$vaccine_type == t & workshop$outcome == 'death' & workshop$dose == d] = "death inferred from severe_disease"
        }
        #death -> severe_outcome
        if (workshop_rows$source[workshop_rows$outcome == 'severe_disease'] == "imputed" &
            is.na(workshop_rows$source_extend[workshop_rows$outcome == 'severe_disease']) &
            is.na(workshop_rows$VE[workshop_rows$outcome == 'death']) == FALSE){ #NOTE ASSUMPTION - may be imputing from already imputed value!!!
          estimate = workshop_rows$VE[workshop_rows$outcome == 'death'] 
          workshop$VE[workshop$strain == s & workshop$vaccine_type == t & workshop$outcome == 'severe_disease' & workshop$dose == d] = estimate
          workshop$source_extend[workshop$strain == s & workshop$vaccine_type == t & workshop$outcome == 'severe_disease' & workshop$dose == d] = "severe_disease inferred from death"
        }
      }
    }
  }
  
  #(B/B) acquisition <-> symptomatic
  for (s in unique(workshop$strain)){
    for (t in unique(workshop$vaccine_type)){  
      for (d in c(1,2)){
        workshop_rows = workshop[workshop$strain == s & workshop$vaccine_type == t & workshop$dose == d & workshop$outcome %in% c('any_infection','symptomatic_disease'), ]
        #any infection -> symptomatic infection
        if (workshop_rows$source[workshop_rows$outcome == 'any_infection'] == "imputed" &
            is.na(workshop_rows$source_extend[workshop_rows$outcome == 'any_infection']) &
            is.na(workshop_rows$VE[workshop_rows$outcome == 'symptomatic_disease']) == FALSE){ #NOTE ASSUMPTION - may be imputing from already imputed value!!!
          estimate = workshop_rows$VE[workshop_rows$outcome == 'symptomatic_disease'] * infection_sympt_ratio$mean[infection_sympt_ratio$strain == s]
          workshop$VE[workshop$strain == s & workshop$vaccine_type == t & workshop$outcome == 'any_infection' & workshop$dose == d] = estimate
          workshop$source_extend[workshop$strain == s & workshop$vaccine_type == t & workshop$outcome == 'any_infection' & workshop$dose == d] = "any_infection inferred from symptomatic_disease"
        }
        #symptomatic infection -> any infection
        if (workshop_rows$source[workshop_rows$outcome == 'symptomatic_disease'] == "imputed" &
            is.na(workshop_rows$source_extend[workshop_rows$outcome == 'symptomatic_disease']) &
            is.na(workshop_rows$VE[workshop_rows$outcome == 'any_infection']) == FALSE){ #NOTE ASSUMPTION - may be imputing from already imputed value!!!
          estimate = workshop_rows$VE[workshop_rows$outcome == 'any_infection'] * 1/infection_sympt_ratio$mean[infection_sympt_ratio$strain == s]
          workshop$VE[workshop$strain == s & workshop$vaccine_type == t & workshop$outcome == 'symptomatic_disease' & workshop$dose == d] = estimate
          workshop$source_extend[workshop$strain == s & workshop$vaccine_type == t & workshop$outcome == 'symptomatic_disease' & workshop$dose == d] = "symptomatic_disease inferred from any_infection"
        }  
      }
    }
  }
  
  #Step Three: estimate omicron from delta
  for (o in unique(workshop$outcome)){
    for (t in unique(workshop$vaccine_type)){
      for (d in c(1,2)){
        workshop_rows = workshop[workshop$dose == d & workshop$vaccine_type == t & workshop$outcome == o, ]
        if (workshop_rows$source[workshop_rows$strain == "omicron"] == "imputed" & 
            is.na(workshop_rows$source_extend[workshop_rows$strain == 'omicron']) &
            is.na(workshop_rows$VE[workshop_rows$strain == 'delta']) == FALSE){
          estimate = workshop_rows$VE[workshop_rows$strain == 'delta'] * delta_omicron_ratio$mean[delta_omicron_ratio$outcome == o]
          workshop$VE[workshop$dose == d & workshop$vaccine_type == t & workshop$outcome == o & workshop$strain == "omicron"] = estimate
          workshop$source_extend[workshop$dose == d & workshop$vaccine_type == t & workshop$outcome == o & workshop$strain == "omicron"] = "omicron estimated from delta"
        }
      }
    }
  }
  if (nrow(workshop[is.na(workshop$VE) & workshop$dose < 3,])>0){stop('Some values to go!')}
  
  VE_estimates_imputed = workshop %>%
    mutate(vaccine_type_long = case_when(
      vaccine_type == "AstraZeneca" ~ "AstraZeneca Vaxzevria (ChAdOx1)",
      vaccine_type == "Johnson & Johnson" ~ "Johnson & Johnson Janssen (Ad26.COV2.S)",
      vaccine_type == "Moderna" ~ "Moderna Spikevax (mRNA-1273)",
      vaccine_type == "Pfizer" ~ "Pfizer-BioNTech Comirnaty (BNT162b2)",
      vaccine_type == "Sinopharm" ~ "Sinopharm BIBP vaccine",
      vaccine_type == "Sinovac" ~ "Sinovac Biotech CoronaVac"         
    ),
    vaccine_mode = case_when(
      vaccine_type == 'Pfizer' ~ 'mRNA',
      vaccine_type == 'Moderna' ~ 'mRNA',
      vaccine_type == 'AstraZeneca' ~ 'viral',
      vaccine_type == 'Sinopharm' ~ 'viral',
      vaccine_type == 'Sinovac' ~ 'viral',
      vaccine_type == 'Johnson & Johnson' ~ 'viral'
    ),
    outcome_family = case_when(
      outcome %in% c('any_infection','symptomatic_disease') ~ 'acquisition',
      outcome %in% c('severe_disease','death') ~ 'severe_outcome'
      
    ))
  
  # to_plot = VE_estimates_imputed %>%
  #   filter(strain == 'omicron' & dose !=3)
  # #to_plot = VE_estimates_imputed %>% filter(vaccine_type %in% c('Moderna','Pfizer','AstraZeneca'), strain == 'omicron')
  # 
  # plot_list = list()
  # for (i in 1:length(unique(to_plot$outcome))){
  #   outcome = unique(to_plot$outcome)[i]
  #   plot_list [[i]] <- ggplot(data=to_plot[to_plot$outcome==outcome,]) + 
  #     geom_pointrange(aes(x=VE,y=vaccine_type_long,color=as.factor(dose),shape=source,xmin=lower_est,xmax=upper_est)) +
  #     xlim(0,100) +
  #     xlab("") +
  #     theme_bw() + 
  #     scale_shape_manual(values=c(1,19)) +
  #     ylab("") + 
  #     labs(title=paste("VE against ",outcome,sep="")) + 
  #     theme(text=element_text(size=10), 
  #           plot.title=element_text(size=12))
  # }
  # plot_VE_point_estimates = ggarrange(plot_list[[1]],plot_list[[4]],plot_list[[3]],plot_list[[2]],
  #                                     common.legend = TRUE,
  #                                     legend="bottom")
  # plot_VE_point_estimates
  #################################################################################################################################################################
  
  
  
  
  
  ##### PART TWO: point estimates for booster doses ##############################################################################################################
  ### (1/2) Inital estimates from IVAC living systematic review 
  booster_VE_point_est = raw_VE_point_est %>% #loaded in PART ONE
    filter(dose == 3 & vaccine_type == 'Pfizer') %>% 
    select(strain, vaccine_type, primary_if_booster, outcome,VE,lower_est,upper_est,sd) %>%
    mutate(primary_if_booster_long = case_when(
      primary_if_booster == "AstraZeneca" ~ "AstraZeneca Vaxzevria (ChAdOx1)",
      primary_if_booster == "Johnson & Johnson" ~ "Johnson & Johnson Janssen (Ad26.COV2.S)",
      primary_if_booster == "Moderna" ~ "Moderna Spikevax (mRNA-1273)",
      primary_if_booster == "Pfizer" ~ "Pfizer-BioNTech Comirnaty (BNT162b2)",
      primary_if_booster == "Sinopharm" ~ "Sinopharm BIBP vaccine",
      primary_if_booster == "Sinovac" ~ "Sinovac Biotech CoronaVac"         
    ))
  
  ######SAMPLE HERE
  options(warn = -1 )   
  if (toggle_sampling == "normal"){ sampled_value = mapply(rnorm,1,booster_VE_point_est$VE, booster_VE_point_est$sd)
  } else if (toggle_sampling == "uniform"){ sampled_value = mapply(runif,1,booster_VE_point_est$lower_est, booster_VE_point_est$upper_est)}
  options(warn = 0)   
  sampled_value[is.nan(sampled_value)] = NA  
  sampled_value[sampled_value>1] = 1
  sampled_value[sampled_value<0] = 0
  VE_estimates = cbind(booster_VE_point_est,sampled_value)
  VE_estimates$VE = VE_estimates$sampled_value
  #######

  #Average across estimates from IVAC living systematic review
  VE_estimates = VE_estimates %>% 
    select(strain, vaccine_type, primary_if_booster, outcome,VE) %>% 
    group_by(strain, vaccine_type, primary_if_booster, outcome) %>%
    summarise(VE = sum(VE)/n(),.groups = "keep")
  
  #Impute missing values based on previous analysis (NB:only 'any_infection' needs imputing)
  imputed_rows = VE_estimates %>% 
    filter(outcome == 'symptomatic_disease' & (!primary_if_booster %in% unique(VE_estimates$primary_if_booster[VE_estimates$outcome == 'any_infection']))) %>%
    mutate(VE = VE * infection_sympt_ratio$mean[infection_sympt_ratio$strain == 'omicron'],
           outcome = 'any_infection')
  VE_estimates = rbind(VE_estimates,imputed_rows)
  
  #Save point estimate for booster doses 
  VE_booster_estimates = VE_estimates %>%
    mutate(dose = 3,
           vaccine_type_long = case_when(
             vaccine_type == "AstraZeneca" ~ "AstraZeneca Vaxzevria (ChAdOx1)",
             vaccine_type == "Johnson & Johnson" ~ "Johnson & Johnson Janssen (Ad26.COV2.S)",
             vaccine_type == "Moderna" ~ "Moderna Spikevax (mRNA-1273)",
             vaccine_type == "Pfizer" ~ "Pfizer-BioNTech Comirnaty (BNT162b2)",
             vaccine_type == "Sinopharm" ~ "Sinopharm BIBP vaccine",
             vaccine_type == "Sinovac" ~ "Sinovac Biotech CoronaVac"         
           ),
           vaccine_mode = case_when(
             vaccine_type == 'Pfizer' ~ 'mRNA',
             vaccine_type == 'Moderna' ~ 'mRNA',
             vaccine_type == 'AstraZeneca' ~ 'viral',
             vaccine_type == 'Sinopharm' ~ 'viral',
             vaccine_type == 'Sinovac' ~ 'viral',
             vaccine_type == 'Johnson & Johnson' ~ 'viral'
           ),
           outcome_family = case_when(
             outcome %in% c('any_infection','symptomatic_disease') ~ 'acquisition',
             outcome %in% c('severe_disease','death') ~ 'severe_outcome'
           ))
  #################################################################################################################################################################
  
  
  
  
  
  ##### PART THREE: VE waning for severe outcomes #################################################################################################################
  #including lower VE against severe outcomes in older adults: (1) a faster speed of waning, and (2) lower strength of inital protection.
  
  #rm(list=ls())
  raw <- raw_VE_severe_outcomes
  
  ######SAMPLE HERE
  if (toggle_sampling == "normal"){ sampled_value = mapply(rnorm,1,raw$VE, raw$sd)
  } else if (toggle_sampling == "uniform"){ sampled_value = mapply(runif,1,raw$LB, raw$UB)}
  sampled_value[sampled_value>1] = 1
  sampled_value[sampled_value<0] = 0
  raw = cbind(raw,sampled_value)
  raw$VE = raw$sampled_value
  #######
  
  
  ###(1/3) Predict distribution
  predicted_distribution = data.frame()
  plot_list = list()
  for (j in 1:length(unique(raw$age_group))){
    subplot_list = list()
    for (i in 1: length(unique(raw$dose))){
      workshop_real = raw[raw$dose == unique(raw$dose)[i] & 
                            raw$age_group == unique(raw$age_group)[j],]
      attach(workshop_real)
      model = lm(VE~days)
      #summary(model)
      model_rsquared = summary(model)$adj.r.squared
      detach(workshop_real)
      rm(workshop_real)
      
      time <- seq(0, 365)
      workshop_predicted <- predict(model,list(days=time))
      workshop_predicted = data.frame(cbind(days = time, VE = workshop_predicted))
  
      workshop_predicted = workshop_predicted %>%
        mutate(age_group = unique(raw$age_group)[j],
               dose = unique(raw$dose)[i],
               rsquared = model_rsquared )
      
      predicted_distribution = rbind(predicted_distribution,workshop_predicted)
    }
  }
  predicted_distribution = predicted_distribution %>% mutate(plot_label = paste(age_group,"(R squared",round(rsquared,digits=3)))

  
  # ggplot(data = raw[raw$age_group == 'overall',]) +
  #   geom_point(aes(x=days,y=VE,color=as.factor(dose))) +
  #   geom_errorbar(aes(x=days, ymin=LB, ymax=UB)) +
  #   geom_line(data = predicted_distribution[ predicted_distribution$age_group == 'overall',],
  #             aes(x=days,y=VE,color=as.factor(dose))) +
  #   ylim(0,1) +
  #   theme_bw() +
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         axis.line = element_line(color = 'black'))
  
  
  ###(2/3) Apply predicted distributions
  #(A) calculate internal
  apply_distribution <- predicted_distribution %>%
    mutate(outcome_family =  'severe_outcome') %>%
    group_by(age_group,dose) %>%
    mutate(VE_internal = VE / max(VE)) %>%
    ungroup() %>%
    select(dose,age_group,outcome_family,days,VE_internal)
  
  # plot_dose2 = ggplot() + 
  #   geom_line(data = apply_distribution[apply_distribution$dose == 2,], aes(x=days,y=VE_internal,color=as.factor(age_group))) +
  #   ylim(0,1)  +
  #   ylab('% of max protection') +
  #   xlab('days since vaccination') +
  #   labs(color='age group') +
  #   ggtitle('primary schedule') 
  # plot_dose3 = ggplot() + 
  #   geom_line(data = apply_distribution[apply_distribution$dose == 3,], aes(x=days,y=VE_internal,color=as.factor(age_group))) +
  #   ylim(0,1) +
  #   ylab('% of max protection') +
  #   xlab('days since vaccination') +
  #   labs(color='age group') +
  #   ggtitle('booster dose') 
  # grid.arrange(plot_dose2,plot_dose3, nrow=2)
  
  #(B) Calculate ratio between age groups
  workshop_overall = raw %>% 
    filter(age_group == 'overall') %>% 
    rename(VE_overall = VE) %>%
    select(dose,days,VE_overall)
  workshop_age = raw %>% 
    filter(! age_group == 'overall') %>%
    left_join(workshop_overall, by = c('dose','days')) %>%
    mutate(VE_overall = VE/VE_overall) 
  apply_ratio = workshop_age %>% filter(days == 22) %>% rename(VE_ratio = VE_overall,agegroup_RAW = age_group) %>% select(dose,agegroup_RAW,VE_ratio)
  
  #(C) Convert ratio to age groups in model
  CS_age_groupings = c(0,59,79,110) #age groupings in VE estimate data
  pop_RAW <- UN_pop_est %>%
    filter(ISO3_code == setting) %>%
    mutate(agegroup_RAW = cut(age,breaks = CS_age_groupings, include.lowest = T, labels = unique(apply_ratio$agegroup_RAW)),
           agegroup_MODEL = cut(age,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
    ungroup() %>%
    group_by(agegroup_MODEL) %>%
    mutate(model_group_percent = population/sum(population))
  
  apply_ratio_MODEL = pop_RAW %>% 
    left_join(apply_ratio, by = "agegroup_RAW") %>% 
    mutate(interim = model_group_percent * VE_ratio) %>%
    group_by(dose,agegroup_MODEL) %>%
    summarise(VE_ratio = sum(interim),.groups = "keep") %>%
    rename(age_group = agegroup_MODEL) %>% 
    arrange(dose) %>%
    mutate(schedule = case_when(
      dose > 2 ~ 'booster',
      TRUE ~ 'primary'
    ))  %>%  
    ungroup() %>%
    select(-dose)
  
  workshop = apply_distribution %>% 
    rename(agegroup_RAW = age_group)
  workshop = pop_RAW %>% 
    left_join(workshop, by = "agegroup_RAW") %>% 
    mutate(interim = model_group_percent * VE_internal) %>%
    group_by(dose,agegroup_MODEL,days) %>%
    summarise(VE_internal = sum(interim),.groups = "keep") %>%
    rename(age_group = agegroup_MODEL)
  
  apply_distribution_MODEL = workshop %>% arrange(dose)  %>%
    mutate(schedule = case_when(
      dose > 2 ~ 'booster',
      TRUE ~ 'primary' #copy dose 2 for dose 1
    )) %>%  
    ungroup() %>%
    select(-dose)
  
  
  #(D) apply to point estimates
  point_estimates = VE_estimates_imputed %>% 
    filter(outcome_family == 'severe_outcome' & !(vaccine_type == 'Pfizer' & dose == 3)) %>%
    select(strain,vaccine_type,dose,outcome,outcome_family,VE) %>%
    mutate(schedule = case_when(
      dose > 2 ~ 'booster',
      dose == 2 & vaccine_type == "Johnson & Johnson" ~ 'booster',
      TRUE ~ 'primary'
    ))
  point_estimates_booster = VE_booster_estimates %>% 
    filter(outcome_family == 'severe_outcome') %>%
    select(strain,vaccine_type,primary_if_booster,dose,outcome,outcome_family,VE) %>%
    mutate(schedule = 'booster')
  point_estimates = bind_rows(point_estimates,point_estimates_booster)
  
  together = point_estimates %>% 
    left_join(apply_ratio_MODEL,by='schedule') %>%
    left_join(apply_distribution_MODEL, by = c('schedule','age_group')) %>%
    rename(VE_days = VE) %>%
    mutate(VE_days = VE_days*VE_internal*VE_ratio)  %>%
    mutate(VE_days = case_when(VE_days>1 ~ 1, TRUE ~ VE_days))
  
  
  ###(3/3) Plot distributions and save VE_waning_distribution
  #(A/B) Plot
  if (exists("vax_type_list") == FALSE){  vax_type_list = c("AstraZeneca","Johnson & Johnson", "Pfizer", "Sinopharm" )}
  
  waning_to_plot = together %>%
    filter(vaccine_type %in% vax_type_list) %>%
    mutate(immunity = paste(vaccine_type,dose))
  
  strain_test = 'omicron'
  outcome_test = 'severe_disease'
  vaccine_type_test = 'Johnson & Johnson'
  #vaccine_type_test = 'Pfizer'
  
  ggplot() +
    geom_point(data=waning_to_plot[waning_to_plot$strain == strain_test  & waning_to_plot$outcome == outcome_test & waning_to_plot$vaccine_type == vaccine_type_test,],
              aes(x=days,y=VE_days,color=as.factor(age_group),shape=as.factor(dose)),na.rm=TRUE) +
    labs(title=(paste("Waning of VE against","(",strain_test,")"))) +
    xlab("days since vaccination") +
    ylab("% max protection") +
    ylim(0,1)+
    theme_bw() +
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  
  VE_waning_distribution_SO = together %>% select(strain, vaccine_type,primary_if_booster, dose, outcome, age_group,days,VE_days)
  
  workshop = VE_waning_distribution_SO %>% 
    filter(dose == 3 & strain == strain_now) %>%
    group_by(strain,outcome,vaccine_type,dose,age_group,days,.add = TRUE) %>%
    summarise(VE_days = mean(VE_days),.groups = "keep")
  VE_waning_distribution_SO = VE_waning_distribution_SO %>% filter(! dose == 3) %>% select(-primary_if_booster)
  VE_waning_distribution_SO = rbind(VE_waning_distribution_SO,workshop)

return(VE_waning_distribution_SO)
}

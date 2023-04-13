library(RColorBrewer)
options(scipen = 1000)

plot_name = "figure_2" #figure_1, figure_2, figure_S3_1_2,figure_S3_1_3,"figure_S3_2_3","figure_S3_2_1"
plot_list = list()

#LIST_outcomes = list('hosp', 'severe_disease','YLL','death') # for extended plot (SM?)
LIST_outcomes = list('hosp', 'death')
LIST_results = list("doses_per_outcome_averted","percentage" )
TOGGLE_print = "on"
TOGGLE_antiviral_type = "nirmatrelvir_ritonavir"

risk_groups_to_plot = c("adults_with_comorbidities","pregnant_women")
#risk_groups_to_plot = "adults_with_comorbidities"

#settings_to_plot = c("PNG_high_beta","PNG_low_beta")
settings_to_plot = c("FJI","PNG_low_beta")
#settings_to_plot = c("FJI")


if (plot_name ==  "figure_S3_2_3"){
  TOGGLE_antiviral_type = c("nirmatrelvir_ritonavir","molunipiravir")
  LIST_outcomes = list('severe_disease','death')
} 
if (plot_name ==  "figure_S3_2_1"){
  LIST_results = list("doses_per_outcome_averted")
}
#copy plot 800 x 600 for paper


### LOAD
MASTER_RECORD_antiviral_model_simulations = data.frame()


for (r in 1:length(risk_groups_to_plot)){
  this_risk_group = risk_groups_to_plot[r]
  for (i in 1:length(settings_to_plot)){
    this_setting = settings_to_plot[i]
    
    list_poss_Rdata = list.files(path=paste("x_results/",sep=''),pattern = paste("AntiviralRun_",this_setting,"_",this_risk_group,"*",sep=""))
    if (length(list_poss_Rdata)>0){
      list_poss_Rdata_details = double()
      for (j in 1:length(list_poss_Rdata)){
        list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                        file.info(paste('x_results/',list_poss_Rdata[[j]],sep=''))$mtime)
      }
      latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
      load(file = paste("x_results/",latest_file,sep=''))
      #load(file = paste('x_results/',"AntiviralRun_PNG_low_beta2023-02-04 11-50-47.Rdata",sep = ''))
      
      if (this_setting == "PNG_low_beta" & !("PNG_high_beta" %in% settings_to_plot)){this_setting = "PNG"}
      
      df_this_setting = RECORD_antiviral_model_simulations %>% mutate(setting_beta = this_setting)
      MASTER_RECORD_antiviral_model_simulations = bind_rows(MASTER_RECORD_antiviral_model_simulations,df_this_setting)
    }
  }
}

RECORD_antiviral_model_simulations = MASTER_RECORD_antiviral_model_simulations %>%
  mutate(vax_scenario_short = case_when(
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to all",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to all low-uptake",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to high-risk",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to high-risk low-uptake",
    vax_scenario == "all willing adults vaccinated with a primary schedule" ~ 
      "no booster",
    vax_scenario =="catchup campaign for high-risk adults: assume booster to high-risk adults who have previously completed their primary schedule but have not recieved a booster" ~
      "catch-up campaign high-risk",
    vax_scenario == "catchup campaign for all adults: assume booster to all adults who have previously completed their primary schedule but have not recieved a booster" ~
      "catch-up campaign all adults",
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: prioritise delivery to high-risk adults"  ~
      "booster to all (prioritised)"
  )) %>%
  mutate(vax_scenario_short_v2 = case_when(
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~
      "(all) prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a first booster dose" ~
      "(all) prev first booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~
      "(high-risk) prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a first booster dose" ~
      "(high-risk) prev first booster"
  ))
#_______________________________________________________________________________



### FIGURE 1 family
if (plot_name %in% c("figure_1",
                     "figure_S3_1_2", # prioritised
                     "figure_S3_1_3"  # only same subset get vaxed again
                     )){
  if (plot_name == "figure_1"){
    LIST_vax_scenarios = c(
      "catch-up campaign all adults",
      "booster to all",
      "catch-up campaign high-risk",
      "booster to high-risk",
      "no booster")
  } else if (plot_name == "figure_S3_1_2"){
    LIST_vax_scenarios = c(
      "booster to all (prioritised)",
      "booster to all",
      "booster to high-risk",
      "no booster")
  } else if (plot_name == "figure_S3_1_3"){
    LIST_vax_scenarios = c("(all) prev first booster",
      "(all) prev primary",
      "(high-risk) prev first booster",
      "(high-risk) prev primary")
    
    RECORD_antiviral_model_simulations = RECORD_antiviral_model_simulations %>% mutate(vax_scenario_short = vax_scenario_short_v2)
  }

  
  workshop_this_plot = RECORD_antiviral_model_simulations %>% 
    filter(vax_scenario_risk_group == "adults_with_comorbidities"  &
             (antiviral_type %in% TOGGLE_antiviral_type | intervention == 'vaccine') &
             (antiviral_target_group == 'adults_with_comorbidities' | intervention == 'vaccine') *
             (evaluation_group == 'pop_level')  &
             (vax_scenario_short %in% LIST_vax_scenarios)) %>%
    mutate(intervention = case_when(
      intervention == 'vaccine' ~ paste('booster dose'),
      intervention ==  "antiviral 2023-01-01" ~  "oral antiviral",
      TRUE ~ intervention
    ))  %>%
    filter(intervention %in% c('booster dose','oral antiviral')) %>%
    mutate(outcome_extended = case_when(outcome == 'hosp' ~ 'hospitalisation',
                                        TRUE ~ outcome))
  workshop_this_plot$vax_scenario_short = factor(workshop_this_plot$vax_scenario_short, levels = LIST_vax_scenarios)
  
  for (a in 1:length(LIST_results)) {
    this_result = LIST_results[[a]]
    if (this_result == "doses_per_outcome_averted"){this_result_label = "individual-level impact"}
    if (this_result == "percentage"){this_result_label = "population-level impact"}
    
    workshop = workshop_this_plot %>% 
      filter(result == this_result) %>%
      group_by(setting_beta,intervention,outcome_extended,outcome,antiviral_type,antiviral_target_group,evaluation_group,vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
      summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75), .groups = "keep")
    
    if (this_result == "percentage"){
      total = workshop %>%
        ungroup() %>%
        group_by(setting_beta,outcome_extended,outcome,
                 evaluation_group,vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
        summarise(median = sum(median), LQ = sum(LQ), UQ = sum(UQ), .groups = "keep") %>%
        mutate(intervention = 'total')%>%
        filter(vax_scenario_short != "no booster")
      workshop = bind_rows(workshop,total)
    } else{
      total = workshop %>%
        ungroup() %>%
        group_by(setting_beta,outcome_extended,outcome,
                 evaluation_group,vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
        summarise(median = NA, LQ = NA, UQ = NA, .groups = "keep") %>% #so included in legend!
        mutate(intervention = 'total', .groups = "keep") %>%
        filter(vax_scenario_short != "no booster")
      workshop = bind_rows(workshop,total)
    }
    
    plot_list[[a]] = ggplot(data = workshop[workshop$outcome %in% LIST_outcomes,]) +
      geom_pointrange(aes(x=median,y=vax_scenario_short,
                          xmin=LQ,xmax=UQ,
                          color=as.factor(intervention)))  +
      labs(title = paste(this_result_label),
           color = 'intervention',
           shape = "setting") +
      ylab('')+
      #xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
      theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.box = "vertical") +
      facet_grid(vars(setting_beta),vars(outcome_extended), scales = "free") +
      scale_color_manual(name = "intervention",values = c("#619CFF","#00BA38" ,"#BEBEBE")) 
    
    if(this_result == "doses_per_outcome_averted"){
      plot_list[[a]]  = plot_list[[a]]  + 
        xlab('doses to avert an outcome') +
        scale_x_reverse()
      
      to_print = workshop %>% 
        ungroup()%>%
        filter(outcome %in% LIST_outcomes) %>%
        select(setting_beta,outcome,vax_scenario_short,intervention,median,LQ,UQ) %>%
        mutate(dose_impact = paste(round(median)," (",round(LQ),"-",round(UQ),")",sep="")) %>%
        select(-median,-LQ,-UQ)
    }
    
    if(this_result == "percentage"){
      plot_list[[a]]  = plot_list[[a]]  + xlab('outcomes averted (%)')
      
      workshop = workshop %>% 
        ungroup()%>%
        filter(outcome %in% LIST_outcomes) %>%
        select(setting_beta,outcome,vax_scenario_short,intervention,median,LQ,UQ) %>%
        mutate(pop_level_impact = paste(round(median)," (",round(LQ),"-",round(UQ),")",sep="")) %>%
        select(-median,-LQ,-UQ) 
      to_print = cbind(to_print,pop_level_impact = workshop$pop_level_impact)
      
      
    }
    
    

    
  }
  if (length(LIST_outcomes) == 2){
    saved_plot = ggarrange(plot_list[[1]],plot_list[[2]],
              common.legend = TRUE,
              legend="bottom",
              ncol = 1,
              nrow = length(LIST_outcomes)) 
  } else if (length(LIST_outcomes) == 4){
    saved_plot = ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],
            common.legend = TRUE,
            legend="bottom",
            ncol = 1,
            nrow = length(LIST_outcomes)) 
  }
  
  if (TOGGLE_print == "on"){
    #save values plotted to allow reference in text
    to_print$vax_scenario_short = factor(to_print$vax_scenario_short, levels = LIST_vax_scenarios)
    temp_name = ''
    time = Sys.time()
    time = gsub(':','-',time)
    time = paste(temp_name,time,sep='')
    write.csv(to_print,paste(rootpath,"x_results/",plot_name,"_",time,".csv",sep=''))
  }
}

#_______________________________________________________________________________



### FIGURE 2 family
if (plot_name %in% c("figure_2",
                     "figure_S3_2_3",
                     "figure_S3_2_1"
)){
  
  LIST_vax_scenarios = c("booster to high-risk",
                         "no booster")
  
  if (plot_name == "figure_2"){
    workshop_this_plot = RECORD_antiviral_model_simulations %>% 
      filter(vax_scenario_risk_group == "adults_with_comorbidities" | (vax_scenario_risk_group == "pregnant_women" & antiviral_target_group == "pregnant_women"))%>%
      filter(intervention == "antiviral 2023-01-01")
  } else if (plot_name == "figure_S3_2_3"){
    workshop_this_plot = RECORD_antiviral_model_simulations %>% 
      filter(antiviral_target_group == "adults_with_comorbidities") %>%
      mutate(antiviral_target_group = antiviral_type) %>% #so colour on plot
      filter(intervention == "antiviral 2023-01-01")
  } else if (plot_name == "figure_S3_2_1"){
    workshop_this_plot = RECORD_antiviral_model_simulations %>% 
      filter(antiviral_target_group == "adults_with_comorbidities") %>% 
      filter(intervention %in% c("antiviral 2023-01-01","antiviral prior to booster 2023-01-01", "antiviral after booster 2023-01-01"))  %>%
      mutate(antiviral_target_group = intervention) #so colour on plot
  }
  
  workshop_this_plot = workshop_this_plot%>%
    filter(antiviral_type %in% TOGGLE_antiviral_type) %>%
    filter(antiviral_target_group != "unvaccinated_adults_AND_adults_with_comorbidities") %>%
    filter(evaluation_group == 'pop_level')  %>%
    filter(vax_scenario_short %in% c("booster to high-risk",
                                        "no booster"))  %>% 
      mutate(antiviral_target_group = case_when(
      antiviral_target_group == "adults_with_comorbidities" ~ "high-risk",
      antiviral_target_group == "all_adults" ~ "all",
      antiviral_target_group == "unvaccinated_adults" ~ "unvaccinated",
      antiviral_target_group == "pregnant_women" ~ "pregnant_women",
      TRUE ~ antiviral_target_group
    )) %>%
    mutate(outcome_extended = case_when(outcome == 'hosp' ~ 'hospitalisation',
                                        TRUE ~ outcome))
  workshop_this_plot$vax_scenario_short = factor(workshop_this_plot$vax_scenario_short, levels = LIST_vax_scenarios)
  
  #join on to UN_pop_est by country
  load(file = "1_inputs/UN_world_population_prospects/UN_pop_est.Rdata")
  UN_total_pop <- UN_pop_est %>% 
    rename(country = ISO3_code) %>%
    filter(country %in% unique(workshop_this_plot$country)) %>%
    group_by(country) %>%
    summarise(pop = sum(PopTotal))
  
  plot_list = list()
  for (a in 1:length(LIST_results)) {
    this_result = LIST_results[[a]]
    if (this_result == "doses_per_outcome_averted"){this_result_label = "individual-level impact"}
    if (this_result == "percentage"){this_result_label = "population-level impact"}
    
    workshop = workshop_this_plot %>% 
      filter(result == this_result) %>%
      group_by(setting_beta,country,intervention,outcome,outcome_extended,antiviral_type,antiviral_target_group,evaluation_group,
             vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
      summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75),
                LQ_doses = quantile(intervention_doses_delivered,probs=0.25,na.rm=TRUE), UQ_doses = quantile(intervention_doses_delivered,probs=0.75,na.rm=TRUE),
                intervention_doses_delivered = median(intervention_doses_delivered,na.rm=TRUE), .groups = "keep") %>%
      left_join(UN_total_pop, by = 'country') %>%
      mutate(antiviral_doses_per_100 = intervention_doses_delivered / pop * 100 )
      
      plot_list[[a]]  = ggplot(data = workshop[workshop$outcome %in% LIST_outcomes,]) +
        geom_pointrange(aes(x=median,y=vax_scenario_short,
                            xmin=LQ,xmax=UQ,
                            #size = antiviral_doses_per_100,
                            color=as.factor(antiviral_target_group),
        ))  +
        labs(title = paste(this_result_label),
             color = 'antiviral eligiblity',
             #size = 'coverage'
             ) +
        # scale_size(range=c(0,2),
        #            breaks=c(0.5,1,5,10,20),
        #            labels=c("0.5","1","5","10","20")) +
        theme_bw()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "bottom",
              legend.box = "vertical") +
        facet_grid(vars(setting_beta),vars(outcome_extended), scales = "free")  +
        xlab('outcomes averted (%)') +
        ylab('') +
        scale_color_manual(values = c("#BEBEBE", # grey - all
                                      "#00BA38", # green - high risk
                                      "#FF61CC", # pink - pregnant owmen
                                      "#C77CFF"  # purple - unvaccinated
        )) 
  
      if(this_result == "doses_per_outcome_averted"){
        plot_list[[a]]  = plot_list[[a]]  + 
          xlab('doses to avert an outcome') +
          scale_x_reverse()
        
        to_print = workshop %>% 
          ungroup()%>%
          filter(outcome %in% LIST_outcomes) %>%
          select(setting_beta,outcome,vax_scenario_short,antiviral_target_group,intervention_doses_delivered,antiviral_doses_per_100,median,LQ,UQ) %>%
          mutate(dose_impact = paste(round(median)," (",round(LQ),"-",round(UQ),")",sep="")) %>%
          select(-median,-LQ,-UQ)
      }
      
      if(this_result == "percentage"){
      plot_list[[a]]  = plot_list[[a]]  + xlab('outcomes averted (%)') +xlim(0,max(workshop$UQ[workshop$outcome %in% LIST_outcomes])) 
      
      workshop = workshop %>% 
        ungroup()%>%
        filter(outcome %in% LIST_outcomes) %>%
        select(setting_beta,outcome,vax_scenario_short,antiviral_target_group,intervention_doses_delivered,antiviral_doses_per_100,median,LQ,UQ) %>%
        mutate(pop_level_impact = paste(round(median)," (",round(LQ),"-",round(UQ),")",sep="")) %>%
        select(-median,-LQ,-UQ) 
      to_print = cbind(to_print,pop_level_impact = workshop$pop_level_impact)
      
      
      }

      if (plot_name == "figure_S3_2_3"){
        plot_list[[a]]  = plot_list[[a]]  + labs(color='') +
          scale_color_manual(values = c("#FF61CC", # grey - all
                                        "#00BA38")) # green - high risk
      }
    

  }
  if (plot_name == "figure_S3_2_1"){
    saved_plot = ggarrange(plot_list[[1]],
                           common.legend = TRUE,
                           legend="bottom",
                           ncol = 1,
                           nrow = length(LIST_results)) 
  } else{
    saved_plot = ggarrange(plot_list[[1]],plot_list[[2]],
                           common.legend = TRUE,
                           legend="bottom",
                           ncol = 1,
                           nrow = length(LIST_results)) 
  }
  
  if (TOGGLE_print == "on"){
    #save values plotted to allow reference in text
    
    to_print$vax_scenario_short = factor(to_print$vax_scenario_short, levels = LIST_vax_scenarios)
    temp_name = ''
    time = Sys.time()
    time = gsub(':','-',time)
    time = paste(temp_name,time,sep='')
    write.csv(to_print,paste(rootpath,"x_results/",plot_name,"_",time,".csv",sep=''))
  }


}
#_______________________________________________________________________________

saved_plot


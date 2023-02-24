
### LOAD
MASTER_RECORD_antiviral_model_simulations = data.frame()
settings_to_plot = c("FJI","PNG_low_beta")
risk_groups_to_plot = c("adults_with_comorbidities")

for (r in 1:length(risk_groups_to_plot)){
  this_risk_group = risk_groups_to_plot[r]
  for (i in 1:length(settings_to_plot)){
    this_setting = settings_to_plot[i]
    
    list_poss_Rdata = list.files(path=paste(rootpath,"x_results/",sep=''),pattern = paste("AntiviralRun_",this_setting,"_",this_risk_group,"*",sep=""))
    if (length(list_poss_Rdata)>0){
      list_poss_Rdata_details = double()
      for (j in 1:length(list_poss_Rdata)){
        list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                        file.info(paste(rootpath,'x_results/',list_poss_Rdata[[j]],sep=''))$mtime)
      }
      latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
      load(file = paste(rootpath,"x_results/",latest_file,sep=''))
      #load(file = paste(rootpath,'x_results/',"AntiviralRun_PNG_low_beta2023-02-04 11-50-47.Rdata",sep = ''))
      
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
      "catch-up campaign all adults"
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



### FIGURE 1
LIST_outcomes = list('hosp', 'severe_disease','YLL','death') # for extended plot (SM?)
LIST_outcomes = list('hosp', 'death')

this_result = "doses_per_outcome_averted" #left-hand side of figure
this_result = "percentage"                #right-hand side of figure

workshop = RECORD_antiviral_model_simulations %>% 
  filter(vax_scenario_risk_group == "adults_with_comorbidities") %>%
  filter(antiviral_type == "nirmatrelvir_ritonavir" | intervention == 'vaccine') %>%
  filter(antiviral_target_group == 'adults_with_comorbidities' | intervention == 'vaccine') %>%
  filter( evaluation_group == 'pop_level') %>% #change eval group here to change from high-risk to pop-level plot
  filter(result == this_result) %>%
  filter(intervention != "antiviral 2023-07-01") %>%
  filter( vax_scenario_short %in% c("catch-up campaign all adults",
                                    "booster to all",
                                    "catch-up campaign high-risk",
                                    "booster to high-risk",
                                    "no booster")) %>%
  mutate(intervention = case_when(
    intervention == 'vaccine' ~ paste('booster dose'),
    intervention ==  "antiviral 2023-01-01" ~  "oral antiviral",
    TRUE ~ intervention
  )) %>% group_by(setting_beta,intervention,outcome,antiviral_type,antiviral_target_group,evaluation_group,vax_scenario,vax_scenario_risk_group,result,vax_scenario_short,vax_scenario_short) %>%
  summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75))
workshop$vax_scenario_short = factor(workshop$vax_scenario_short, levels = c("catch-up campaign all adults",
                                                                                   "booster to all",
                                                                                   "catch-up campaign high-risk",
                                                                                   "booster to high-risk",
                                                                                   "no booster"
                                                                                   ))
#save values plotted to allow reference in text
to_print = workshop %>% 
  ungroup()%>%
  filter(outcome %in% LIST_outcomes) %>%
  select(setting_beta,outcome,vax_scenario_short,intervention,median,LQ,UQ) %>%
  mutate(output = paste(round(median)," (",round(LQ),"-",round(UQ),")",sep="")) %>%
  select(-median,-LQ,-UQ)
to_print$vax_scenario_short = factor(to_print$vax_scenario_short, levels = c("catch-up campaign all adults",
                                                                             "booster to all",
                                                                             "catch-up campaign high-risk",
                                                                             "booster to high-risk",
                                                                             "no booster"
))
temp_name = ''
time = Sys.time()
time = gsub(':','-',time)
time = paste(temp_name,time,sep='')
#write.csv(to_print,paste(rootpath,"x_results/export_for_table",time,".csv",sep=''))

options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  plot_list[[a]] = ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_pointrange(aes(x=median,y=vax_scenario_short,
                        xmin=LQ,xmax=UQ,
                        color=as.factor(intervention)))  +
    labs(title = paste(this_outcome),
         color = 'intervention',
         shape = "setting") +
    ylab('')+
    xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical") +
    facet_grid(setting_beta ~.)
  if(this_result == "percentage"){plot_list[[a]]  = plot_list[[a]]  + xlab('outcomes averted (%)')}
  if(this_result == "doses_per_outcome_averted"){plot_list[[a]]  = plot_list[[a]]  + xlab('doses to avert an outcome')}
  
}
ggarrange(plot_list[[1]],plot_list[[2]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = length(LIST_outcomes)) 
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],#plot_list[[5]],plot_list[[6]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = length(LIST_outcomes)) 
#ggsave(paste(rootpath,"x_results/plot_VaxVsAntivirals_",time,".png",sep=''), width = 9.6, height = 5.7)
options(warn = 0)
#copy plot 500 x 700 for paper, 500 x 900 for additional two outcomes
#_______________________________________________________________________________



### SM - lower vaccine acceptance
LIST_outcomes = list('hosp', 'death')

this_result = "doses_per_outcome_averted" #left-hand side of figure
this_result = "percentage"                #right-hand side of figure

workshop = RECORD_antiviral_model_simulations %>% 
  filter(vax_scenario_risk_group == "adults_with_comorbidities") %>%
  filter(antiviral_type == "nirmatrelvir_ritonavir" | intervention == 'vaccine') %>%
  filter(antiviral_target_group == 'adults_with_comorbidities' | intervention == 'vaccine') %>%
  filter( evaluation_group == 'pop_level') %>% #change eval group here to change from high-risk to pop-level plot
  filter(result == this_result) %>%
  filter(intervention != "antiviral 2023-07-01") %>%
  filter( is.na(vax_scenario_short_v2) == FALSE) %>%
  mutate(intervention = case_when(
    intervention == 'vaccine' ~ paste('booster dose'),
    intervention ==  "antiviral 2023-01-01" ~  "oral antiviral",
    TRUE ~ intervention
  )) %>% group_by(setting_beta,intervention,outcome,antiviral_type,antiviral_target_group,evaluation_group,vax_scenario_risk_group,result,vax_scenario_short_v2) %>%
  summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75))
workshop$vax_scenario_short_v2 = factor(workshop$vax_scenario_short_v2, levels = c("(all) prev first booster",
                                                                             "(all) prev primary",
                                                                             "(high-risk) prev first booster",
                                                                             "(high-risk) prev primary"
))


options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  plot_list[[a]] = ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_pointrange(aes(x=median,y=vax_scenario_short_v2,
                        xmin=LQ,xmax=UQ,
                        color=as.factor(intervention)))  +
    labs(title = paste(this_outcome),
         color = 'intervention',
         shape = "setting") +
    ylab('')+
    xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical") +
    facet_grid(setting_beta ~.)
  if(this_result == "percentage"){plot_list[[a]]  = plot_list[[a]]  + xlab('outcomes averted (%)')}
  if(this_result == "doses_per_outcome_averted"){plot_list[[a]]  = plot_list[[a]]  + xlab('doses to avert an outcome')}
  
}
ggarrange(plot_list[[1]],plot_list[[2]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = length(LIST_outcomes)) 
options(warn = 0)
#copy plot 500 x 700 for paper, 500 x 900 for additional two outcomes
#_______________________________________________________________________________



### FIGURE 2
this_result = "percentage"
this_result = "doses_per_outcome_averted"
workshop = RECORD_antiviral_model_simulations %>% 
  filter(vax_scenario_risk_group == "adults_with_comorbidities") %>%
  filter(antiviral_type == "nirmatrelvir_ritonavir") %>%
  filter(antiviral_target_group != "unvaccinated_adults_AND_adults_with_comorbidities") %>%
  filter( evaluation_group == 'pop_level') %>% #change eval group here to change from high-risk to pop-level plot
  filter(result == this_result) %>%
  filter(intervention == "antiviral 2023-01-01") %>%
  filter(vax_scenario_short %in% c("booster to high-risk",
                                      "no booster"))  %>% 
  group_by(setting_beta,intervention,outcome,antiviral_type,antiviral_target_group,evaluation_group,
           vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
  summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75),
            LQ_doses = quantile(intervention_doses_delivered,probs=0.25), UQ_doses = quantile(intervention_doses_delivered,probs=0.75),intervention_doses_delivered = median(intervention_doses_delivered)) %>%
  mutate(antiviral_target_group = case_when(
    antiviral_target_group == "adults_with_comorbidities" ~ "high-risk",
    antiviral_target_group == "all_adults" ~ "all",
    antiviral_target_group == "unvaccinated_adults" ~ "unvaccinated"
  ))
workshop$vax_scenario_short = factor(workshop$vax_scenario_short, levels = c("booster to high-risk",
                                                                                   "no booster"
))

#save values plotted to allow reference in text
to_print = workshop %>% 
  ungroup()%>%
  filter(outcome %in% LIST_outcomes) %>%
  select(setting_beta,outcome,vax_scenario_short,antiviral_target_group,median,LQ,UQ) %>%
  mutate(output = paste(round(median)," (",round(LQ),"-",round(UQ),")",sep="")) %>%
  select(-median,-LQ,-UQ)
to_print$vax_scenario_short = factor(to_print$vax_scenario_short, levels = c("catch-up campaign all adults",
                                                                             "booster to all",
                                                                             "catch-up campaign high-risk",
                                                                             "booster to high-risk",
                                                                             "no booster"
))
temp_name = ''
time = Sys.time()
time = gsub(':','-',time)
time = paste(temp_name,time,sep='')
#write.csv(to_print,paste(rootpath,"x_results/export_for_table",time,".csv",sep=''))

options(scipen = 1000)
options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  if(this_result == "percentage"){
    plot_list[[a]]  = ggplot(data = workshop[workshop$outcome == this_outcome,]) +
      geom_pointrange(aes(x=median,y=intervention_doses_delivered,
                          xmin=LQ,xmax=UQ,
                          
                          color=as.factor(antiviral_target_group),
                          shape = as.factor(setting_beta)
      ))  + 
      geom_pointrange(aes(x=median,y=intervention_doses_delivered,
                          ymin = LQ_doses,ymax=UQ_doses,
                          
                          color=as.factor(antiviral_target_group),
                          shape = as.factor(setting_beta)
      )) +
      labs(title = paste(this_outcome),
           color = 'antiviral eligiblity',
           shape = "setting") +
      ylab('oral antiviral schedules delivered')+
      xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
      ylim(0,max(workshop$intervention_doses_delivered[workshop$outcome == this_outcome],na.rm=TRUE)) + 
      theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.box = "vertical") +
      facet_grid(vax_scenario_short ~.)  +
      xlab('outcomes averted (%)')
    }
  if(this_result == "doses_per_outcome_averted"){
    plot_list[[a]]  = ggplot(data = workshop[workshop$outcome == this_outcome,]) +
      geom_pointrange(aes(x=median,y=setting_beta,
                          xmin=LQ,xmax=UQ,
                          
                          color=as.factor(antiviral_target_group)
      )) +
      labs(title = paste(this_outcome),
           color = 'antiviral eligiblity') +
      xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
      ylab('') + 
      theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.box = "vertical") +
      facet_grid(vax_scenario_short ~.) + 
      xlab('doses to avert an outcome')
  }
  
}
ggarrange(plot_list[[1]],plot_list[[2]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = length(LIST_outcomes)) 
#ggsave(paste(rootpath,"x_results/plot_VaxVsAntivirals_",time,".png",sep=''), width = 9.6, height = 5.7)
options(warn = 0)
#_______________________________________________________________________________




### SM timing of antivirals
LIST_outcomes = list('hosp', 'death')
this_result = "percentage"
this_result = "doses_per_outcome_averted"

workshop = RECORD_antiviral_model_simulations %>% 
  filter(vax_scenario_risk_group == "adults_with_comorbidities") %>%
  filter(antiviral_type == "nirmatrelvir_ritonavir") %>%
  filter(antiviral_target_group == 'adults_with_comorbidities') %>%
  filter( evaluation_group == 'pop_level') %>% #change eval group here to change from high-risk to pop-level plot
  filter(result == this_result) %>%
  filter(intervention %in% c("antiviral 2023-01-01","antiviral 2023-07-01")) %>%
  filter(vax_scenario_short %in% c("catch-up campaign all adults",
                                   "booster to all",
                                   "catch-up campaign high-risk",
                                   "booster to high-risk",
                                   "no booster"
  ))  %>% 
  group_by(setting_beta,intervention,outcome,antiviral_type,antiviral_target_group,evaluation_group,
           vax_scenario,vax_scenario_risk_group,result,vax_scenario_short) %>%
  summarise(median = median(value), LQ = quantile(value,probs=0.25), UQ = quantile(value,probs=0.75),
            LQ_doses = quantile(intervention_doses_delivered,probs=0.25), UQ_doses = quantile(intervention_doses_delivered,probs=0.75),intervention_doses_delivered = median(intervention_doses_delivered)) %>%
  mutate(antiviral_target_group = case_when(
    antiviral_target_group == "adults_with_comorbidities" ~ "high-risk",
    antiviral_target_group == "all_adults" ~ "all",
    antiviral_target_group == "unvaccinated_adults" ~ "unvaccinated"
  ))
workshop$vax_scenario_short = factor(workshop$vax_scenario_short, levels = c("catch-up campaign all adults",
                                                                             "booster to all",
                                                                             "catch-up campaign high-risk",
                                                                             "booster to high-risk",
                                                                             "no booster"
))


options(warn = -1)
plot_list = list()
for (a in 1:length(LIST_outcomes)) {
  this_outcome = LIST_outcomes[[a]]
  
  plot_list[[a]] = ggplot(data = workshop[workshop$outcome == this_outcome,]) +
    geom_pointrange(aes(x=median,y=vax_scenario_short,
                        xmin=LQ,xmax=UQ,
                        color=as.factor(intervention)))  +
    labs(title = paste(this_outcome),
         color = 'intervention',
         shape = "setting") +
    ylab('')+
    xlim(0,max(workshop$UQ[workshop$outcome == this_outcome])) +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical") +
    facet_grid(setting_beta ~.)
  if(this_result == "percentage"){plot_list[[a]]  = plot_list[[a]]  + xlab('outcomes averted (%)')}
  if(this_result == "doses_per_outcome_averted"){plot_list[[a]]  = plot_list[[a]]  + xlab('doses to avert an outcome')}
  
}
ggarrange(plot_list[[1]],plot_list[[2]],
          common.legend = TRUE,
          legend="bottom",
          ncol = 1,
          nrow = length(LIST_outcomes)) 

options(warn = 0)
#_______________________________________________________________________________
#_______________________________________________________________________________




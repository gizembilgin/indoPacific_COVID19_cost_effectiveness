this_risk_group_name = "adults_with_comorbidities"

seroprevalence_tracker = data.frame()

for (setting_beta in c("PNG_low_beta","IDN","FJI","TLS")){
  
  this_setting = setting_beta
  if (setting_beta == "PNG_low_beta"){this_setting = "PNG"}
  
  list_poss_Rdata = list.files(path="01_inputs/fit/",pattern = paste("fitted_results_",setting_beta,"*",sep=""))
  list_poss_Rdata_details = double()
  for (i in 1:length(list_poss_Rdata)){
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste("01_inputs/fit/",list_poss_Rdata[[i]],sep=''))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(paste('01_inputs/fit/',latest_file,sep=''))
  
  fitted_next_state = fitted_results[[2]] 
  
  seroprevalence = fitted_next_state %>% 
    group_by(age_group,class) %>%
    summarise(pop = sum(pop)) %>%
    group_by(age_group) %>%
    mutate(seroprevalence = pop/sum(pop)) %>%
    filter(class == "R") %>%
    mutate(setting = this_setting)
  seroprevalence_tracker = rbind(seroprevalence_tracker,seroprevalence)
}

ggplot(seroprevalence_tracker) + 
  geom_col(aes(x=age_group,y=seroprevalence,fill = as.factor(setting)),position = "dodge") +
  xlab("age group") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical") + 
  labs(fill = "")

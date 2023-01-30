
check_sampled_severe_outcome_country_level = data.frame()
check_sampled_VE_waning_distribution = data.frame()
check_sampled_rho_SO_est = data.frame()

for (this_sample in c(1:100)){
  SO_sample = stochastic_severe_outcomes_sampling( booster_combinations = booster_combinations,
                                                 setting = setting,
                                                 vaccine_type_list = input_vaccine_type_list,
                                                 risk_group_name = unique(manager_scenario_dataframe$vax_scenario_risk_group),
                                                 local_stochastic_VE_sampling = worker_stochastic_VE_sampling)
 sampled_severe_outcome_country_level = SO_sample$SAVE_severe_outcome_country_level
 sampled_VE_waning_distribution = SO_sample$SAVE_VE_waning_distribution
 sampled_rho_SO_est = SO_sample$SAVE_rho_SO_est
 
 check_sampled_severe_outcome_country_level = rbind(check_sampled_severe_outcome_country_level,sampled_severe_outcome_country_level)
 check_sampled_VE_waning_distribution = rbind(check_sampled_VE_waning_distribution,sampled_VE_waning_distribution)
 check_sampled_rho_SO_est = rbind(check_sampled_rho_SO_est,sampled_rho_SO_est)
 
 if (sampled_rho_SO_est<0){stop('sampled_rho_SO_est<0')}
 if (nrow(sampled_VE_waning_distribution[sampled_VE_waning_distribution$VE_days<0,])>0){stop('sampled VE<0')}
 if (nrow(sampled_severe_outcome_country_level[sampled_severe_outcome_country_level$percentage<0 & sampled_severe_outcome_country_level$percentage>1,])>0){stop('sampled sampled_severe_outcome_country_level<0')}
 
 workshop = sampled_VE_waning_distribution %>%
   select(-schedule) %>%
   pivot_wider(names_from = dose,
               names_prefix = "dose_",
               values_from = VE_days) %>%
   filter(dose_3<dose_2|dose_2<dose_1|(is.na(dose_4) == FALSE & dose_4<dose_3))
 
 if (nrow(workshop)>0){stop('next dose lower VE than previous')}
 
}

options(warn = 2)
for (this_sample in c(1:100)){
  VE_waning_distribution = stochastic_VE(booster_combinations = booster_combinations,
                                         setting = setting,
                                         toggle_sampling = local_stochastic_VE_sampling) %>%
    filter(strain == strain_now & vaccine_type %in% vaccine_type_list)
  
  workshop = VE_waning_distribution %>%
    select(-schedule) %>%
    pivot_wider(names_from = dose,
                names_prefix = "dose_",
                values_from = VE_days) %>%
    filter(dose_3<dose_2|dose_2<dose_1|(is.na(dose_4) == FALSE & dose_4<dose_3))
  
  if (nrow(workshop)>0){stop('next dose lower VE than previous')}

}
options(warn = 0)


VE_waning_distribution_SO %>%
  select(-schedule) %>%
  pivot_wider(names_from = dose,
              names_prefix = "dose_",
              values_from = VE_days) %>%
  filter(dose_3<dose_2|dose_2<dose_1|(is.na(dose_4) == FALSE & dose_4<dose_3))

point_estimates%>%
  pivot_wider(names_from = dose,
              names_prefix = "dose_",
              values_from = VE) %>%
  filter((is.na(dose_2) == FALSE & dose_2<dose_1)|(is.na(dose_3) == FALSE & dose_3<dose_2)|(is.na(dose_4) == FALSE & dose_4<dose_3))

together%>%
  select(-schedule) %>%
  pivot_wider(names_from = dose,
              names_prefix = "dose_",
              values_from = VE_days) %>%
  filter(dose_3<dose_2|dose_2<dose_1|(is.na(dose_4) == FALSE & dose_4<dose_3))

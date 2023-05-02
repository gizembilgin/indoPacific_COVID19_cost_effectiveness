for (i in 1:5){
  for (run_number in 1:nrow(manager_scenario_dataframe)){
    
    toggle_vax_scenario            = manager_scenario_dataframe$vax_scenario[run_number]
    toggle_vax_scenario_risk_group = manager_scenario_dataframe$vax_scenario_risk_group[run_number]
    this_vaccination_history_FINAL = RECORD_antiviral_setup$vaccination_history_FINAL %>%
      filter(
        vax_scenario            == toggle_vax_scenario,
        vax_scenario_risk_group == toggle_vax_scenario_risk_group
      )
    this_incidence_log_tidy = RECORD_antiviral_setup$incidence_log_tidy %>%
      filter(
        vax_scenario            == toggle_vax_scenario,
        vax_scenario_risk_group == toggle_vax_scenario_risk_group
      )
    this_exposed_log= RECORD_antiviral_setup$exposed_log %>%
      filter(
        vax_scenario            == toggle_vax_scenario,
        vax_scenario_risk_group == toggle_vax_scenario_risk_group
      )
    
    SO_sample = stochastic_severe_outcomes_sampling( booster_combinations = booster_combinations,
                                                     setting = setting,
                                                     vaccine_type_list = input_vaccine_type_list,
                                                     risk_group_name = unique(manager_scenario_dataframe$vax_scenario_risk_group),
                                                     local_stochastic_VE_sampling = worker_stochastic_VE_sampling)
    #sampled_severe_outcome_country_level = SO_sample$SAVE_severe_outcome_country_level
    sampled_VE_waning_distribution = SO_sample$SAVE_VE_waning_distribution
    #sampled_rho_SO_est = SO_sample$SAVE_rho_SO_est
    

    
    check = sampled_VE_waning_distribution %>% filter(dose == 3 & vaccine_type == "Pfizer" & outcome != "death")
    
    if (nrow(check) == 0){stop("here!")}
    # SO_applied = stochastic_severe_outcomes_application (
    #   incidence_log_tidy = this_incidence_log_tidy,
    #   vaccination_history_FINAL = this_vaccination_history_FINAL,
    #   exposed_log = this_exposed_log,
    #   
    #   risk_group_name = toggle_vax_scenario_risk_group,
    #   prop_sympt_LOCAL = prop_sympt,
    #   VE_waning_distribution = sampled_VE_waning_distribution,
    #   severe_outcome_country_level = sampled_severe_outcome_country_level,
    #   rho_SO_est = sampled_rho_SO_est
    # )
  }
}

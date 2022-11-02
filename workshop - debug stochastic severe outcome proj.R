antiviral_model(toggle_antiviral_start_date = as.Date('2023-01-01'), 
                            toggle_antiviral_type = 'paxlovid',
                            toggle_antiviral_target = "adults_with_comorbidities",
                            toggle_vax_scenario = 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster',
                            toggle_vax_scenario_risk_group = "adults_with_comorbidities",
                            
                            RECORD_antiviral_setup,
                            
                            toggle_number_of_runs = 1,
                            toggle_stochastic_SO = "on",
                            toggle_compare_to_vaccine_effect = "off",
                            toggle_antiviral_delivery_capacity = NA,
                            toggle_fixed_antiviral_coverage = 0.2)


toggle_antiviral_start_date = as.Date('2023-01-01') 
toggle_antiviral_type = 'paxlovid'
toggle_antiviral_target = "adults_with_comorbidities"
toggle_vax_scenario = 'all willing adults vaccinated with a primary schedule and high risk group recieve a booster'
toggle_vax_scenario_risk_group = "adults_with_comorbidities"

toggle_number_of_runs = 1
toggle_stochastic_SO = "on"
toggle_compare_to_vaccine_effect = "off"
toggle_antiviral_delivery_capacity = NA
toggle_fixed_antiviral_coverage = 0.2



system.time({load_stochastic_SO = stochastic_severe_outcomes(
  incidence_log = this_incidence_log,
  incidence_log_tidy = this_incidence_log_tidy,
  vaccination_history_FINAL = vaccination_history_FINAL,
  exposed_log = this_exposed_log,
  
  setting = 'SLE',
  num_time_steps = 365,
  strain_now = 'omicron',
  risk_group_name = toggle_vax_scenario_risk_group,
  date_start = toggle_antiviral_start_date
)})

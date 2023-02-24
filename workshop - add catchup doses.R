load(file = paste(rootpath,"x_results/antiviralSetUp_FJI_adults_with_comorbidities_2023-02-21.Rdata",sep=''))
load(file = paste(rootpath,"x_results/antiviralSetUp_PNG_low_beta_adults_with_comorbidities_2023-02-18.Rdata",sep=''))


RECORD_outcomes_without_antivirals = RECORD_antiviral_setup$outcomes_without_antivirals
RECORD_likelihood_severe_outcome = RECORD_antiviral_setup$likelihood_severe_outcome
RECORD_incidence_log_tidy = RECORD_antiviral_setup$incidence_log_tidy 
RECORD_exposed_log = RECORD_antiviral_setup$exposed_log
RECORD_incidence_log = RECORD_antiviral_setup$incidence_log
RECORD_vaccination_history_FINAL = RECORD_antiviral_setup$vaccination_history_FINAL
RECORD_generic_booster_toggles = RECORD_antiviral_setup$generic_booster_toggles

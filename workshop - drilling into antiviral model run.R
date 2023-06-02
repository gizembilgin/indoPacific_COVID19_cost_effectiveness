#manager parameters
LIST_antiviral_start_date = c(as.Date('2023-01-01'),as.Date('2023-07-01'))
LIST_vax_scenario = unique(RECORD_antiviral_setup$outcomes_without_antivirals$vax_scenario)
LIST_antiviral_target_group = list('adults_with_comorbidities', #baseline
                                   'unvaccinated_adults',
                                   'unvaccinated_adults_AND_adults_with_comorbidities',
                                   'all_adults')
LIST_antiviral_type = list("nirmatrelvir_ritonavir","molunipiravir") #options:nirmatrelvir_ritonavir,molunipiravir 
toggle_high_risk_group = "adults_with_comorbidities"


toggle_number_of_runs = 100 #DEFAULT 100
toggle_cluster_number = 1

toggle_stochastic_SO = "off" # DEFAULT "on"
toggle_compare_to_vaccine_effect = "on"

toggle_sensitivity_analysis = list()
pathway_to_care = "fixed_RAT" #options: gold_standard (require data on healthcare seeking/access), fixed_RAT (probability of testing positive to a RAT within 5 days), fixed_direct (samples fixed proportion)
toggle_fixed_antiviral_coverage = 1 #% of people with access to antivirals OR access to RAT tests -> antiviral
toggle_antiviral_delivery_capacity = NA
manager_stochastic_VE_sampling = "uniform" 



#worker parameters
local_LIST_antiviral_target_group = LIST_antiviral_target_group
local_LIST_antiviral_start_date = LIST_antiviral_start_date
local_LIST_antiviral_type = LIST_antiviral_type
local_stochastic_SO = toggle_stochastic_SO
local_compare_to_vaccine_effect = toggle_compare_to_vaccine_effect
local_sensitivity_analysis = toggle_sensitivity_analysis
local_pathway_to_care = pathway_to_care
local_antiviral_delivery_capacity = toggle_antiviral_delivery_capacity
local_fixed_antiviral_coverage = toggle_fixed_antiviral_coverage
local_antiviral_effectiveness = antiviral_effectiveness
local_booster_start_date = booster_start_date
worker_stochastic_VE_sampling = manager_stochastic_VE_sampling
num_at_which_to_sample = 250000


to_plot = RECORD_antiviral_model_simulations %>% filter(evaluation_group == "net" & outcome == "severe_disease" & intervention %in% c(NA,"antiviral 2023-01-01") & (antiviral_type != "molunipiravir" | is.na(antiviral_type)) &  (antiviral_target_group == "adults_with_comorbidities" | is.na(antiviral_target_group)))
ggplot(to_plot) + geom_point(aes(x=antiviral_type,y=n,color=as.factor(vax_scenario))) + theme(legend.position = "bottom")

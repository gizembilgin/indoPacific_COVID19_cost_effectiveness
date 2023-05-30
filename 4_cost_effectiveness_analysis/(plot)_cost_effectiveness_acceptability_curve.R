
this_outcome = "QALYs"

CommandDeck_CONTROLS = list(
  LIST_booster_vax_scenarios = list(
    "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule"
     ,"all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule"                    
    # "catchup campaign for all adults: assume booster to all adults who have previously completed their primary schedule but have not recieved a booster"                       
    # "catchup campaign for high-risk adults: assume booster to high-risk adults who have previously completed their primary schedule but have not recieved a booster"           
     ,"all willing adults vaccinated with a primary schedule"  
  ),
  LIST_antiviral_elig_groups = list(
    "adults_with_comorbidities" 
    # "all_adults" 
    # "unvaccinated_adults" 
  ),
  LIST_antiviral_types = list(
    #"molunipiravir"          
    "nirmatrelvir_ritonavir"
  ),
  TOGGLE_uncertainty = "rand"
)

source(paste(getwd(),"/CommandDeck.R",sep=""))

CEAC_dataframe = data.frame()
this_workshop = CommandDeck_result_long %>% filter(outcome == this_outcome)


for (this_WTP in seq(round(min(this_workshop$cost_per_outcome_averted )),
                   round(max(this_workshop$cost_per_outcome_averted )),by =50)){
  
  rows = this_workshop %>%
    filter(cost_per_outcome_averted  <= this_WTP) %>%
    group_by(setting,booster_vax_scenario,antiviral_scenario) %>%
    summarise(probability = n() / TOGGLE_numberOfRuns, .groups = "keep") %>%
    mutate(WTP = this_WTP)
  
  CEAC_dataframe = rbind(CEAC_dataframe,rows)
}

to_plot = CEAC_dataframe %>%
  mutate(scenario = case_when(
    booster_vax_scenario == "no booster dose" & antiviral_scenario == "nirmatrelvir_ritonavir 2023-01-01 adults_with_comorbidities" ~ "oral antiviral to high-risk adults",
    
    booster_vax_scenario == "booster to all high-risk adults previously willing to be vaccinated" & antiviral_scenario == "no antiviral"   ~ "booster to high-risk adults",
    booster_vax_scenario == "booster to all adults previously willing to be vaccinated" & antiviral_scenario == "no antiviral"   ~ "booster to all adults",
    
    booster_vax_scenario == "booster to all high-risk adults previously willing to be vaccinated" & antiviral_scenario == "nirmatrelvir_ritonavir 2023-01-01 adults_with_comorbidities" ~ "booster and oral antiviral to high-risk adults",
    booster_vax_scenario == "booster to all adults previously willing to be vaccinated" & antiviral_scenario == "nirmatrelvir_ritonavir 2023-01-01 adults_with_comorbidities" ~ "booster to all and oral antiviral to high-risk adults",
    
  )) %>%
  filter(is.na(scenario) == FALSE)

ggplot() + geom_point(data = to_plot, aes(x=WTP,y=probability,color=as.factor(scenario))) +
  xlab("Willingness to pay ($/QALY)") +
  ylab("Probability cost-effective") +
  theme_bw() 

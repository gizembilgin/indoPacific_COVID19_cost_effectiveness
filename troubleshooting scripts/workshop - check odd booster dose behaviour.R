workshop = RECORD_antiviral_setup$vaccination_history_FINAL %>% 
  filter(schedule == "booster") %>%
  group_by(vaccine_type,dose,risk_group,FROM_vaccine_type,vax_scenario) %>%
  summarise(max_delivery = max(date)) %>%
  mutate(vax_scenario_short = case_when(
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved two booster doses"  ~
      "booster to all second booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved two booster doses" ~
      "booster to high-risk prev second booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to all prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to all first booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to high-risk prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to high-risk prev first booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule" ~ 
      "no booster"
  ))
workshop = unique(workshop)


#plot likelihood of severe outcomes with color risk_group, facet dose, fixed age_group per vax scenario short
this_age_group = '18 to 29'
workshop = RECORD_antiviral_setup$likelihood_severe_outcome %>%
  filter(age_group == this_age_group  & outcome == 'death') %>%
  mutate(vax_scenario_short = case_when(
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved two booster doses"  ~
      "booster to all second booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved two booster doses" ~
      "booster to high-risk prev second booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to all prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to all first booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to high-risk prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to high-risk prev first booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule" ~ 
      "no booster"
  )) %>%
  mutate(vaccine_combo = paste(vaccine_type,dose)) %>%
  filter((dose == 2 & vaccine_type == "Johnson & Johnson") | dose>=3) %>%
  filter(vaccine_type %in% unique(RECORD_antiviral_setup$vaccination_history_FINAL$vaccine_type[RECORD_antiviral_setup$vaccination_history_FINAL$schedule == "booster"]))
  #filter(vaccine_type %in% unique(RECORD_antiviral_setup$vaccination_history_FINAL$vaccine_type[RECORD_antiviral_setup$vaccination_history_FINAL$age_group == this_age_group]))
  #filter((dose < 3 & vaccine_type == "AstraZeneca") | (dose>=3 & vaccine_type == "Moderna"))

plot_list = list()
for (i in 1:length(unique(workshop$vax_scenario_short))){
  plot_list[[i]] = ggplot(workshop[workshop$vax_scenario_short == unique(workshop$vax_scenario_short)[i],]) + 
    geom_line(aes(x=date,y=percentage,color=as.factor(risk_group))) +
    facet_grid(vaccine_combo ~ .) + 
    labs(title = paste(unique(workshop$vax_scenario_short)[i]))
}
plot_list


workshop = VE_tracker %>%
  filter(age_group == this_age_group &
           outcome_VE == 'death' &
           ((dose < 3 & vaccine_type == "AstraZeneca") | (dose>=3 & vaccine_type == "Moderna")))
ggplot(workshop) +
  geom_point(aes(x=date,y=VE,color=as.factor(risk_group))) + 
  facet_grid(dose ~.)


workshop = severe_outcome_this_run %>%
  filter(age_group == this_age_group  & outcome == 'death') %>%
  #filter(vaccine_type %in% unique(RECORD_antiviral_setup$vaccination_history_FINAL$vaccine_type[RECORD_antiviral_setup$vaccination_history_FINAL$age_group == this_age_group]))
  filter((dose < 3 & vaccine_type == "AstraZeneca") | (dose>=3 & vaccine_type == "Moderna"))
ggplot(workshop) + 
    geom_line(aes(x=date,y=percentage,color=as.factor(risk_group))) +
    facet_grid(dose ~ .) 




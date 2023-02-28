
### lower incidence with additional booster doses to those previous second
workshop = RECORD_antiviral_setup$incidence_log %>%
  filter(vax_scenario %in% c("all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved two booster doses",
                             "all willing adults vaccinated with a primary schedule" )) %>%
  mutate(vax_scenario_short = case_when(
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved two booster doses"  ~
      "booster to all 2nd booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved two booster doses" ~
      "booster to high-risk prev 2nd booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to all prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to all 1st booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to high-risk prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to high-risk prev 1st booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule" ~ 
      "no booster"
  ))

ggplot(workshop) +
  geom_line(aes(x=date,y=daily_cases,color=as.factor(vax_scenario_short))) +
  theme(legend.position = "bottom")


### same vax history except dose == 5 for bosoter dose to previous second
workshop = RECORD_antiviral_setup$vaccination_history_FINAL%>%
  filter(vax_scenario %in% c("all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved two booster doses",
                             "all willing adults vaccinated with a primary schedule" ) )%>%
  mutate(vax_scenario_short = case_when(
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved two booster doses"  ~
      "booster to all 2nd booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved two booster doses" ~
      "booster to high-risk prev 2nd booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to all prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to all 1st booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to high-risk prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to high-risk prev 1st booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule" ~ 
      "no booster"
  )) %>%
  group_by(date,dose,age_group,risk_group,vax_scenario_short) %>%
  summarise(cov = sum(coverage_this_date))

this_age_group = '30 to 44'
plot_list = list()
for (this_dose in 1:max(workshop$dose)){
  plot_list[[this_dose]] = ggplot(workshop[workshop$dose == this_dose & workshop$age_group == this_age_group, ]) +
    geom_point(aes(
      x = date,
      y = cov,
      color = as.factor(risk_group)
    )) +
    theme(legend.position = "bottom") +
    facet_grid(vax_scenario_short ~ .)
}
plot_list


### check likelihood of severe outcome
this_age_group = '30 to 44'
this_outcome = 'severe_disease'
this_risk_group = 'general_public'

workshop = RECORD_antiviral_setup$likelihood_severe_outcome %>%
  filter(vax_scenario %in% c("all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved two booster doses",
                             "all willing adults vaccinated with a primary schedule" ) )%>%
  mutate(vax_scenario_short = case_when(
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved two booster doses"  ~
      "booster to all 2nd booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved two booster doses" ~
      "booster to high-risk prev 2nd booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to all prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule plus booster dose: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to all 1st booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a primary schedule" ~
      "booster to high-risk prev primary",
    vax_scenario ==  "all willing adults vaccinated with a primary schedule and high risk group recieve a booster: assume booster to all adults who have previously recieved a first booster dose" ~
      "booster to high-risk prev 1st booster",
    vax_scenario == "all willing adults vaccinated with a primary schedule" ~ 
      "no booster"
  )) %>%
  filter((dose < 3 & vaccine_type == "AstraZeneca") | (dose>=3 & vaccine_type == "Moderna")) %>%
  filter(age_group == this_age_group & 
           outcome == this_outcome & 
           risk_group == this_risk_group)


ggplot(workshop) + 
    geom_line(aes(x=date,y=percentage,color=as.factor(vax_scenario_short))) +
    facet_grid(dose ~ .)









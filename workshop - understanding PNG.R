

check = RECORD_antiviral_setup$vaccination_history_FINAL %>% 
  filter(schedule == "booster") %>%
  group_by(date,vax_scenario,risk_group) %>%
  summarise(doses = sum(doses_delivered_this_date)) %>%
  left_join(pop_risk_group) %>%
  group_by(vax_scenario,risk_group) %>%
  mutate(coverage = cumsum(doses)/pop*100)

plot_list = list()
for(i in 1:5){
  this_scenario = unique(check$vax_scenario)[i]
  plot_list[[i]] = ggplot(check[check$vax_scenario == this_scenario,]) + 
    geom_line(aes(x=date,y=coverage)) + facet_grid(risk_group ~ .) +
    labs(title = paste(this_scenario))
  
}
plot_list



check = RECORD_antiviral_setup$incidence_log_tidy %>% 
  group_by(date,vax_scenario,risk_group) %>%
  summarise(incidence = sum(incidence)) %>%
  left_join(pop_risk_group) %>%
  group_by(vax_scenario,risk_group) %>%
  mutate(previously_infected = cumsum(incidence)/pop*100)

plot_list = list()
for(i in 1:5){
  this_scenario = unique(check$vax_scenario)[i]
  plot_list[[i]] = ggplot(check[check$vax_scenario == this_scenario,]) + 
    geom_line(aes(x=date,y=previously_infected*100)) + facet_grid(risk_group ~ .) +
    labs(title = paste(this_scenario)) +
    ylab("cumulative infections (%)") + 
    xlab("") + theme_bw()
  
}
plot_list[[1]]


check = RECORD_antiviral_setup$incidence_log_tidy %>% 
  group_by(date,vax_scenario) %>%
  summarise(incidence = sum(incidence)) %>%
  group_by(vax_scenario) %>%
  mutate(cumulative_infections = cumsum(incidence))%>%
  mutate(vax_scenario_short = case_when(
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


ggplot(check) +
  geom_line(aes(
    x = date,
    y = cumulative_infections,
    color = as.factor(vax_scenario_short)
  )) +
  ylab("cumulative infections") +
  xlab("") + 
  theme_bw() +
  labs(color="")


ggplot(check) +
  geom_line(aes(
    x = date,
    y = cumulative_infections,
    color = as.factor(vax_scenario_short)
  )) +
  ylab("cumulative infections (%)") +
  xlab("") + 
  theme_bw() +
  labs(color="") +
  facet_grid(vax_scenario_short ~ .)

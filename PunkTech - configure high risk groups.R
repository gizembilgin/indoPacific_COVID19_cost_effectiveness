### This program will configure the 'at risk' group

### Notes on related programs
# CommandDeck default includes (you may need to turn off):
    # risk_group_toggle = "on"
    # risk_group_name = "pregnant_women"
# (2)_inital_state loads age-spcific prevalence into data.frame risk_dn


### NB: requires risk_group_toggle == "on" to run
### NB: age-specific prevalence loaded in (2)_inital_state called risk_dn


### Rearrange existing data.frames for usefulness ______________
#configure incidence_log_risk
incidence_log_risk = incidence_log_tidy
incidence_log_risk = aggregate(incidence_log_risk$incidence, by = list(incidence_log_risk$date, incidence_log_risk$risk_group), FUN = sum)
colnames(incidence_log_risk) = c('date','risk_group','daily_cases')
incidence_log_risk = incidence_log_risk %>%
  group_by(risk_group) %>%
  arrange(date) %>%
  mutate(rolling_average = (daily_cases + lag(daily_cases) + lag(daily_cases,n=2)+lag(daily_cases,n=3)
                            +lag(daily_cases,n=4)+lag(daily_cases,n=5)+lag(daily_cases,n=6))/7) %>%
  mutate(cumulative_incidence = cumsum(daily_cases)) 
incidence_log_risk = incidence_log_risk %>% left_join(pop_risk_group) %>%
  mutate(rolling_average_percentage = 100*rolling_average/pop) %>%
  mutate(cumulative_incidence_percentage = 100*cumsum(daily_cases)/pop)

#configure severe_outcomes_log_risk
severe_outcomes_log_risk = severe_outcome_log_tidy
severe_outcomes_log_risk = aggregate(severe_outcomes_log_risk$proj, by = list(severe_outcomes_log_risk$date, severe_outcomes_log_risk$risk_group, severe_outcomes_log_risk$outcome), FUN = sum)
colnames(severe_outcomes_log_risk) = c('date','risk_group','outcome','incidence')
severe_outcomes_log_risk = severe_outcomes_log_risk %>% left_join(pop_risk_group) %>%
  mutate(incidence_percentage = 100*incidence/pop) %>%
  group_by(risk_group) %>% arrange(date) %>%
  mutate(cumulative_incidence_percentage = 100*cumsum(incidence)/pop)
#______________________________________________________________________


### (Plot) 
plot_outcome = 'severe_disease'

# (1/2) Absolute numbers_________________________________________________________
abs_plot1 = ggplot(incidence_log_risk) + 
  geom_area(aes(date,rolling_average,fill=factor(risk_group))) +
  theme_bw()+ ylab('incidence') + xlab('')+ labs(fill='')

abs_plot2 = ggplot(severe_outcomes_log_risk[severe_outcomes_log_risk$outcome == plot_outcome,]) + 
  geom_area(aes(date,incidence,fill=factor(risk_group))) +
  theme_bw() + ylab(paste('incidence of',plot_outcome)) + xlab('') + labs(fill='')

grid.arrange(abs_plot1, abs_plot2, top = paste("Proportion of incidence attributable to 'at risk' group"))


# (2/2) Relative numbers_________________________________________________________
lay <- rbind(c(1,2),c(3,4))
rel_plot1 <- 
  ggplot(incidence_log_risk[incidence_log_risk$risk_group == risk_group_name,]) + 
  geom_line(aes(x=date,y=rolling_average_percentage),na.rm=TRUE) +
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  labs(title='at risk group') +
  ylab("daily cases % this pop") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

rel_plot2 <- 
  ggplot(incidence_log_risk[incidence_log_risk$risk_group == 'general_public',]) + 
  geom_line(aes(x=date,y=rolling_average_percentage),na.rm=TRUE) +
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  labs(title='general public') +
  ylab("daily cases % this pop") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

rel_plot3 <- 
  ggplot(severe_outcomes_log_risk[severe_outcomes_log_risk$risk_group == risk_group_name & severe_outcomes_log_risk$outcome == plot_outcome,]) + 
  geom_line(aes(x=date,y=incidence_percentage),na.rm=TRUE) +
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("severe disease % this pop") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

rel_plot4 <- 
  ggplot(severe_outcomes_log_risk[severe_outcomes_log_risk$risk_group == 'general_public' & severe_outcomes_log_risk$outcome == plot_outcome,]) + 
  geom_line(aes(x=date,y=incidence_percentage),na.rm=TRUE) +
  xlab("") + 
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  ylab("severe disease % this pop") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

grid.arrange(rel_plot1, rel_plot2, rel_plot3, rel_plot4, layout_matrix = lay, top = paste("Comparison between incidence in 'at risk' group and general public"))


# (2/2) comparison plot between this group and the general public




### What proportion of the 'at risk' population are untouched by the existing vaccination program?

#(1) pop at risk
workshop_pop = pop_risk_group_dn[pop_risk_group_dn == risk_group_name,] %>% select(age_group,pop)

#(2) current vaccine coverage in at risk
workshop_cov = vaccination_history_TRUE %>% 
  filter(date == max(vaccination_history_TRUE$date) &
           risk_group == risk_group_name)

#(3) pop untouched by existing vaccination program
workshop_touched = workshop_cov %>% 
  filter(dose == 1) 
workshop_touched = aggregate(workshop_touched$coverage_this_date, by = list(category = workshop_touched$age_group), FUN = sum)
colnames(workshop_touched) = c('age_group','cov')

unreachable = 1-risk_group_acceptability 
workshop_pop_dn = workshop_touched %>% 
  left_join(workshop_pop) %>%
  mutate(pop_touched = pop*cov,
         pop_untouched = pop - pop*unreachable - pop*cov)

#(4) calculate proportion to booster / (booster + primary)
vax_risk_proportion_booster = (sum(workshop_pop_dn$pop_touched))/(sum(workshop_pop_dn$pop_untouched)+ sum(workshop_pop_dn$pop_touched))
#NOTE: this is standard assumption of basis of population size

if (is.na(override_vax_risk_proportion) == FALSE){vax_risk_proportion_booster = override_vax_risk_proportion} #COMEBACK - sensitivity analysis, since real 0.54 set to 0.25 and 0.75

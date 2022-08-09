### FIGURE 2
results_warehouse[[1]][[3]]
results_warehouse[[3]][[3]]



### TABLE 2A
table2 = results_warehouse[[2]][[1]]
table2 = table2 %>% 
  pivot_longer(
    cols = 3:ncol(table2) ,
    names_to = 'outcome',
    values_to = 'num'
  ) 

#add overall
workshop = table2 %>%
  group_by(scenario,outcome) %>%
  summarise(num=sum(num)) %>%
  mutate(macro_age_group = 'overall')
table2 = rbind(table2,workshop)

#compare to baseline
baseline = table2 %>% 
  filter(scenario == "current vaccination targets (51.6%)" ) %>%
  rename(baseline=num) %>%
  select(-scenario)

table2 = table2 %>%
  left_join(baseline,by=c('macro_age_group','outcome')) 

table2 = table2 %>%
  mutate(abs_reduction = num - baseline,
         rel_reduction = 100*(num - baseline)/baseline)

table2$outcome = factor(table2$outcome,levels=c('cases','severe_disease','hosp','death','YLL'))
table2$macro_age_group = factor(table2$macro_age_group,levels=c('children','adults','overall'))
table2$scenario = factor(table2$scenario, levels = 
                           c("lower than current vaccination targets (40.0%)","current vaccination targets (51.6%)",
                             "expand to children now 40%","expand to children now 51.6%","expand to children now 60%",
                             "expand to children now 70%","expand to children now 80%",
                             "current then expand to children 60%","current then expand to children 70%", 
                             "current then expand to children 80%")   )
table2 = table2 %>% arrange(scenario,macro_age_group,outcome)

print = table2 %>% filter(! scenario %in% c("lower than current vaccination targets (40.0%)","current vaccination targets (51.6%)",
                                    "expand to children now 40%" ))  %>%
  mutate(abs_reduction = round(abs_reduction),
         rel_reduction = round(rel_reduction,digits=1),
         together_value = paste(format(abs_reduction, format="f", big.mark=",", digits=1),
                          ' (',rel_reduction,'%)',sep=''),
         together_outcome = paste(outcome,macro_age_group,sep='_')) %>%
  ungroup() %>%
  select(-num,-baseline,-abs_reduction,-rel_reduction,-macro_age_group,-outcome) %>%
  pivot_wider(
    id_cols = scenario,
    names_from = together_outcome,
    values_from = together_value)
write.csv(print,file='x_results/table2A.csv')



### TABLE 2B
table2 = results_warehouse[[3]][[1]]
table2 = table2 %>% 
  pivot_longer(
    cols = 3:ncol(table2) ,
    names_to = 'outcome',
    values_to = 'num'
  ) 

#add overall
workshop = table2 %>%
  group_by(scenario,outcome) %>%
  summarise(num=sum(num)) %>%
  mutate(macro_age_group = 'overall')
table2 = rbind(table2,workshop)

#compare to baseline
baseline = table2 %>% 
  filter(scenario == "current vaccination targets (51.6%)" ) %>%
  rename(baseline=num) %>%
  select(-scenario)

table2 = table2 %>%
  left_join(baseline,by=c('macro_age_group','outcome')) 

table2 = table2 %>%
  mutate(abs_reduction = num - baseline,
         rel_reduction = 100*(num - baseline)/baseline)

table2$outcome = factor(table2$outcome,levels=c('cases','severe_disease','hosp','death','YLL'))
table2$macro_age_group = factor(table2$macro_age_group,levels=c('children','adults','overall'))
table2$scenario = factor(table2$scenario, levels = 
                           c("lower than current vaccination targets (40.0%)","current vaccination targets (51.6%)",
                             "expand to children now 40%","expand to children now 51.6%","expand to children now 60%",
                             "expand to children now 70%","expand to children now 80%",
                             "current then expand to children 60%","current then expand to children 70%", 
                             "current then expand to children 80%")   )
table2 = table2 %>% arrange(scenario,macro_age_group,outcome)

print = table2 %>% filter(! scenario %in% c("lower than current vaccination targets (40.0%)","current vaccination targets (51.6%)",
                                            "expand to children now 40%" ))  %>%
  mutate(abs_reduction = round(abs_reduction),
         rel_reduction = round(rel_reduction,digits=1),
         together_value = paste(format(abs_reduction, format="f", big.mark=",", digits=1),
                                ' (',rel_reduction,'%)',sep=''),
         together_outcome = paste(outcome,macro_age_group,sep='_')) %>%
  ungroup() %>%
  select(-num,-baseline,-abs_reduction,-rel_reduction,-macro_age_group,-outcome) %>%
  pivot_wider(
    id_cols = scenario,
    names_from = together_outcome,
    values_from = together_value)
write.csv(print,file='x_results/table2B.csv')

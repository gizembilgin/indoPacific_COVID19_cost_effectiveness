
###In R Shiny:
### - format table header row variable names nicely
### - include descriptive text explaining assumptions of booster vax scenario eligibility
### - INPUT as buttons
### - make reactive up to user input

INPUT_include_setting = c("PNG","TLS")
INPUT_include_booster_vax_scenario = c("high-risk adults")

INPUT_include_antiviral_type = c("nirmatrelvir ritonavir")
INPUT_include_antiviral_type = c(INPUT_include_antiviral_type,"no antiviral")
INPUT_antiviral_target_group = c("adults_with_comorbidities")

INPUT_perspective = "healthcare"
INPUT_discounting_rate = 0.03
INPUT_antiviral_cost = "low_generic_cost"
INPUT_include_outcomes = c("QALY")
INPUT_include_net = "Y"

to_plot = CommandDeck_result %>%
  filter(evaluation_level == "incremental" | variable_type == "ICER")%>% 
  filter(variable_type %in% c("ICER","outcome") | variable == "netCost") %>%
  
  rename(outcome = variable) %>%
  mutate(outcome = gsub("cost_per_","",outcome),
         outcome = gsub("_averted","",outcome),
         outcome = gsub("QALYs","QALY",outcome)) %>%
  filter(outcome == INPUT_include_outcomes) %>%
  
  mutate(antiviral_type = gsub("_"," ",antiviral_type),
         antiviral_type = gsub(" 2023-01-01","",antiviral_type)#,
        # antiviral_target_group = gsub("_"," ",antiviral_target_group)
         ) %>%
  filter(antiviral_type %in% INPUT_include_antiviral_type &
           antiviral_target_group %in% INPUT_antiviral_target_group) %>%
  
  rename(booster_vax_scenario_long = booster_vax_scenario) %>%
  mutate(
    booster_vax_scenario = case_when(
      booster_vax_scenario_long ==  "booster to all high-risk adults previously willing to be vaccinated" ~ "high-risk adults",
      booster_vax_scenario_long ==  "booster to all adults previously willing to be vaccinated"           ~ "all adults",
      booster_vax_scenario_long ==  "booster dose catch-up campaign for all adults"                       ~ "catch-up for all adults",
      booster_vax_scenario_long ==  "booster dose catch-up campaign for high-risk adults"                 ~ "catch-up for high-risk adults",
      booster_vax_scenario_long ==  "all willing adults vaccinated with a primary schedule"               ~ "no booster",
      booster_vax_scenario_long ==  "booster to all adults, prioritised to high-risk adults"              ~ "all adults, prioritised to high-risk"
    )) %>%


  select(-sd)

cost_column = to_plot %>% 
  filter(variable_type == "cost") %>%
  mutate(variable_type = "netCost") %>%
  rename(netCost = mean) %>%
  ungroup() %>%
  select(-variable_type,-outcome,-LPI,-UPI)
cost_column = crossing(cost_column,outcome = unique(to_plot$outcome[to_plot$outcome != "netCost"]))


to_plot = to_plot %>% 
  filter(variable_type != "cost") %>%
  left_join(cost_column, by = join_by(evaluation_level, setting, booster_vax_scenario_long, antiviral_type, antiviral_target_group, outcome, discounting_rate, antiviral_cost, perspective,booster_vax_scenario)) %>%
  pivot_wider(names_from = variable_type,
              values_from = c(mean,LPI,UPI)) %>%
  select(-LPI_outcome,-UPI_outcome) %>%
  rename(count_outcome_averted = mean_outcome,
         net_cost = netCost,
         ICER = mean_ICER) %>%
  mutate(count_outcome_averted = round(count_outcome_averted,digits=1),
         net_cost = round(net_cost),
         ICER = round(ICER),
         LPI_ICER = round(LPI_ICER),
         UPI_ICER = round(UPI_ICER),
         PI = paste(round(LPI_ICER),"to",round(UPI_ICER)),
  ) %>%
  ungroup() %>%
  select(discounting_rate, antiviral_cost, perspective, setting,booster_vax_scenario,antiviral_type,antiviral_target_group,outcome,count_outcome_averted,net_cost,ICER,LPI_ICER,UPI_ICER)

#apply user input (hence reactive up to here!)
to_plot = to_plot %>%
  filter(outcome %in% INPUT_include_outcomes &
           setting %in% INPUT_include_setting &
           perspective == INPUT_perspective & 
           discounting_rate == INPUT_discounting_rate &
           antiviral_cost == INPUT_antiviral_cost &
           booster_vax_scenario %in% INPUT_include_booster_vax_scenario &
           antiviral_type %in% INPUT_include_antiviral_type)

if (INPUT_include_net == "N"){
  to_plot = to_plot %>%
    select(-net_cost,-count_outcome_averted)
}
 



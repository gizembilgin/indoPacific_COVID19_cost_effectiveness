

INPUT_ICER_table = "net" #options: net, incremental



INPUT_include_setting = c("PNG","TLS")
INPUT_include_booster_vax_scenario = c("high-risk adults")

INPUT_include_antiviral_type = c("nirmatrelvir ritonavir")
INPUT_include_antiviral_type = c(INPUT_include_antiviral_type,"no antiviral")
INPUT_antiviral_target_group = c("adults_with_comorbidities")

INPUT_perspective = "healthcare"
INPUT_discounting_rate = 0.03
INPUT_antiviral_cost_scenario = "low_generic_cost"
INPUT_include_outcomes = c("QALY")
INPUT_include_net = "Y"


rootpath = str_replace(getwd(), "GitHub_vaxAllocation/4_cost_effectiveness_analysis","")
this_path = paste0(rootpath,"/x_results/")
list_poss_Rdata = list.files(path = this_path,pattern = paste0(INPUT_ICER_table,"_complete_CEA_result*"))
if (length(list_poss_Rdata) > 0) {
  list_poss_Rdata_details = double()
  for (j in 1:length(list_poss_Rdata)) {
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste0(this_path, list_poss_Rdata[[j]]))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste0(this_path, latest_file))
} else{
  stop(paste("no results for",this_setting,"with",this_risk_group,"see Translator"))
}
CommandDeck_result = complete_results$CommandDeck_result
rm(complete_results)




###(1/3) Shiny ICER table
Shiny_ICER_table = CommandDeck_result %>%
  filter(evaluation_level == "incremental") %>%
  ungroup() %>%
  filter(variable_type %in% c("ICER","outcome") | outcome == "netCost") %>%
  select(-sd,-evaluation_level)
cost_column = Shiny_ICER_table %>%
  filter(variable_type == "cost") %>%
  mutate(variable_type = "netCost") %>%
  rename(netCost = mean) %>%
  ungroup() %>%
  select(-variable_type,-outcome,-LPI,-UPI)
cost_column = crossing(cost_column,outcome = unique(Shiny_ICER_table$outcome[Shiny_ICER_table$outcome != "netCost"]))
Shiny_ICER_table = Shiny_ICER_table %>%
  filter(variable_type != "cost") %>%
  left_join(cost_column, by = join_by(perspective, discounting_rate, setting, booster_vax_scenario, antiviral_cost_scenario, antiviral_type, antiviral_target_group, outcome)) %>%
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
  select(perspective,discounting_rate,setting,booster_vax_scenario,antiviral_cost_scenario,antiviral_type,antiviral_target_group,outcome,count_outcome_averted,net_cost,ICER,LPI_ICER,UPI_ICER)
View(Shiny_ICER_table)

if (INPUT_ICER_table = "net"){
  ###(2/3) Table S3.1 Overview of net outcomes and costs for different booster dose program and oral antiviral eligibility programs. 
  table_S3_1 = CommandDeck_result %>%
    filter(evaluation_level == "net",
           discounting_rate == 3,
           perspective == "societal perspective",
           booster_vax_scenario %in% c("all adults"   ,"high risk adults","no booster"),
           antiviral_target_group %in% c("no antiviral","adults with comorbidities"),
           antiviral_cost_scenario == "middle_income_cost" &
           antiviral_type != "molunipiravir" &
           outcome %in% c("death","QALYs","interventionCost","healthcareCostAverted","productivityLoss")) %>%
    ungroup()%>%
    select(-evaluation_level,-discounting_rate,-perspective,-antiviral_cost_scenario,-antiviral_type,-LPI,-UPI,-sd,-variable_type) %>%
    pivot_wider(names_from = outcome,values_from = mean) %>%
    mutate(scenario = case_when(
      booster_vax_scenario == "no booster" & antiviral_target_group == "no antiviral"                     ~ "Baseline with no booster or oral antiviral program",
      booster_vax_scenario == "no booster" & antiviral_target_group == "adults with comorbidities"       ~ "No booster, oral antiviral to high-risk adults",
      booster_vax_scenario == "high risk adults" & antiviral_target_group == "no antiviral"             ~ "Booster to high-risk adults, no oral antiviral",
      booster_vax_scenario == "all adults" & antiviral_target_group == "no antiviral"                    ~ "Booster to all adults, no oral antiviral",
      booster_vax_scenario == "high risk adults" & antiviral_target_group == "adults with comorbidities" ~ "Booster to high-risk adults, oral antiviral to high-risk adults",
      booster_vax_scenario == "all adults" & antiviral_target_group == "adults with comorbidities"       ~  "Booster to all adults, oral antiviral to high-risk adults"
    )) %>%
    select(setting,scenario,death,QALYs,interventionCost,healthcareCostAverted,productivityLoss)
  table_S3_1$scenario <- factor(table_S3_1$scenario,
                                levels = c("Baseline with no booster or oral antiviral program",
                                    "No booster, oral antiviral to high-risk adults",
                                    "Booster to high-risk adults, no oral antiviral",
                                    "Booster to all adults, no oral antiviral",
                                    "Booster to high-risk adults, oral antiviral to high-risk adults",
                                    "Booster to all adults, oral antiviral to high-risk adults"
                                ))
  table_S3_1 <- table_S3_1 %>% arrange(setting,scenario)
  write.csv(table_S3_1, "x_results/table_S3_1.csv")
  
  ###(3/3) Table S3.2 Incremental health benefits, costs, and resulting incremental cost-effectiveness ratios (ICERs) for different booster dose program and oral antiviral eligibility programs. 
  table_S3_2 = CommandDeck_result %>%
    filter(evaluation_level == "incremental",
           discounting_rate == 3,
           booster_vax_scenario %in% c("all adults"   ,"high risk adults","no booster"),
           antiviral_target_group %in% c("no antiviral","adults with comorbidities"),
           antiviral_cost_scenario == "middle_income_cost" &
            antiviral_type != "molunipiravir" ) %>%
    mutate(mean = format(round(mean), big.mark=","),
           LPI_new = format(round(min(LPI,UPI)), big.mark=","),
           UPI_new = format(round(max(LPI,UPI)), big.mark=","),
           
           outcome = case_when(variable_type == "ICER" ~ paste0("ICER_", outcome),
                               TRUE ~ outcome),
           # mean = case_when(
           #   outcome %in% c("netCost","ICER_QALYs") ~ paste0(mean," (",LPI_new," to ",UPI_new,")"),
           #   TRUE ~ as.character(mean)
           # ), 
           outcome = case_when(outcome %in% c("netCost","ICER_QALYs") ~ paste(gsub(" perspective","",perspective),outcome,sep="_"),
                          TRUE ~ outcome)
    ) %>% 
    filter(outcome %in% c("death","QALYs","interventionCost","healthcareCostAverted","productivityLoss","societal_netCost","healthcare_netCost","societal_ICER_QALYs","healthcare_ICER_QALYs") &
             (perspective == "societal perspective" | outcome %in% c("healthcare_netCost","healthcare_ICER_QALYs"))) %>%
    ungroup()%>%
    select(-evaluation_level,-discounting_rate,-perspective,-antiviral_cost_scenario,-antiviral_type,-sd,-variable_type) %>%

    select(-LPI,-UPI,-LPI_new,-UPI_new) %>%
    pivot_wider(names_from = outcome,values_from = mean) %>%
    mutate(scenario = case_when(
      booster_vax_scenario == "no booster" & antiviral_target_group == "no antiviral"                     ~ "Baseline with no booster or oral antiviral program",
      booster_vax_scenario == "no booster" & antiviral_target_group == "adults with comorbidities"       ~ "No booster, oral antiviral to high-risk adults",
      booster_vax_scenario == "high risk adults" & antiviral_target_group == "no antiviral"             ~ "Booster to high-risk adults, no oral antiviral",
      booster_vax_scenario == "all adults" & antiviral_target_group == "no antiviral"                    ~ "Booster to all adults, no oral antiviral",
      booster_vax_scenario == "high risk adults" & antiviral_target_group == "adults with comorbidities" ~ "Booster to high-risk adults, oral antiviral to high-risk adults",
      booster_vax_scenario == "all adults" & antiviral_target_group == "adults with comorbidities"       ~  "Booster to all adults, oral antiviral to high-risk adults"
    )) %>%
    select(setting,scenario,death,QALYs,interventionCost,healthcareCostAverted,productivityLoss,healthcare_netCost,societal_netCost,healthcare_ICER_QALYs,societal_ICER_QALYs)
  
  table_S3_2$scenario <- factor(table_S3_2$scenario,
                                levels = c("Baseline with no booster or oral antiviral program",
                                           "No booster, oral antiviral to high-risk adults",
                                           "Booster to high-risk adults, no oral antiviral",
                                           "Booster to all adults, no oral antiviral",
                                           "Booster to high-risk adults, oral antiviral to high-risk adults",
                                           "Booster to all adults, oral antiviral to high-risk adults"
                                ))
  table_S3_2 <- table_S3_2 %>% arrange(setting,scenario)
  write.csv(table_S3_2, "x_results/table_S3_2.csv")
}


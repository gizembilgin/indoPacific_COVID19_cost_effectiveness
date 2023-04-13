
#estimate for IDN 2022:
hosp_adm 
#          variable estimate       sd currency year              source
# hospital_admission  5847.48 4.144219      USD 2022 Nugraha et al. 2022

hosp_adm_details <- read.csv("2_inputs/inpatient_cost_details.csv",header=TRUE)

#_______________________________________________________________________________


#ratio of WHO CHOICE inpatient costs compared to IDN
workshop = WHO_CHOICE_2022 %>%
  filter(patient_type == "inpatient" &
           currency_short == "PPP" &        # DECISION: USD or purchasing power parities (PPP) shows how much a local currency unit is worth within the country's borders.
           statistic == "model_prediction") # DECISION: do we attempt to propogate the uncertainty in the ratio of model predictions?


#divide cost in setting by IDN cost
comparison = workshop %>%
  filter(ISO3_code == "IDN") %>%
  rename(baseline = "value") %>%
  select(-ISO3_code) %>%
  #left_join(workshop[workshop$ISO3_code != "IDN",],
  left_join(workshop,
            by = join_by(patient_type, patient_type_long, care_setting, currency_short,
                                                                statistic)) %>%
  mutate(value = value/baseline) %>%
  select(-baseline)


#include distribution of access to care
care_setting_distribution = hosp_adm_details %>%
  filter(variable == "care_setting") %>%
  rename(care_setting = category) %>%
  select(-variable)

comparison = comparison %>% 
  left_join(care_setting_distribution, by = "care_setting") %>%
  mutate(value = value * percentage) %>%
  group_by(ISO3_code,currency_short,statistic) %>%
  summarise(value = sum(value), .groups = "keep")


#divide hosp adm cost into factors to fix and adjust
hosp_adm_breakdown = hosp_adm_details %>%
  filter(variable == "expense_category") %>%
  mutate(calculation_category =
           case_when(
             category %in% c("drugs","consumables") ~ "fixed",
             TRUE ~ "adjust"
           )) %>%
  group_by(calculation_category) %>%
  summarise(percentage = sum(percentage))

#apply
comparison$currency_short = "USD" #for joining purposes

hosp_adm = hosp_adm %>%
  rename(currency_short = currency) %>%
  select(-source,-year) %>%
  left_join(comparison,by = "currency_short")%>%
  mutate(value = value*hosp_adm_breakdown$percentage[hosp_adm_breakdown$calculation_category == "adjust"] + 
           hosp_adm_breakdown$percentage[hosp_adm_breakdown$calculation_category == "fixed"]) %>%
  mutate(estimate = estimate * value,
         sd = sd * value)

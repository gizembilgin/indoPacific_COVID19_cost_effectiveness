
require(readr); require(ggplot2); require(tidyverse)

### (0) IMPORTS ################################################################
###import WB data on conversion rates
#https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.PCAP.CD&country=#
list_poss_csv = list.files(pattern = "2_inputs/World Bank extract*")
list_poss_csv_details = double()
for (i in 1:length(list_poss_csv)){
  list_poss_csv_details = rbind(list_poss_csv_details,
                                file.info(list_poss_csv[[i]])$mtime)
}
latest_file = list_poss_csv[[which.max(list_poss_csv_details)]]
raw <- read.csv(latest_file,header=TRUE)

workshop_WB = raw %>%
  pivot_longer(
    cols = 5:ncol(raw) ,
    names_to = 'year',
    names_prefix = "X",
    values_to = 'value'
  ) %>%
  mutate(year = as.numeric(substr(year, 1, 4))) %>%
  rename(variable = Series.Name,
         country_name = Country.Name,
         ISO3_code = Country.Code) %>%
  select(-Series.Code)
#_______________________________________________________________________________

###import IMF data on GDP deflator (latest October 2022)
#https://www.imf.org/en/Publications/SPROLLS/world-economic-outlook-databases#sort=%40imfdate%20descending
raw <- read.csv("2_inputs/IMF_data.csv",header=TRUE)

workshop_IMF = raw %>%
  pivot_longer(
    cols = 5:(ncol(raw)-1) ,
    names_to = 'year',
    names_prefix = "X",
    values_to = 'value'
  ) %>%
  mutate(year = as.numeric(substr(year, 1, 4))) %>%
  select(ISO,Subject.Descriptor,year,value) %>%
  rename(ISO3_code = ISO) %>%
  mutate(variable = case_when(
    Subject.Descriptor == "Gross domestic product, deflator" ~ "GDP_deflator",
    Subject.Descriptor == "Implied PPP conversion rate"   ~ "PPP_conversion"
  )) %>%
  rename(variable_long = Subject.Descriptor) %>%
  select(ISO3_code,year,variable_long,variable,value) %>%
  mutate(value = gsub(",","",value)) %>% #4,203 -> 4203
  mutate(value = as.numeric(value))
#_______________________________________________________________________________

###import cost estimates
cost_estimates <- read.csv("2_inputs/cost_estimates.csv",header=TRUE)
#_______________________________________________________________________________

###import WHO CHOICE estimates
WHO_CHOICE_raw <- read.csv("2_inputs/WHO_CHOICE_estimates.csv",header=TRUE)  %>%
  mutate(currency_short = case_when(
    currency == "2010 internal dollars (PPP $)" ~ "PPP",
    currency == "2010 National Currency Unit (NCU)" ~ "NCU",
    currency == "2010 USD" ~ "USD"
  )) %>%
  rename(ISO3_code = setting)
#_______________________________________________________________________________

###import exchange rates
exchange_rates <- read.csv("2_inputs/exchange_rate.csv",header=TRUE)
#_______________________________________________________________________________
################################################################################



### (1) data manipulation ######################################################
###average exchange_rates across 2022
exchange_rates = exchange_rates %>%
  group_by(ISO3_code) %>%
  summarise(NCU_to_USD = mean(NCU_to_USD))
TLS_row = data.frame(ISO3_code = "TLS", NCU_to_USD = 1)
exchange_rates$NCU_to_USD[exchange_rates$ISO3_code == "IDN"] = 1/exchange_rates$NCU_to_USD[exchange_rates$ISO3_code == "IDN"]
exchange_rates = rbind(exchange_rates,TLS_row)

###propagate uncertainty in WHO CHOICE estimates
#uncertainty provided only for PPP, let's propagate proportionally to NCU and USD
workshop = WHO_CHOICE_raw %>%
  filter(currency_short == "PPP") %>%
  select(-currency,-currency_short) %>%
  pivot_longer(cols = c("mean_value_from_sample",     "UB",    "LB",    "SD"),
               names_to = "statistic",
               values_to = "value") %>%
  mutate(proportion = value/model_prediction) %>%
  select(-model_prediction,-value)

workshop = WHO_CHOICE_raw  %>%
  pivot_longer(cols = c("mean_value_from_sample",     "UB",    "LB",    "SD"),
               names_to = "statistic",
               values_to = "value") %>%
  left_join(workshop, by = c("ISO3_code", "patient_type", "patient_type_long", "care_setting","statistic")) %>%
  mutate(value = model_prediction * proportion) %>%
  select(-proportion) %>%
  pivot_wider(names_from = "statistic",
              values_from = 'value')

#special case TLS - only provided as PPP and not NCU/USD (the same thing as TLS uses USD)
workshop_TLS = workshop %>%
  filter(ISO3_code == "TLS") %>% 
  filter(is.na(model_prediction) == FALSE)

adj_factor = workshop_IMF$value[workshop_IMF$year == 2010 & 
                                  workshop_IMF$variable == "PPP_conversion" & 
                                  workshop_IMF$ISO3_code == "TLS"]

workshop_TLS$model_prediction       = workshop_TLS$model_prediction * adj_factor
workshop_TLS$mean_value_from_sample = workshop_TLS$mean_value_from_sample * adj_factor
workshop_TLS$UB = workshop_TLS$UB * adj_factor
workshop_TLS$LB = workshop_TLS$LB * adj_factor
workshop_TLS$SD = workshop_TLS$SD * adj_factor

workshop = workshop %>%
  filter(!(ISO3_code == "TLS" & currency_short %in% c("USD","NCU")))

workshop_TLS$currency = "2010 National Currency Unit (NCU)"
workshop_TLS$currency_short = "NCU"
workshop = rbind(workshop,workshop_TLS)

workshop_TLS$currency = "2010 USD" 
workshop_TLS$currency_short = "USD"
workshop = rbind(workshop,workshop_TLS)

WHO_CHOICE_2010 = workshop
#_______________________________________________________________________________



### (2) calculate costs in 2022 USD #############################################
###cost of a hospital admission
hosp_adm <- cost_estimates %>% filter(variable == "hospital_admission")

#adjust NCU to 2022
adj_factor = 
  workshop_IMF$value[workshop_IMF$ISO3_code == "IDN" & workshop_IMF$year %in% c(2022) & workshop_IMF$variable == "GDP_deflator"]/
  mean(workshop_IMF$value[workshop_IMF$ISO3_code == "IDN" & workshop_IMF$year %in% c(2020,2021) & workshop_IMF$variable == "GDP_deflator"])
hosp_adm$estimate = hosp_adm$estimate * adj_factor
hosp_adm$sd = hosp_adm$sd  * adj_factor

#convert to USD
adj_factor = exchange_rates$NCU_to_USD[exchange_rates$ISO3_code == "IDN"] #average exchange rate over 2022 from Indonesian central bank https://www.bi.go.id/id/statistik/informasi-kurs/jisdor/Default.aspx
hosp_adm$estimate = hosp_adm$estimate * adj_factor
hosp_adm$sd = hosp_adm$sd  * adj_factor
hosp_adm$currency = "USD"
hosp_adm$year = "2022"
#_______________________________________________________________________________


###cost per outpatient visit
#adjust NCU to 2022
adj_factor = workshop_IMF %>%
  filter(variable == "GDP_deflator" & year %in% c(2010,2022)) %>%
  pivot_wider(names_from = year,
              values_from = value,
              names_prefix = "year_") %>%
  mutate(adj = year_2022/year_2010) %>%
  select(ISO3_code,adj)

workshop_NCU = WHO_CHOICE_2010 %>%
  filter(currency_short == "NCU") %>%
  left_join(adj_factor,by= "ISO3_code")  %>%
  pivot_longer(cols = c("model_prediction", "mean_value_from_sample",     "UB",    "LB",    "SD"),
               names_to = "statistic",
               values_to = "value") %>%
  mutate(value = value * adj) %>%
  select(-currency,-adj)

#adj to NCU to USD in 2022
workshop_USD = workshop_NCU %>%
  left_join(exchange_rates, by = 'ISO3_code') %>%
  mutate(value = value * NCU_to_USD)  %>%
  mutate(currency_short = "USD")%>%
  select(-NCU_to_USD)

#adj to NCU to PPP in 2022
workshop_PPP = workshop_IMF %>%
  filter(variable == "PPP_conversion" & year == 2022) %>%
  rename(PPP_conversion = value) %>%
  left_join(workshop_NCU, by = "ISO3_code") %>%
  mutate(value = value * 1/PPP_conversion)  %>%
  mutate(currency_short = "PPP") %>%
  select(-PPP_conversion, -variable_long,-variable,-year)

WHO_CHOICE_2022 = rbind(workshop_USD,workshop_PPP,workshop_NCU)

to_plot = WHO_CHOICE_2022 %>%
  mutate(label = paste(patient_type,care_setting,sep="-")) %>%
  filter(statistic == "model_prediction" &
           currency_short == "USD")
ggplot(to_plot) + geom_col(aes(x=ISO3_code,y=value,fill=as.factor(label)),position="dodge")

# to_plot = WHO_CHOICE_2022 %>%
#   mutate(label = paste(patient_type,care_setting,sep="-")) %>%
#   filter(statistic == "model_prediction" &
#            currency_short == "PPP")
# ggplot(to_plot) + geom_col(aes(x=ISO3_code,y=value,fill=as.factor(label)),position="dodge")
# 
# to_plot = WHO_CHOICE_2010 %>%
#   mutate(label = paste(patient_type,care_setting,sep="-")) %>%
#   filter(currency_short == "USD")
# ggplot(to_plot) + geom_col(aes(x=ISO3_code,y=model_prediction,fill=as.factor(label)),position="dodge")
# 
# to_plot = WHO_CHOICE_2010 %>%
#   mutate(label = paste(patient_type,care_setting,sep="-")) %>%
#   filter(currency_short == "PPP")
# ggplot(to_plot) + geom_col(aes(x=ISO3_code,y=model_prediction,fill=as.factor(label)),position="dodge")
#_______________________________________________________________________________


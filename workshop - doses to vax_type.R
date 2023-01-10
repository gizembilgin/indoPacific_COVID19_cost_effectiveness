

### Load data on vaccine supply in from WHO Dashboard https://app.powerbi.com/view?r=eyJrIjoiMWNjNzZkNjctZTNiNy00YmMzLTkxZjQtNmJiZDM2MTYxNzEwIiwidCI6ImY2MTBjMGI3LWJkMjQtNGIzOS04MTBiLTNkYzI4MGFmYjU5MCIsImMiOjh9
#(last update 07/12/2022)
setting_vaccine <- read.csv("1_inputs/WHO_reported_vax_type.csv",header=TRUE) %>%
  select(-WHO_REGION,-UNICEF_REGION,-WBINCOME_LONG_STATUS,-WHO_LEGAL_STATUS_TITLE,-DEVELOPMENT_STATUS,-COVAX_STATUS,-UNPOP,-NUMBER_TARGET,-NOT_VACCINATED) %>%
  filter(ISO_3_CODE %in% setting_list)


setting_vaccine <- read.csv("1_inputs/vaccine_market_UNICEF.csv",header=TRUE) 
setting_vaccine$delivery_month = as.Date(setting_vaccine$delivery_month, "%d/%m/%Y")

#The Oxford–AstraZeneca COVID‑19 vaccine, sold under the brand names Covishield[29] and Vaxzevria[1][30] among others, is a viral vector vaccine for prevention of COVID-19.
setting_vaccine$vaccine_type[setting_vaccine$vaccine_type == "Covishield"] = "AstraZeneca"
setting_vaccine = setting_vaccine %>%
  group_by(setting,delivery_month,vaccine_type) %>%
  summarise(doses = sum(doses), .groups = "keep")



this_setting_vaccine_type = setting_vaccine %>%
  filter(setting == this_setting)




### previous code from (1) simulate setting
setting_vaccine$last_update = as.Date(setting_vaccine$last_update,format = '%d/%m/%Y')
setting_vaccine <- setting_vaccine %>%
  filter(setting == setting & last_update == max(setting_vaccine$last_update))

if ("Johnson & Johnson" %in% unique(setting_vaccine$vaccine_type)){ #J&J the only single-dose vaccine
  setting_vaccine <- setting_vaccine %>%
    mutate(
      dose_one = case_when(
        vaccine_type == "Johnson & Johnson" ~ 2*doses/(sum(setting_vaccine$doses)+setting_vaccine$doses[setting_vaccine$vaccine_type == "Johnson & Johnson"]),
        vaccine_type != "Johnson & Johnson" ~ doses/(sum(setting_vaccine$doses)+setting_vaccine$doses[setting_vaccine$vaccine_type == "Johnson & Johnson"])
      ),
      dose_two = case_when(
        vaccine_type == "Johnson & Johnson" ~ 0,
        vaccine_type != "Johnson & Johnson" ~ doses/(sum(setting_vaccine$doses)- setting_vaccine$doses[setting_vaccine$vaccine_type == "Johnson & Johnson"])
      ))
  
} else {
  setting_vaccine <- setting_vaccine %>%
    mutate(dose_one=doses/sum(setting_vaccine$doses),
           dose_two=doses/sum(setting_vaccine$doses))
}

setting_vaccine_2 <- setting_vaccine %>%
  select(vaccine_type,dose_two,dose_one) %>%
  pivot_longer(
    cols='dose_one':'dose_two',
    names_to='dose_charac',
    values_to = 'prop'
  ) %>%
  mutate(dose = case_when(
    dose_charac == 'dose_one' ~ 1,
    dose_charac == 'dose_two' ~ 2
  )) %>%
  select(-dose_charac)
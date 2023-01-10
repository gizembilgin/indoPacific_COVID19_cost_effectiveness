setting_list = c("PNG","TLS","IDN","FJI","SLB","PHL")
settings_long_list = c("Fiji","Indonesia","Philippines","Papua New Guinea","Solomon Islands","Timor-Leste")

###Seroprevalence
#metadata: https://airtable.com/shr9XzggGpYFqMdJF/tblIdx2b6ZOLevdJr
#GitHub: https://github.com/serotracker/sars-cov-2-data?utm_source=GitHub
# "For more information and context about our data, please take a look at the appendix 
#to our full systematic review and meta-analysis of SARS-CoV-2 serosurveys for our full methods (https://bit.ly/3u2iEKe). 
#For any other questions, you can contact Rahul Arora at rahul.arora@balliol.ox.ac.uk."

workshop <- readr::read_csv("https://raw.githubusercontent.com/serotracker/sars-cov-2-data/main/serotracker_dataset.csv")
# [1] "estimate_name"              "study_name"                 "source_name"                "publication_date"           "source_type"               
# [6] "estimate_grade"             "study_type"                 "country"                    "state"                      "city"                      
# [11] "study_inclusion_criteria"   "study_exclusion_criteria"   "sampling_start_date"        "sampling_end_date"          "population_group"          
# [16] "sex"                        "age"                        "age_min"                    "age_max"                    "subgroup_var"              
# [21] "subgroup_cat"               "subgroup_specific_category" "denominator_value"          "serum_pos_prevalence"       "seroprev_95_ci_lower"      
# [26] "seroprev_95_ci_upper"       "dashboard_primary_estimate" "test_adj"                   "pop_adj"                    "clustering_adjustment"     
# [31] "academic_primary_estimate"  "sampling_method"            "test_name"                  "test_manufacturer"          "test_type"                 
# [36] "specimen_type"              "isotypes"                   "antibody_target"            "test_validation"            "sensitivity"               
# [41] "specificity"                "overall_risk_of_bias"       "jbi_1"                      "jbi_2"                      "jbi_3"                     
# [46] "jbi_4"                      "jbi_5"                      "jbi_6"                      "jbi_7"                      "jbi_8"                     
# [51] "jbi_9"                      "first_author"               "lead_institution"           "is_unity_aligned"           "url"                       
# [56] "date_created"               "last_modified_time"         "data_quality_status"        "zotero_citation_key"        "alpha_3_code"   
workshop = workshop %>% filter(alpha_3_code %in% setting_list)
workshop = workshop %>% select(-estimate_name,-study_name,-age_min,-age_max,-subgroup_var,-subgroup_cat,-denominator_value)
View(workshop)
#last sampling data is 12/2021



workshop <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-manufacturer.csv")
#by vaccine type!
# unique(workshop$location) #all UIC
workshop <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-age-group.csv")
#by age group!!
# unique(workshop$location) #all UIC


locations <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv")
workshop = locations %>% filter(iso_code %in% setting_list)
#vaccine types listed with last observed date and source name (NB: when WHO and not a gov less reliable)


workshop <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_vaccines_full.csv")
workshop = workshop %>% filter(CountryCode %in% setting_list)
check = workshop %>% filter(Date == '20221101')
#metadata https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md#vaccination-policies
#This program applies a waning distribution to the point estimates of VE (VE_estimates_imputed created in `(mech shop) VE point estimate.R`)

# We include two distributions:
# 1. acquisition - covering any_infection and symptomatic_disease
# 2. severe_outcomes - covering severe_disease and death


###(1/3) Load distributions ('acqusition' and 'severe outcomes')
#COMEBACK - loading NG day-day waning
#distribution for viral (AZ reference) and mRNA (Pfizer reference) for dose 1,2, and 3 (booster is mRNA only)
load(file = "1_inputs/NG_VE_processed.Rdata")
load_distribution <- VE_together %>%
  mutate(vaccine_type = gsub("AZ", "AstraZeneca", vaccine_type),
         vaccine_mode = case_when(
                vaccine_type == 'Pfizer' ~ 'mRNA',
                vaccine_type == 'AstraZeneca' ~ 'viral'),
         outcome_family = case_when(
           outcome %in% c('acquisition') ~ 'acquisition',
           outcome %in% c('hospitalisation') ~ 'severe_outcome'
         #COMEBACK - options =  "symptoms"        "hospitalisation" "death"           "acquisition"     "transmission"   
         )
         ) %>%
  filter(is.na(outcome_family) == FALSE) %>%
  group_by(outcome,strain,vaccine_mode,dose) %>%
  mutate(VE_internal = ve_predict_mean / max(ve_predict_mean)) %>%
  ungroup() %>%
  select(vaccine_mode,dose,outcome_family,strain,days,VE_internal)


###(2/3) Apply distributions
apply_distribution = VE_estimates_imputed %>% left_join(load_distribution, by =  c("outcome_family","dose","vaccine_mode","strain")) %>%
  select('strain','outcome','vaccine_mode','vaccine_type','dose','days','VE','VE_internal')  %>%
  mutate(VE_days = VE * VE_internal/100)


###(3/3) Plot distributions and save VE_waning_distribution
#(A/B) Plot
workshop = apply_distribution %>%
  filter(dose < 3) %>%
  mutate(immunity = paste(vaccine_type,dose))
# 
# workshop = no_waning %>%
#   filter(dose < 3) %>%
#   mutate(immunity = paste(vaccine_type,dose))

strain_test = 'omicron'

plot_list = list()

for (i in 1:length(unique(workshop$outcome))){
  proj_outcome = unique(workshop$outcome)[i]
  
  plot_list[[i]]  <- ggplot() +
    geom_line(data=workshop[workshop$outcome == proj_outcome & workshop$strain == strain_test,],
              aes(x=days,y=VE_days,color=as.factor(immunity)),na.rm=TRUE) +
    labs(title=(paste("Waning of VE against",proj_outcome,"(",strain_test,')'))) +
    xlab("days since vaccination") +
    ylab("% max protection") +
    ylim(0,1)+
    theme_bw() +
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  
}
plot_list

#(B/B) Save
waning = apply_distribution %>% mutate(waning = TRUE)
no_waning = apply_distribution %>% mutate(waning = FALSE) %>%
  group_by(strain,outcome,vaccine_type,dose) %>%
  mutate(VE_days = max(VE_days))

VE_waning_distribution = rbind(waning,no_waning) %>% select(strain,outcome,vaccine_type,dose,days,VE_days,waning)
save(VE_waning_distribution, file = '1_inputs/VE_waning_distribution.Rdata')



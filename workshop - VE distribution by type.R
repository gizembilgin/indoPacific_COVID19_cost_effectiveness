## checking distribution provided by NG consistent between strains

outcome = 'hospitalisation'

strain_test = 'omicron'

workshop <- VE_together[VE_together$outcome == outcome &
                                 VE_together$strain == strain_test,]
workshop <- VE_together[VE_together$strain == strain_test,]

workshop <- workshop %>%
  group_by(vaccine_type,outcome,dose) %>%
  mutate(VE_internal = ve_predict_mean / max(ve_predict_mean),
         immunity=paste(vaccine_type,dose))

plot_list = list()

for (i in 1:length(unique(workshop$outcome))){
  proj_outcome = unique(workshop$outcome)[i]
  
  plot_list[[i]]  <- ggplot() +
    geom_line(data=workshop[workshop$outcome == proj_outcome,],aes(x=days,y=VE_internal,color=as.factor(immunity)),na.rm=TRUE) +
    labs(title=(paste("Waning of VE against",proj_outcome,"(",strain_test,')'))) +
    xlab("days since vaccination") +
    ylab("% max protection") +
    ylim(0,1)+
    theme_bw() +
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  
}
plot_list

#NB: waning consistent for each vaccine type and dose between strains



### applying to other vaccine_types
VE_waning_distribution <- VE_together %>%
  mutate(vaccine_type = gsub("AZ", "AstraZeneca", vaccine_type)) %>%
  mutate(vaccine_mode = case_when(
    vaccine_type == 'Pfizer' ~ 'mRNA',
    vaccine_type == 'AstraZeneca' ~ 'viral'
  ))
#Create VE_internal as metric of % max VE for this type
VE_waning_distribution <- VE_waning_distribution%>%
  group_by(outcome,strain,vaccine_mode,dose) %>%
  mutate(VE_internal = ve_predict_mean / max(ve_predict_mean))
VE_waning_distribution <- VE_waning_distribution[,-c(3)] #remove vaccine_type

#Load VE for all vaccines we are interested in  
VE_full_vaccine_type = read.csv("1_inputs/vaccine_effectiveness.csv",header=TRUE)
VE_full_vaccine_type <- VE_full_vaccine_type %>%
  mutate(vaccine_mode = case_when(
    vaccine_type == 'Pfizer' ~ 'mRNA',
    vaccine_type == 'Moderna' ~ 'mRNA',
    vaccine_type == 'AstraZeneca' ~ 'viral',
    vaccine_type == 'Sinopharm' ~ 'viral',
    vaccine_type == 'Sinovac' ~ 'viral',
    vaccine_type == 'Johnson & Johnson' ~ 'viral'
  ))

workshop <- VE_full_vaccine_type %>%
  left_join(VE_waning_distribution, by = c("outcome","dose","vaccine_mode")) %>%
  select('strain','outcome','vaccine_mode','vaccine_type','dose','days','VE','VE_internal') %>%
  mutate(VE_days = VE * VE_internal/100) %>%
  mutate(immunity = paste(vaccine_type,dose))


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
#NB: some overlap as taken from same estimate
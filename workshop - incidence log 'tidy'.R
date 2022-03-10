workshop = subset(incidence_log_unedited, select=-c(time,daily_cases))

workshop = pivot_longer(
  workshop,
  cols = paste((num_disease_classes)*(num_age_groups*num_vax_classes)+1):paste((num_disease_classes+1)*(num_age_groups*num_vax_classes)),
  names_to = 'temp',
  values_to = 'incidence'
)
workshop$temp = as.numeric(workshop$temp) - (num_disease_classes)*(num_age_groups*num_vax_classes)

workshop2=as.data.frame(unique(workshop$temp)); colnames(workshop2)=c('temp')

workshop2$age_group = rep(age_group_labels,num_vax_classes)
workshop2$dose = 0
workshop2$vaccine_type = "unvaccinated"
for (d in 1:num_vax_doses){
  workshop2$dose[workshop2$temp %in% c((T*(d-1)+1)*J+1):((T*d+1)*J)] = d
  for (t in 1:num_vax_types){
    workshop2$vaccine_type[workshop2$temp %in% c((((t-1)+(d-1)*T+1)*J+1):(((t-1)+(d-1)*T+2)*J))] = vax_type_list[t]
  }
}
#View(workshop2)
#CHECKED: yes aligns as expected

incidence_log_tidy = workshop %>% 
  left_join(workshop2)

incidence_log_tidy = subset(incidence_log_tidy,select=-c(temp))

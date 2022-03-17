options(scipen = 100)


### flatten to vaccine classes
flat = data.frame(colSums(next_state))
colnames(flat) = c('pop')
flat$temp = seq(1,49)
flat$age_group = rep(age_group_labels,num_vax_classes)
flat$dose = 0
flat$vaccine_type = "unvaccinated"
for (d in 1:num_vax_doses){
  flat$dose[flat$temp %in% c((T*(d-1)+1)*J+1):((T*d+1)*J)] = d
  for (t in 1:num_vax_types){
    flat$vaccine_type[flat$temp %in% c((((t-1)+(d-1)*T+1)*J+1):(((t-1)+(d-1)*T+2)*J))] = vax_type_list[t]
  }
}

aggregate(flat$pop, by=list(flat$age_group), FUN=sum)
pop
#CHECKED: pop remains constant

aggregate(flat$pop, by=list(flat$age_group,flat$dose), FUN=sum)
aggregate(flat$pop, by=list(flat$dose), FUN=sum)

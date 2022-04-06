options(scipen = 100)


### Existing code looks like:
S = as.matrix(state_working[1:A])
E = as.matrix(state_working[(A+1):(2*A)])
I = as.matrix(state_working[(2*A+1):(3*A)])
R = as.matrix(state_working[(3*A+1):(4*A)])
Incid = as.matrix(state_working[(4*A+1):(5*A)]) 


prev_state <- as.data.frame(rbind(S,E,I,R))
row.names(prev_state) <- c("S","E","I","R")
next_state=prev_state # initialise next state


### Fresh start!
prev_state = data.frame()
class_list = list(S,E,I,R)
class_name_list = c('S','E','I','R')
for (i in 1:num_disease_classes){
  workshop = data.frame(t(class_list[[i]]))
  colnames(workshop) = c('pop')
  workshop$class = class_name_list[i]
  workshop$temp = seq(1,(num_age_groups*num_vax_classes))
  workshop$age_group = rep(age_group_labels,num_vax_classes)
  workshop$dose = 0
  workshop$vaccine_type = "unvaccinated"
  for (d in 1:num_vax_doses){
    workshop$dose[workshop$temp %in% c((T*(d-1)+1)*J+1):((T*d+1)*J)] = d
    for (t in 1:num_vax_types){
      workshop$vaccine_type[workshop$temp %in% c((((t-1)+(d-1)*T+1)*J+1):(((t-1)+(d-1)*T+2)*J))] = vax_type_list[t]
    }
  }
  prev_state = rbind(prev_state,workshop)
}
sum(prev_state$pop); sum(pop)


aggregate(flat$pop, by=list(flat$age_group), FUN=sum)
pop
#CHECKED: pop remains constant

aggregate(flat$pop, by=list(flat$age_group,flat$dose), FUN=sum)
aggregate(flat$pop, by=list(flat$dose), FUN=sum)
aggregate(flat$pop, by=list(flat$vaccine_type), FUN=sum)

library("Rcpp")
cppFunction("bool isOddCpp(int num = 10) {
   bool result = (num % 2 == 1);
   return result;
}")
isOddCpp(21L)


time.start=proc.time()[[3]]

workshop = data.frame()
for (i in 1:J){
  for (t in 1:num_vax_types){
    for (d in 1:D){
      for (r in 1:num_risk_groups){
        this_vax = vax_type_list[t]
        if (!( this_vax %in% unique(workshop$vaccine_type[workshop$risk_group == risk_group_labels[r] & workshop$dose == d & workshop$age_group == age_group_labels[i]]))){
          workshop2 = crossing(risk_group = risk_group_labels[r],
                               dose = d,
                               vaccine_type = this_vax,
                               age_group = age_group_labels[i],
                               VE =0) 
          workshop = rbind(workshop,workshop2)
        } 
      }
    }
  }
}



time.end=proc.time()[[3]]
time.end-time.start


time.start=proc.time()[[3]]

workshop = data.frame()
for (t in 1:num_vax_types){
  this_vax = vax_type_list[t]
  if (!( this_vax %in% unique(workshop$vaccine_type[workshop$risk_group == risk_group_labels[r] & workshop$dose == d & workshop$age_group == age_group_labels[i]]))){
    for (i in 1:J){
    for (d in 1:D){
      for (r in 1:num_risk_groups){
        
          workshop2 = crossing(risk_group = risk_group_labels[r],
                               dose = d,
                               vaccine_type = this_vax,
                               age_group = age_group_labels[i],
                               VE =0) 
          workshop = rbind(workshop,workshop2)
        } 
      }
    }
  }
}

time.end=proc.time()[[3]]
time.end-time.start
### Plot cumulative incidence of different vaccine strategies

vax_strategy_num_doses = 200000
vax_strategy_vaccine_type = "AstraZeneca"
vax_strategy_delivery_timeframe = 10 #(days)
vax_strategy_start_date = as.Date('2022-01-20')

vax_strategy_addition = data.frame()
colnames(vax_strategy_addition) = colnames(vaccination_history_FINAL)
#vax_strategy_addition$date = as.Date(c(vax_strategy_start_date:(vax_strategy_start_date+vax_strategy_delivery_timeframe)))


#want to row bind to:"date","vaccine_type","vaccine_mode","dose","coverage_this_date","doses_delivered_this_date"
# (6 columns)

for (i in 1:vax_strategy_delivery_timeframe){
  date = as.Date(vax_strategy_start_date + (i-1))
  vaccine_type = vax_strategy_vaccine_type
  doses_delivered_this_date = vax_strategy_num_doses/(vax_strategy_delivery_timeframe*2)
  
  dose1 = cbind.data.frame(date,vaccine_type,"viral",1,NA,doses_delivered_this_date)
  dose2 = cbind.data.frame(date,vaccine_type,"viral",2,NA,doses_delivered_this_date)
  colnames(dose1) = colnames(vaccination_history_FINAL)
  colnames(dose2) = colnames(vaccination_history_FINAL)
  
  if (i == 1){vax_strategy_addition = rbind(dose1,dose2)
  } else{vax_strategy_addition = rbind(vax_strategy_addition,dose1,dose2)}
}


vaccination_history_FINAL = rbind(vaccination_history_FINAL,vax_strategy_addition)

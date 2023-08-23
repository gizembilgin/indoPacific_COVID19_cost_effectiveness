
workshop = vaccination_history_FINAL %>% filter(risk_group == risk_group_name)

sum(vaccination_history_FINAL$doses_delivered_this_date) #5560686
sum(vaccination_history_TRUE$doses_delivered_this_date)  #4560687
sum(at_risk_delivery_outline$doses_delivered_this_date)  #284653
sum(generalPublic_leftover_outline$doses_delivered_this_date) #715346

sum(vaccination_history_FINAL$doses_delivered_this_date)  == sum(vaccination_history_TRUE$doses_delivered_this_date) + sum(at_risk_delivery_outline$doses_delivered_this_date) + sum(generalPublic_leftover_outline$doses_delivered_this_date) 
#TRUE


#p + b
#pb = vax_delivery_outline
sum(pb$doses_delivered_this_date) #139624
#poached b
sum(booster_delivery_outline$doses_delivered_this_date) #145029

vax_delivery_outline %>% group_by(dose) %>% summarise(total = sum(doses_delivered_this_date))
#      1  69812
#      2  69812
#      8 145029

#unique(vax_delivery_outline$date[vax_delivery_outline$dose == 8]) + vaxCovDelay$delay[vaxCovDelay$dose == booster_dose_number]


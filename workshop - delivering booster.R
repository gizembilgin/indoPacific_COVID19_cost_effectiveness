workshop = aggregate(at_risk_delivery_outline$doses_delivered_this_date,by=list(at_risk_delivery_outline$date),FUN=sum) 
colnames(workshop) = c('date','doses')
workshop %>% filter(doses>0)


workshop = aggregate(vax_delivery_outline$doses_delivered_this_date,by=list(vax_delivery_outline$date,vax_delivery_outline$age_group),FUN=sum) 
colnames(workshop) = c('date','age_group','doses')
workshop %>% filter(doses>0)


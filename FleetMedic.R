### This program is intended runs the model in multiple ways to deliver targetted doses to the at-risk group
# The whole of FleetMedic should run once per risk group, and should be called from the FleetAdmiral to print results there

### Currently four main sections:
# (1) Prioritising primary dose coverage by x%
# (2) Delivery of booster doses to at risk group
# (3) Sensitivity analysis: (a) RR, (b) VE
#________________________________________________________________________________________________________________

time.start.FleetMedic=proc.time()[[3]]

results_warehouse_FM = list()
subreceipt = 0

outbreak_timing = "off" ###ASSUMPTION

age_split_results = "N"
vax_strategy_toggle = "on"
vax_risk_strategy_toggle = "on"
risk_group_toggle = "on" 
vax_strategy_toggles = vax_strategy_toggles_CURRENT_TARGET
#risk_group_name = "pregnant_women" ### SPECIFIED IN FLEET ADMIRAL

risk_group_prioritisation_to_date = NA
default_prioritisation_proportion = 0.5
VE_at_risk_suppress = 1
RR_estimate = RR_default = 2


risk_group_visulation = ggplot(data = pop_risk_group_dn) + geom_bar(aes(x=factor(age_group,level=age_group_labels),y=pop,fill=factor(risk_group,level=risk_group_labels)),
                                            stat='summary',position="dodge") + labs(fill='risk group') + xlab('age group')




###(1) Prioritising primary dose coverage by x%
subreceipt = 1
source(paste(getwd(),"/(FM run 1) prioritising primary doses.R",sep=""))
#________________________________________________________________________________________________________________


### (2) Delivery of booster doses to at risk group
subreceipt = 2
source(paste(getwd(),"/(FM run 2) priority delivery of booster dose.R",sep=""))
#________________________________________________________________________________________________________________


### (3) Sensitivity analysis: (a) RR, (b) VE
#(A)  RR 1-2.5
subreceipt = 3
source(paste(getwd(),"/(FM sensitivity 1) Varying RR.R",sep=""))

#(B) lower VE
subreceipt = 4
source(paste(getwd(),"/(FM sensitivity 2) Reduced VE in 'at risk'.R",sep=""))
#________________________________________________________________________________________________________________




save.image(file = paste("x_results/FleetMedic_run_",Sys.Date(),risk_group_name,".Rdata",sep=''))

time.end.FleetMedic=proc.time()[[3]]
time.end.FleetMedic-time.start.FleetMedic 
# 6798.61 = 1.9 hours #COMEBACK - time!


time = Sys.time()
time = gsub(':','-',time)

file_name = paste( "x_results/FleetMedic",risk_group_name,time)
file_name = gsub(' ','_',file_name)

library(rmarkdown)
#render('FleetMedic_compiler.Rmd',output_file = file_name) ### COMEBACK


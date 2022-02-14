### This program is my blank piece of paper

#checking distribution of disease parameters O.K.
Z= rlnorm(100000,meanlog = 1.3, sd=0.2)
plot(density(Z))
mean(Z);median(Z);mean(Z)+1.96*sd(Z); mean(Z)-1.96*sd(Z)



##### one way sensitivity
source(paste(getwd(),"/(2)_configure_population.R",sep="")) #leave as separate script so can swap in/out Queanbeyan
source(paste(getwd(),"/(3)_COVID_ODE.R",sep=""))


source(paste(getwd(),"/(1)_disease_characteristics.R",sep=""))
gamma = 1-0.387
source(paste(getwd(),"/(4)_time_step.R",sep=""))











#time current run of transmission model for 52 weeks with debug = on


# system.time(source(paste(getwd(),"/CommandDeck.R",sep=""))) # 9 minutes
# 
# system.time(source(paste(getwd(),"/(1)_simulate_setting.R",sep="")))            #6 sec
# system.time(source(paste(getwd(), "/(3)_disease_characteristics.R", sep = ""))) #0.01 sec
# system.time(source(paste(getwd(), "/(2)_inital_state.R", sep ="")))             #1.19 sec
# system.time(source(paste(getwd(), "/(4)_time_step.R", sep ="")))                #514.7 sec
# system.time(source(paste(getwd(), "/(5)_severe_outcomes_calc.R", sep = "")))    #0.5 sec
# system.time(source(paste(getwd(), "/(function)_severe_outcome_proj.R", sep = ""))) #1.2 sec
# 
# names(.GlobalEnv)


# model_weeks = 1
# system.time(source(paste(getwd(),"/(Table 2)_varying_eligb_age.R",sep="")))

CLUSTER <- parallel::makeCluster(4) # create cluster
doParallel::registerDoParallel(CLUSTER) # activate cluster

b=1
test_function <-function(i,a=b){
  return(i+a)
}

system.time({
  test <- foreach::foreach( i = 1:10,
      .combine = rbind,
      .inorder = FALSE
    ) %dopar% {
      test_function(i)
    }
})

parallel::stopCluster(CLUSTER)
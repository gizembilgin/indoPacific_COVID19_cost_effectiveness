
this_perspective = "healthcare"
this_antiviral_scenario = "middle_income_cost"
this_discounting_rate = 0.03

timing_df = data.frame()

for (num_cores in c(5,4,3,2,1)){
  if (!(num_cores %in% timing_df$cores)){
    time.start=proc.time()[[3]] #let's see how long this runs for
    
    CommandDeck_CONTROLS =
      list(
        TOGGLE_perspective = this_perspective,
        TOGGLE_discounting_rate = this_discounting_rate,
        TOGGLE_antiviral_cost_scenario = this_antiviral_cost_scenario,
        
        TOGGLE_longCOVID = "off",
        TOGGLE_uncertainty = "rand",
        TOGGLE_numberOfRuns = 100, #1000 eventually
        TOGGLE_clusterNumber = num_cores,  #4 or 5? test and time!
        DECISION_save_result = "N"
      )
    
    source(paste(getwd(),"/CommandDeck.R",sep=""))
    
    time.end=proc.time()[[3]]
    
    time_this_run = time.end-time.start
    this_row = data.frame(cores = num_cores, 
                          time_seconds = time_this_run,
                          time_minutes = time_this_run/60)
    timing_df = rbind(timing_df,this_row)
  }
}

# cores time_seconds time_minutes
# 5       962.73     16.04550
# 4      1075.00     17.91667

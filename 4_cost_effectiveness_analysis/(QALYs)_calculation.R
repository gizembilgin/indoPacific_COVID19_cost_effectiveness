QALYs_all = rbind(QALYs_nonFatal,QALYs_fatal)
ggplot(QALYs_all) + 
  geom_col(aes(x=age_group,y=QALYs,fill=as.factor(ISO3_code)),position="dodge") +
  theme_bw() +
  labs(fill="")+ 
  facet_grid(severity ~ ., scale = "free_y") 
#NB: no uncertainty in these estimates since data sources are expert estimates of: population (UN), life expectancy (UN), HRQoL (Robinson, Eber & Hammitt), and age_severity_specific_QALYs (Robinson, Eber & Hammitt)

QALYs_calculation <- function(
    function_setting = this_setting
    ){ 

  return()  
}

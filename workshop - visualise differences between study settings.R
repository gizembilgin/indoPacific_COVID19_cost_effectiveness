

#### CONTACT MATRICES
WPR_settings = c("PNG","TLS","IDN","FJI","SLB","PHL")

plot_list = list()
for (i in 1:length(WPR_settings)){
  this_setting = WPR_settings[i]
  
  contact_matrix_setting <- data.frame(contact_all[[this_setting]])
  
  visualise = contact_matrix_setting %>%
    rownames_to_column() %>%
    gather(contacts,value,-rowname) %>%
    rename(age_group = rowname)
  
  plot_list[[i]] = ggplot(visualise, aes(x = age_group, y = contacts, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low="white", high="blue")+
    labs(title=paste(this_setting))
  
}
plot_list



### RISK GROUP DISTRIBUTIONS
workshop <- read.csv('1_inputs/risk_group_distribution_Clarke_et_al_2020.csv')

all_ages <- workshop %>% filter(age_group_charac == 'all ages')
ggplot() + geom_col(data = all_ages,aes(x=setting,y=high_risk)) 
ggplot() + geom_col(data = all_ages,aes(x=setting,y=increased_risk)) 

age_specific <- workshop %>% filter(age_group_charac != 'all ages')
ggplot() + geom_line(data = age_specific,aes(x=age_group_num,y=high_risk,color=as.factor(setting)),size=1) 
ggplot() + geom_line(data = age_specific,aes(x=age_group_num,y=increased_risk,color=as.factor(setting)),size=1) 


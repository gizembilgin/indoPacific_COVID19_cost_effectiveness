

#### PLOTTING proportion of cases by age and vaccination status
## (A) by age
age_plot = data.frame()

for (i in 1:J){
  
  #select all in incidence_log_unedited in this age group
  workshop = incidence_log_unedited[,i+1]
  for (v in 1:(num_vax_doses*num_vax_types)){
    workshop = cbind(workshop,incidence_log_unedited[,i+J*v+1]) #+1 to avoid time
  }
  
  #sum across row
  workshop = as.data.frame(rowSums(workshop))
  colnames(workshop) <- c('incidence_num')
  
  workshop = cbind(age_group = age_group_labels[i], workshop)
  workshop$date = incidence_log_unedited$date
  workshop$incidence_percentage = workshop$incidence_num / incidence_log_unedited$daily_cases
  workshop$incidence_rate = 100000*workshop$incidence_num / pop[i]
  
  workshop <- workshop %>%
    mutate(cumulative_incidence = cumsum(incidence_num),
           cumulative_incidence_prop = 100*cumsum(incidence_num)/pop[i])
  
  age_plot = rbind(age_plot,workshop)
  
}

age_plot <- age_plot %>% select(date,age_group,incidence_num, incidence_percentage,incidence_rate,
                                cumulative_incidence,cumulative_incidence_prop)


# ggplot() + 
#   geom_line(data=age_plot,aes(x=date,y=incidence_percentage,color=as.factor(age_group)),na.rm=TRUE) + 
#   xlab("") +
#   labs(colour = "age group") +
#   theme_bw() + 
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         panel.border = element_blank(),
#         axis.line = element_line(color = 'black')) 

ggplot() + 
  geom_line(data=age_plot,aes(x=date,y=incidence_rate,color=as.factor(age_group)),na.rm=TRUE) + 
  xlab("") +
  labs(colour = "age group") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black')) 


plot1 <- ggplot() + 
  geom_line(data=age_plot,aes(x=date,y=cumulative_incidence,color=as.factor(age_group)),na.rm=TRUE) + 
  xlab("") +
  labs(colour = "age group") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black')) 

plot2 <- ggplot() + 
  geom_line(data=age_plot,aes(x=date,y=cumulative_incidence_prop,color=as.factor(age_group)),na.rm=TRUE) + 
  xlab("") +
  labs(colour = "age group") +
  ylab("cumulative cases % whole pop") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black')) 

grid.arrange(plot1, plot2, nrow=2)
#____________________________________________


## (B) by vaccination status
w_unvax = incidence_log_unedited[,2:(J+1)]
w_unvax = as.data.frame(rowSums(w_unvax))
w_unvax$vax_status = "unvax"
colnames(w_unvax) = c('incidence_num','vax_status')
w_unvax$incidence_percentage = w_unvax$incidence_num / incidence_log_unedited$daily_cases

w_doseone = incidence_log_unedited[,(J+2):((num_vax_types+1)*J+1)]
w_doseone = as.data.frame(rowSums(w_doseone))
w_doseone$vax_status = "dose one"
colnames(w_doseone) = c('incidence_num','vax_status')
w_doseone$incidence_percentage = w_doseone$incidence_num / incidence_log_unedited$daily_cases

w_dosetwo = incidence_log_unedited[,((num_vax_types+1)*J+2):((2*num_vax_types+1)*J+1)]
w_dosetwo = as.data.frame(rowSums(w_dosetwo))
w_dosetwo$vax_status = "dose two"
colnames(w_dosetwo) = c('incidence_num','vax_status')
w_dosetwo$incidence_percentage = w_dosetwo$incidence_num / incidence_log_unedited$daily_cases

vax_plot = as.data.frame(rbind(w_unvax,w_doseone,w_dosetwo))
vax_plot$date = incidence_log_unedited$date

vax_plot <- vax_plot %>% select(date,vax_status,incidence_num, incidence_percentage)


ggplot() + 
  geom_line(data=vax_plot,aes(x=date,y=incidence_percentage,color=as.factor(vax_status)),na.rm=TRUE) + 
  xlab("") +
  labs(colour = "vax_status") +
  #ylim(0,0.1)+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black')) 
#__________________________________________________________________________________________________



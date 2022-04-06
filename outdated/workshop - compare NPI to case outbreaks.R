
require(gridExtra)

#sec.axis seems impossible

workshop <- NPI_estimates %>% left_join(case_history)

plot1 <- ggplot(workshop, aes(x=date)) + 
  geom_point(aes(y=rolling_average),na.rm=TRUE) + 
  xlab("") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

plot2 <- ggplot(workshop, aes(x=date)) + 
 geom_line (aes(y=NPI)) +
  xlab("") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))

grid.arrange(plot1, plot2, nrow=2)

# Katie: "Is this just saying that the government reads their own data?"
# To be honest, it doesn't even look like NPIs react to case numbers!



## compare two NPI metrics
ggplot(NPI_estimates_full, aes(x=date)) + 
  geom_line(aes(y=stringency,color="stringency"),na.rm=TRUE) + 
  geom_line(aes(y=contain_health,color="contain_health"),na.rm=TRUE) + 
  xlab("") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'))


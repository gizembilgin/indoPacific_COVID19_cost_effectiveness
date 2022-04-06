options(scipen = 100)




aggregate(next_state$pop, by=list(next_state$age_group), FUN=sum)
pop
#CHECKED: pop remains constant

aggregate(next_state$pop, by=list(next_state$age_group,next_state$dose), FUN=sum)
aggregate(next_state$pop, by=list(next_state$dose), FUN=sum)
aggregate(next_state$pop, by=list(next_state$vaccine_type), FUN=sum)

aggregate(next_state$pop,by=list(next_state$dose,next_state$age_group),FUN=sum)

#### This program calculates R0, and Reff
require(tidyverse)

#### R0 #########################################################################
# R0 = p(NGM) - spectral radius, i.e., absolute maximum eigenvalue
# NGM = contact_matrix_adjusted * diag{beta*suscept*(gamma+lota(1-gamme)*1/delta)}

#(A) setup
if (strain == 'WT'){R0_to_fit = 2.79
} else if (strain == "delta"){R0_to_fit = 5.08
} else if (strain == "omicron"){R0_to_fit = 5.08*1.64} #World Health Organization. (2022). COVID-19 weekly epidemiological update, edition 82, 8 March 2022. World Health Organization. https://apps.who.int/iris/handle/10665/352390. License: CC BY-NC-SA 3.0 IGO
#} else if (strain == "omicron"){R0_to_fit = 5.08}

#COMEBACK - is R0 lower in SLE and PNG?
#R0_to_fit = R0_to_fit*0.79

##testing reduced R0
# if (setting == "PNG"){ R0_to_fit = R0_to_fit*0.7
# } else if (setting == "SLE"){ R0_to_fit = R0_to_fit * 0.8}

contact_matrix_adjust = matrix(data = 0, nrow = num_age_groups, ncol = num_age_groups)
for (i in 1:num_age_groups){
  for (j in 1:num_age_groups){
    contact_matrix_adjust[i,j] = contact_matrix[i,j] * pop[i]/pop[j]
  }
}

#(B) ballpark beta
contact_sum = rowSums(contact_matrix[,1:ncol(contact_matrix)])
contact_ave=sum(contact_sum*(pop/sum(pop))) 
beta_ball_park = R0_to_fit/(contact_ave*AverageSymptomaticPeriod)

#(C) fit!
minimise_this <- function(beta) {
  diag_matrix = beta*AverageSymptomaticPeriod*(gamma+(1-gamma)*lota)
  diag_matrix = diag(diag_matrix,num_age_groups)
  diag_matrix = suscept*diag_matrix
  
  NGM_R0 <- contact_matrix_adjust %*% diag_matrix
  R0_beta <- abs(eigen(NGM_R0)$values[1])
  
  fit = abs(R0_to_fit-R0_beta)
  
  return(fit)
}

#(D) check fit!
beta_optimised = optimize(minimise_this,c(beta_ball_park*1/2,beta_ball_park*2))$minimum
beta_optimised = rep(beta_optimised,num_age_groups)

beta_check = beta_optimised[1]
diag_matrix = beta_check*AverageSymptomaticPeriod*(gamma+(1-gamma)*lota)
diag_matrix = diag(diag_matrix,num_age_groups)
diag_matrix = suscept*diag_matrix
NGM_R0 <- contact_matrix_adjust %*% diag_matrix
R0_beta <- abs(eigen(NGM_R0)$values[1])
R0_beta; R0_to_fit #COMEBACK - should these match?
#_______________________________________________________________________________




#### Reff ######################################################################
Reff_time_step <- function(parameters,next_state){

#NGM_modified = NGM_R0 * (1-NPI) * (1-pre-existing immunity)

#(A) NPI  
NGM_modified = NGM_R0 * (1-parameters$NPI*(1+behaviour_mod))  
  
#(B) pre-existing immunity, i.e., vax and recovery
if (round(sum(next_state$pop) - sum(pop),digits =0 ) == 0) {
  next_state_immunity = next_state
  
  #immunity from infection
  next_state_immunity$pop[next_state_immunity$class == 'R'] = next_state_immunity$pop[next_state_immunity$class == 'R']  * (1-rho)
  
  #immunity from vaccination
  for (i in 1:J){
    for (t in 1:T){
      for (d in 1:D){
        next_state_immunity$pop[next_state_immunity$dose == d & next_state_immunity$vaccine_type == vax_type_list[t] & next_state$age_group == age_group_labels[i]] = 
          next_state_immunity$pop[next_state_immunity$dose == d & next_state_immunity$vaccine_type == vax_type_list[t] & next_state$age_group == age_group_labels[i]] * 
          (1-VE$VE[VE$dose==d & VE$vaccine_type == vax_type_list[t]& VE$age_group == age_group_labels[i]])
      }
    }
  }
  
  immunity = sum(next_state_immunity$pop)/sum(pop)
  
  NGM_modified = NGM_modified * immunity
  
} else{
  stop("issue with time_step population fluctating")
}

  Reff  <- abs(eigen(NGM_modified)$values[1]) 
  return(Reff)
}




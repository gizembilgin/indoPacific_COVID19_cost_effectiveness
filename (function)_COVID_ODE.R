### This program contains the system of ordinary differential equations (ODEs) for COVID-19 transmission

covidODE <- function(t, state, parameters){
  require(deSolve)
  
  with(as.list(c(state,parameters)),{
    
    J=num_age_groups
    T=num_vax_types
    D=num_vax_doses
    
    A=J*(T*D+1) # +1 is unvax
    
    S=state[1:A]
    E=state[(A+1):(2*A)]
    I=state[(2*A+1):(3*A)]
    R=state[(3*A+1):(4*A)]
    
    dS = dE = dI = dR = dIncidence  <- numeric(length=A)
 
    tau =(rep(0,J))
    
    for (i in 1:J){
      for (j in 1:J){
        
        total=0
        for(interval in 1:(num_disease_classes*(T*D+1))){
          total = total+state[j+(interval-1)*J]
        }
        
        # inclusion of reduced transmission from infected individuals
        total_infected_mod = I[j]
        for (t in 1:T){
          for (d in 1:D){
            B = j + J*(t+(d-1)*T)
            #total_infected_mod=total_infected_mod + (1-VE_onwards[t,d])*I[B]
            total_infected_mod=total_infected_mod + I[B]
          }
        }
        
        tau[i]=tau[i]+contact_matrix[i,j]*(total_infected_mod*(lota*(1-gamma[j])+gamma[j]))/(total)
      
      }
      tau[i]=tau[i]*(1-NPI*(1+behaviour_mod))*beta[i]*uniform_mod*suscept[i]
      tau[i]=max(min(1,tau[i]),0) #transmission can not be more than 1 (100%)
      
    }
    
    
    for (i in 1:J){
      #unvaccinated
      dS[i] = omega*R[i]  - tau[i]*S[i] 
      dE[i] = tau[i]*S[i] - lambda*E[i] + tau[i]*(1-rho)*R[i]
      dI[i] = lambda*E[i] - delta*I[i]
      dR[i] = delta*I[i]  - omega*R[i]  - tau[i]*(1-rho)*R[i]
      dIncidence[i] = lambda*E[i]
      
      for (t in 1:T){
        for (d in 1:D){
          #B = i+J+(t-1)*J+(d-1)*J*T = i+J(1+(t-1)+(d-1)*T)
          B = i + J*(t+(d-1)*T)
          
          dS[B] = omega*R[B]              - tau[i]*(1-VE[t,d])*S[B] 
          dE[B] = tau[i]*(1-VE[t,d])*S[B] - lambda*E[B] + tau[i]*(1-VE[t,d])*(1-rho)*R[B]
          dI[B] = lambda*E[B]             - delta*I[B]
          dR[B] = delta*I[B]              - omega*R[B]  - tau[i]*(1-VE[t,d])*(1-rho)*R[B]
          dIncidence[B] = lambda*E[B] 
          
        }
      }
    }
    
    dS = as.numeric(dS)
    dE = as.numeric(dE)
    dI = as.numeric(dI)
    dR = as.numeric(dR)
    
    list(c(dS,dE,dI,dR,dIncidence))  
  })
}


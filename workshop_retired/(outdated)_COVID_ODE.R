### This program contains the system of ordinary differential equations (ODEs) for COVID-19 transmission

covidODE <- function(t, state, parameters){
  require(deSolve)
  
  with(as.list(c(state,parameters)),{
    
    J=num_age_groups
    K=num_vax_types
    L=J*K
    
    S=state[1:J]
    Sv1=state[(J+1):(2*L)]
    Sv2=state[(2*L+1):(3*L)]
    Sv2wan=state[(3*L+1):(4*L)]
    Sv3=state[(4*L+1):(5*L)]
    
    Es=state[(5*L+1):(6*L)]
    Ev1=state[(6*L+1):(7*L)]
    Ev2=state[(7*L+1):(8*L)]
    Ev2wan=state[(8*L+1):(9*L)]
    Ev3=state[(9*L+1):(10*L)]
    
    Is=state[(10*L+1):(11*L)]
    Iv1=state[(11*L+1):(12*L)]
    Iv2=state[(12*L+1):(13*L)]
    Iv2wan=state[(13*L+1):(14*L)]
    Iv3=state[(14*L+1):(15*L)]
    
    Rs=state[(15*L+1):(16*L)]
    Rv1=state[(16*L+1):(17*L)]
    Rv2=state[(17*L+1):(18*L)]
    Rv2wan=state[(18*L+1):(19*L)]
    Rv3=state[(19*L+1):(20*L)]
    
    
    dS = dEs = dIs = dRs = dIncidence_unvax  <- numeric(length=J)
    dSv1 = dSv2 = dSv2wan = dSv3 <- numeric(length=L)
    dEv1 = dEv2 = dEv2wan = dEv3 <- numeric(length=L)
    dIv1 =  dIv2 =  dIv2wan = dIv3 <- numeric(length=L)
    dRv1  = dRv2  =  dRv2wan = dRv3  <- numeric(length=L)
    dIncidence_v1 = dIncidence_v2 = dIncidence_v2wan = dIncidence_v3 <- numeric(length=L)
    
    tau =(rep(0,num_age_groups))
    
    for (i in 1:num_age_groups){
      for (j in 1:num_age_groups){
        
        total=0
        for(interval in 1:(num_disease_classes*num_vax_classes*num_vax_types)){
          total = total+state[j+(interval-1)*num_age_groups]
        }
    
        total_infected    = Is[j] + Iv1[j] + Iv2[j] + Iv2wan[j] + Iv3[j]
 
        
        # inclusion of reduced transmission from infected individuals
        total_infected_mod = c(Is[j],Iv1[j],Iv2[j],Iv2wan[j],Iv3[j])
        total_infected_mod = total_infected_mod * (1-VE_onwards[j,])
        total_infected_mod = sum(total_infected_mod)
        
        #tau[i]=tau[i]+contact_matrix[i,j]*(total_infected*(lota*(1-gamma[j])+gamma[j]))/(total)
        tau[i]=tau[i]+contact_matrix[i,j]*(total_infected_mod*(lota*(1-gamma[j])+gamma[j]))/(total)
      }
      tau[i]=tau[i]*(1-TTIQ)*beta[i]
      tau[i]=max(min(1,tau[i]),0) #transmission can not be more than 1 (100%)
     
     for (k in 1:num_vax_types){
       i2 = i+K*(k-1)
      # dS[i]   = -tau[i]*S[i]               + omega*Rs[i]
      # dSv1[i] = -tau[i]*(1-VE[i,1])*Sv1[i] + omega*Rv1[i]
      # dSv2[i] = -tau[i]*(1-VE[i,2])*Sv2[i] + omega*Rv2[i]
      # dSv3[i] = -tau[i]*(1-VE[i,3])*Sv3[i] + omega*Rv3[i]
      
      dS[i]   = -tau[i]*S[i]               
      dSv1[i2] = -tau[i]*(1-VE[i,1])*Sv1[i] 
      dSv2[i] = -tau[i]*(1-VE[i,2])*Sv2[i] 
      dSv2wan[i] = -tau[i]*(1-VE[i,3])*Sv2wan[i] + omega*Rs[i]+ omega*Rv1[i]+ omega*Rv2[i] + omega*Rv2wan[i]
      dSv3[i] = -tau[i]*(1-VE[i,4])*Sv3[i] + omega*Rv3[i]
      
      dEs[i]  = tau[i]*S[i]               - lambda*Es[i]
      dEv1[i] = tau[i]*(1-VE[i,1])*Sv1[i] - lambda*Ev1[i]
      dEv2[i] = tau[i]*(1-VE[i,2])*Sv2[i] - lambda*Ev2[i]
      dEv2wan[i] = tau[i]*(1-VE[i,3])*Sv2wan[i] - lambda*Ev2wan[i]
      dEv3[i] = tau[i]*(1-VE[i,4])*Sv3[i] - lambda*Ev3[i]
      
      dIs[i]  = lambda*Es[i]  - delta*Is[i]
      dIv1[i] = lambda*Ev1[i] - delta*Iv1[i]
      dIv2[i] = lambda*Ev2[i] - delta*Iv2[i]
      dIv2wan[i] = lambda*Ev2wan[i] - delta*Iv2wan[i]
      dIv3[i] = lambda*Ev3[i] - delta*Iv3[i]
      
      dRs[i]  = delta*Is[i]  - omega*Rs[i]
      dRv1[i] = delta*Iv1[i] - omega*Rv1[i]
      dRv2[i] = delta*Iv2[i] - omega*Rv2[i]
      dRv2wan[i] = delta*Iv2wan[i] - omega*Rv2wan[i]
      dRv3[i] = delta*Iv3[i] - omega*Rv3[i]
      
      #dIncidence[i]=tau[i]*(S[i]+Sv1[i]*(1-VE[i,1]) + Sv2[i]*(1-VE[i,2]))
      #dIncidence[i]=lambda*Es[i] + lambda*Ev1[i] + lambda*Ev2[i] + lambda*Ev3[i]
      
      dIncidence_unvax[i] = lambda*Es[i]
      dIncidence_v1[i] = lambda*Ev1[i]
      dIncidence_v2[i] = lambda*Ev2[i]
      dIncidence_v2wan[i] = lambda*Ev2wan[i]
      dIncidence_v3[i] = lambda*Ev3[i]
     }
    }
    
    list(c(dS,dSv1,dSv2,dSv2wan,dSv3,
           dEs,dEv1,dEv2,dEv2wan,dEv3,
           dIs,dIv1,dIv2,dIv2wan,dIv3,
           dRs,dRv1,dRv2,dRv2wan,dRv3,
           dIncidence_unvax,dIncidence_v1,dIncidence_v2,dIncidence_v2wan,dIncidence_v3
           ))  
  })
}
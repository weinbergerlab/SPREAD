## Create an MSIS function
msis <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
          I_ex_t=I_ex*airtravel[time]/100000
    
   seasonal.txn <- (1+b1*cos(2*pi*(time-phi*period)/period))# seasonality waves
   # 
    infectiousN <- I1 + rho1*I2 +rho2*I3 + rho2*I4 +rho2*I_ex_t
    lambda <- seasonal.txn*tau*contact*infectiousN/sum(state)
    period.birth.rate <- exp(B_m[time]/sum(state)/4.345)-1
    
    #Pull out the states  for the model
    M <-  state['M']
    S0 <-  state['S0']
    I1 <-  state['I1']
    
    S1 <-  state['S1']
    I2 <-  state['I2']
    
    S2 <-  state['S2']
    I3 <-  state['I3']
    
    S3 <-  state['S3']
    I4 <-  state['I4']
    
    dM <- period.birth.rate*sum(state) - omega*M - death.rate*M
    
    dS0 <- omega*M-lambda*S0 - death.rate*S0 
    
    dI1 <-  lambda*S0 - gamma1*I1  - death.rate*I1
    
    dS1 <-  gamma1*I1 - sigma1*lambda*S1- death.rate*S1
    
    dI2 <-  sigma1*lambda*S1 - gamma2*I2- death.rate*I2
    
    dS2 <-  gamma2*I2 - sigma2*lambda*S2 - death.rate*S2 
    
    dI3 <-  sigma2*lambda*S2 - gamma3*I3 - death.rate*I3
    dS3 <-  gamma3*I3 +  gamma4*I4 - sigma3*lambda*S3- death.rate*S3
    dI4 <- sigma3*lambda*S3 - gamma4*I4 - death.rate*I4
    
    return(list(c(dM,dS0, dI1, dS1,dI2,dS2,dI3,dS3,dI4)))
  })
}

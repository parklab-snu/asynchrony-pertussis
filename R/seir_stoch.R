simulate_seir_stoch <- function(S0,
                                I0,
                                R0=17,
                                delta,
                                pop=1000,
                                sigma=-log(1-7/8),
                                gamma=-log(1-7/15),
                                tmax=52) {
  Svec <- Evec <- Ivec <- Cvec <- rep(0, tmax)
  
  Svec[1] <- round(S0 * pop)
  Evec[1] <- round(I0 * pop)
  Ivec[1] <- round(I0 * pop)
  
  beta <- R0 * (1-exp(-gamma))
  
  for (i in 2:tmax) {
    foi <- beta * (Ivec[i-1]+0.1) * delta[i]/pop
    StoE <- rbinom(1, Svec[i-1], (1-exp(-foi)))
    EtoI <- rbinom(1, Evec[i-1], (1-exp(-sigma)))
    ItoR <- rbinom(1, Ivec[i-1], (1-exp(-gamma)))
    
    Svec[i] <- Svec[i-1] - StoE
    Evec[i] <- Evec[i-1] + StoE - EtoI
    Ivec[i] <- Ivec[i-1] + EtoI - ItoR
    Cvec[i] <- EtoI
  }
  
  list(
    Svec=Svec,
    Evec=Evec,
    Ivec=Ivec,
    Cvec=Cvec
  )
}
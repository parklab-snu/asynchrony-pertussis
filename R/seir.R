simulate_seir <- function(S0,
                          I0,
                          R0=17,
                          delta,
                          sigma=-log(1-7/8),
                          gamma=-log(1-7/15),
                          tmax=52) {
  Svec <- Evec <- Ivec <- Cvec <- rep(0, tmax)
  
  Svec[1] <- S0
  Evec[1] <- I0
  Ivec[1] <- I0
  
  beta <- R0 * (1-exp(-gamma))
  
  for (i in 2:tmax) {
    foi <- beta * Ivec[i-1] * delta[i]
    StoE <- (1-exp(-foi)) * Svec[i-1]
    EtoI <- (1-exp(-sigma)) * Evec[i-1]
    ItoR <- (1-exp(-gamma)) * Ivec[i-1]
    
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
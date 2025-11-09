data {
  int N;
  int Nregion;
  int cases[N,Nregion];
  real pop[Nregion];
  real sigma;
  real gamma;
  real delta[N];
  real R0;
}

parameters {
  real<lower=0, upper=1> S0[Nregion];
  real<lower=0, upper=1> I0[Nregion];
  real<lower=0, upper=1> rho[Nregion];
}

transformed parameters {
  matrix<lower=0>[N,Nregion] S;
  matrix<lower=0>[N,Nregion] E;
  matrix<lower=0>[N,Nregion] I;
  matrix<lower=0>[N,Nregion] C;
  
  real beta[Nregion];
  
  for (j in 1:Nregion) {
    beta[j] = R0 * (1-exp(-gamma));
  }
  
  for (j in 1:Nregion) {
    S[1,j] = S0[j] * pop[j];
    E[1,j] = I0[j] * pop[j];
    I[1,j] = I0[j] * pop[j];
    C[1,j] = 0;
  }
  
  for (i in 2:N) {
    for (j in 1:Nregion) {
      real foi = beta[j] * I[i-1,j]/pop[j] * delta[i];
      real StoE = (1-exp(-foi)) * S[i-1,j];
      real EtoI = (1-exp(-sigma)) * E[i-1,j];
      real ItoR = (1-exp(-gamma)) * I[i-1,j];
    
      S[i,j] = S[i-1,j] - StoE;
      E[i,j] = E[i-1,j] + StoE - EtoI;
      I[i,j] = I[i-1,j] + EtoI - ItoR;
      C[i,j] = EtoI * rho[j];
    }
  }
}

model {
  S0 ~ uniform(0, 1);
  I0 ~ normal(0, 0.001);
  rho ~ uniform(0, 1);
  
  for (i in 2:N) {
    for (j in 1:Nregion) {
      cases[i,j] ~ poisson(C[i,j]);
    }
  }
}

library(dplyr)
library(rstan)
source("../R/seir.R")
source("../script/script_data.R")

load("../stanfit/stanfit_region.rda")

pertussis_korea_spatial_region <- pertussis_korea_spatial %>%
  merge(population)

data <- pertussis_korea_spatial_region %>%
  group_by(region) %>%
  filter(
    time >= 2024 + (4-1)/12, # april
    time < 2025 + (4-1)/12, # april
    sum(cases) > 400
  )

data_spread <- data %>%
  select(time, cases, region) %>%
  spread(region, cases) %>%
  arrange(time)

ss <- summary(stanfit_region)

delta <- ss$summary[grepl("delta\\[", rownames(ss$summary)),6]

S0vec <- seq(0.13, 0.23, length.out=51)
I0vec <- exp(seq(log(7e-6), log(1.5e-3), length.out=51))

pardata <- expand.grid(S0vec, I0vec)

reslist <- vector('list', nrow(pardata))
for (i in 1:nrow(pardata)) {
  pp <- pardata[i,]
  
  out <- simulate_seir(
    S0=pp[[1]],
    I0=pp[[2]],
    delta=delta,
    R0=17
  )
  
  time <- data_spread$time
  
  reslist[[i]] <- data.frame(
    S0=pp[[1]],
    I0=pp[[2]],
    cog=sum(out$Cvec*time)/sum(out$Cvec),
    size=out$Svec[1]-tail(out$Svec,1),
    min_time=min(time[which(out$Cvec>1e-3)])
  )
}

simulate_seir_stanfit <- reslist %>%
  bind_rows

save("simulate_seir_stanfit",
     file="simulate_seir_stanfit.rda")

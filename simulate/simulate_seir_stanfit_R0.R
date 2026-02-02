library(dplyr)
library(rstan)
source("../R/seir.R")
source("../script/script_data.R")

load("../stanfit/stanfit_region_R0.rda")

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

ss <- summary(stanfit_region_R0)

delta <- ss$summary[grepl("delta\\[", rownames(ss$summary)),6]
S0 <- ss$summary[grepl("S0", rownames(ss$summary)),6]

sort(ss$summary[grepl("R0\\[", rownames(ss$summary)),6])
sort(ss$summary[grepl("I0\\[", rownames(ss$summary)),6])

R0vec <- seq(14.2, 22.2, length.out=51)
I0vec <- exp(seq(log(5e-6), log(2.3e-3), length.out=51))

pardata <- expand.grid(R0vec, I0vec)

reslist <- vector('list', nrow(pardata))
for (i in 1:nrow(pardata)) {
  pp <- pardata[i,]
  
  out <- simulate_seir(
    R0=pp[[1]],
    S0=S0,
    I0=pp[[2]],
    delta=delta
  )
  
  time <- data_spread$time
  
  reslist[[i]] <- data.frame(
    R0=pp[[1]],
    I0=pp[[2]],
    cog=sum(out$Cvec*time)/sum(out$Cvec),
    size=out$Svec[1]-tail(out$Svec,1),
    min_time=min(time[which(out$Cvec>1e-3)])
  )
}

simulate_seir_stanfit_R0 <- reslist %>%
  bind_rows

save("simulate_seir_stanfit_R0",
     file="simulate_seir_stanfit_R0.rda")

library(dplyr)
library(tidyr)
library(rstan)
library(loo)
source("../script/script_data.R")

load("../stanfit/stanfit_region_R0.rda")
load("../stanfit/stanfit_region.rda")

pertussis_korea_spatial_region_R0 <- pertussis_korea_spatial %>%
  merge(population)

data <- pertussis_korea_spatial_region_R0 %>%
  group_by(region) %>%
  filter(
    time >= 2024 + (4-1)/12, # april
    time < 2025 + (4-1)/12, # april
    sum(cases) <= 400
  )

data_pop <- data %>%
  group_by(region) %>%
  summarize(
    pop=unique(pop)
  )

data_spread <- data %>%
  select(time, cases, region) %>%
  spread(region, cases) %>%
  arrange(time)

cases <- unname(as.matrix(data_spread[,-1]))

ee_R0 <- rstan::extract(stanfit_region_R0, pars="C")$C

ndraw <- dim(ee_R0)[1]
N <- dim(ee_R0)[2]
Nregion <- dim(ee_R0)[3]

N_obs <- (N - 1) * Nregion
log_lik_R0 <- matrix(NA_real_, nrow = ndraw, ncol = N_obs)

k <- 1
for (i in 2:N) {
  for (j in 1:Nregion) {
    log_lik_R0[, k] <- dpois(cases[i, j], lambda = ee_R0[, i, j], log = TRUE)
    k <- k + 1
  }
}

ee <- rstan::extract(stanfit_region, pars = "C")$C

ndraw <- dim(ee)[1]
N <- dim(ee)[2]
Nregion <- dim(ee)[3]

log_lik_S0 <- matrix(NA_real_, nrow = ndraw, ncol = (N - 1) * Nregion)

k <- 1
for (i in 2:N) {
  for (j in 1:Nregion) {
    log_lik_S0[, k] <- dpois(cases[i, j], lambda = ee[, i, j], log = TRUE)
    k <- k + 1
  }
}

loo_R0 <- loo(log_lik_R0)
loo_S0 <- loo(log_lik_S0)

loo_compare(loo_R0, loo_S0)

waic_R0 <- waic(log_lik_R0)
waic_S0 <- waic(log_lik_S0)

loo_compare(waic_R0, waic_S0)

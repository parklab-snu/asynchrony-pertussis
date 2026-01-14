library(dplyr)
library(tidyr)
library(rstan)
source("../script/script_data.R")

load("../stanfit/stanfit_region_R0.rda")

ss_region_R0 <- summary(stanfit_region_R0)

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

Nregion <- ncol(data_spread)-1

standata <- list(
  N=nrow(data_spread),
  Nregion=Nregion,
  cases=unname(as.matrix(data_spread[,-1])),
  pop=data_pop$pop,
  sigma=-log(1-7/8),
  gamma=-log(1-7/15),
  delta=ss_region_R0$summary[grepl("delta\\[", rownames(ss_region_R0$summary)),6]
)

model <- stan_model("../stanmodel/seir_region_R0_delta.stan")

init_list <- list(
  R0=rep(mean(ss_region_R0$summary[grepl("R0\\[", rownames(ss_region_R0$summary)),6]), Nregion_R0),
  S0=ss_region_R0$summary[grepl("S0", rownames(ss_region_R0$summary)),6],
  I0=rep(mean(ss_region_R0$summary[grepl("I0\\[", rownames(ss_region_R0$summary)),6]), Nregion_R0),
  rho=rep(mean(ss_region_R0$summary[grepl("rho\\[", rownames(ss_region_R0$summary)),6]), ncol(data_spread)-1)
)

stanfit_all_R0_delta <- optimizing(model,
           data = standata,
           seed=112,
           init=init_list)

save("stanfit_all_R0_delta", file="stanfit_all_R0_delta.rda")

library(dplyr)
library(tidyr)
library(rstan)
library(egg)
source("../script/script_data.R")
source("../R/seir.R")

load("../stanfit/stanfit_region.rda")
load("../simulate/simulate_seir_stanfit.rda")

pertussis_korea_spatial_region <- pertussis_korea_spatial %>%
  merge(population)

data <- pertussis_korea_spatial_region %>%
  group_by(region) %>%
  filter(
    time >= 2024 + (4-1)/12, # april
    time < 2025 + (4-1)/12, # april
    sum(cases) > 400
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

standata <- list(
  N=nrow(data_spread),
  Nregion=ncol(data_spread)-1,
  cases=unname(as.matrix(data_spread[,-1])),
  pop=data_pop$pop,
  sigma=-log(1-7/8),
  gamma=-log(1-7/15)
)

ss <- summary(stanfit_region)

analysis_S0_region <- data.frame(
  region=data_pop$region,
  S0=unname(ss$summary[grepl("S0\\[", rownames(ss$summary)),6]),
  Sfinal=unname(ss$summary[grepl("S\\[52,", rownames(ss$summary)),6]),
  I0=unname(ss$summary[grepl("I0\\[", rownames(ss$summary)),6]),
  rho=unname(ss$summary[grepl("rho\\[", rownames(ss$summary)),6])
)

write.csv(analysis_S0_region,
          file="analysis_S0_region.csv")

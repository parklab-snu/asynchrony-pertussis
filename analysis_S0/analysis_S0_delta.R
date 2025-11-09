library(dplyr)
library(tidyr)
library(rstan)
source("../script/script_data.R")

load("../stanfit/stanfit_all_delta.rda")

pertussis_korea_spatial_region <- pertussis_korea_spatial %>%
  merge(population)

data <- pertussis_korea_spatial_region %>%
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

standata <- list(
  N=nrow(data_spread),
  Nregion=ncol(data_spread)-1,
  cases=unname(as.matrix(data_spread[,-1])),
  pop=data_pop$pop 
)

analysis_S0_delta <- data.frame(
  region=data_pop$region,
  S0=unname(stanfit_all_delta$par[grepl("S0\\[", names(stanfit_all_delta$par))]),
  Sfinal=unname(stanfit_all_delta$par[grepl("S\\[52,", names(stanfit_all_delta$par))]),
  I0=unname(stanfit_all_delta$par[grepl("I0\\[", names(stanfit_all_delta$par))]),
  rho=unname(stanfit_all_delta$par[grepl("rho\\[", names(stanfit_all_delta$par))])
)

write.csv(analysis_S0_delta,
          file="analysis_S0_delta.csv")

library(dplyr)
library(ncf)
source("../script/script_data.R")

pertussis_korea_spatial_region <- pertussis_korea_spatial %>%
  merge(population)

data <- pertussis_korea_spatial_region %>%
  group_by(region) %>%
  filter(
    sum(cases)>0,
    time >= 2024 + (5-1)/12, # may
    time < 2025 + (3-1)/12
  )

data_pop <- data %>%
  group_by(region) %>%
  summarize(
    pop=unique(pop),
    x=x[1],
    y=y[1]
  )

data_spread <- data %>%
  select(time, cases, region) %>%
  spread(region, cases) %>%
  arrange(time)

datamat <- log(as.matrix(data_spread[,-1])+1)

Sncf_fit <- Sncf(x=data_pop$x,
                 y=data_pop$y,
                 z=t(datamat),
                 latlon=TRUE)

analysis_synchrony <- data.frame(
  dist=c(Sncf_fit$boot$boot.summary$predicted$x),
  lwr=c(Sncf_fit$boot$boot.summary$predicted$y["0.025",]),
  median=c(Sncf_fit$boot$boot.summary$predicted$y["0.5",]),
  upr=c(Sncf_fit$boot$boot.summary$predicted$y["0.975",]),
  cbar=Sncf_fit$real$cbar
)

save("analysis_synchrony",
     file="analysis_synchrony.rda")

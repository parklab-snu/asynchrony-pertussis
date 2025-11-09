library(dplyr)
library(ncf)
source("../script/script_data.R")

pertussis_korea_spatial_region <- pertussis_korea_spatial %>%
  merge(population)

data <- pertussis_korea_spatial_region %>%
  group_by(region) %>%
  filter(
    sum(cases)>0
  )

analysis_R_t <- read.csv("../analysis_R_t/analysis_R_t.csv") %>%
  merge(population) %>%
  group_by(region) %>%
  arrange(region, time) %>%
  mutate(
    ccases=cumsum(cases)/pop,
    R=pmin(R, 5)
  ) %>%
  filter(
    region %in% unique(data$region),
    time >= 2024 + (5-1)/12, # may
    time < 2025 + (3-1)/12
  )

data_pop <- analysis_R_t %>%
  group_by(region) %>%
  summarize(
    pop=unique(pop),
    x=x[1],
    y=y[1]
  )

data_spread <- analysis_R_t %>%
  ungroup %>%
  select(time, R, region) %>%
  spread(region, R) %>%
  arrange(time)

datamat <- as.matrix(data_spread[,-1])

Sncf_fit <- Sncf(x=data_pop$x,
                 y=data_pop$y,
                 z=t(datamat),
                 latlon=TRUE,
                 na.rm=TRUE)

analysis_synchrony_Rt <- data.frame(
  dist=c(Sncf_fit$boot$boot.summary$predicted$x),
  lwr=c(Sncf_fit$boot$boot.summary$predicted$y["0.025",]),
  median=c(Sncf_fit$boot$boot.summary$predicted$y["0.5",]),
  upr=c(Sncf_fit$boot$boot.summary$predicted$y["0.975",]),
  cbar=Sncf_fit$real$cbar
)

save("analysis_synchrony_Rt",
     file="analysis_synchrony_Rt.rda")

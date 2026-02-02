library(dplyr)
library(tidyr)
library(rstan)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
library(egg)
source("../R/seir.R")
source("../script/script_data.R")

load("../simulate/simulate_seir_stanfit_R0.rda")

pardata <- read.csv("../analysis_S0/analysis_R0_region.csv")

ggplot(simulate_seir_stanfit_R0) +
  geom_raster(aes(R0, I0, fill=cog)) +
  geom_point(data=pardata, aes(R0, I0), shape=21, size=2, col="white") +
  scale_x_continuous("Basic reproduction number", expand=c(0,0)) +
  scale_y_log10("Initial infected, I(0)", expand=c(0,0)) +
  coord_cartesian(xlim=c(14.5, NA)) +
  scale_fill_viridis_c("Center of\ngravity",
                       breaks=month_break[4:6],
                       labels=month_label_nl[4:6],
                       option="A")

simdata <- lapply(c(0.13, 0.15, 0.17, 0.19, 0.21), function(x) {
  out <- simulate_seir(
    S0=x,
    I0=1e-4,
    delta=deltadata$delta,
    R0=17
  )
  
  data.frame(
    time=data_spread$time,
    C=out$Cvec,
    S0=x
  )
}) %>%
  bind_rows

library(dplyr)
library(tidyr)
library(rstan)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
library(egg)
source("../script/script_data.R")
source("../R/seir.R")

load("../stanfit/stanfit_region.rda")
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
ss_R0 <- summary(stanfit_region_R0)

fitdata <- data.frame(
  pred=ss$summary[grepl("C\\[", rownames(ss$summary)),6],
  pred_R0=ss_R0$summary[grepl("C\\[", rownames(ss_R0$summary)),6],
  region=rep(data_pop$region, standata$N),
  time=rep(data_spread$time, each=standata$Nregion)
)

g1 <- ggplot(fitdata) +
  geom_line(aes(time, pred_R0)) +
  geom_line(aes(time, pred), col="red", lty=2) +
  scale_x_continuous("Year", expand=c(0, 0),
                     limits=c(2024.25-1/104, 2025.231+1/104),
                     breaks=month_break,
                     labels=month_label_nl) +
  scale_y_continuous("Cases") +
  facet_wrap(~region, scale="free", nrow=7) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

ggsave("figure_compare_supp_1.pdf", g1, width=12, height=12)

pardata <- data.frame(
  S0=ss$summary[grepl("S0\\[", rownames(ss$summary)),6],
  R0=ss_R0$summary[grepl("R0\\[", rownames(ss_R0$summary)),6],
  I0=ss$summary[grepl("I0\\[", rownames(ss$summary)),6],
  I0_R0=ss_R0$summary[grepl("I0\\[", rownames(ss_R0$summary)),6],
  rho=ss$summary[grepl("rho\\[", rownames(ss$summary)),6],
  rho_R0=ss_R0$summary[grepl("rho\\[", rownames(ss_R0$summary)),6],
  region=rep(data_pop$region, standata$N),
  time=rep(data_spread$time, each=standata$Nregion)
)

ggplot(pardata) +
  geom_point(aes(R0, S0)) +
  scale_x_continuous("Basic reproduction number (original model)") +
  scale_y_continuous("Initial susceptible fraction (alternative model)")

ggplot(pardata) +
  geom_point(aes(I0_R0, I0)) +
  geom_abline(intercept=0, slope=1, lty=2) +
  scale_x_log10() +
  scale_y_log10()

ggplot(pardata) +
  geom_point(aes(rho_R0, rho)) +
  geom_abline(intercept=0, slope=1, lty=2) 

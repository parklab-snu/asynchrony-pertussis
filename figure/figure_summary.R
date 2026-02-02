library(dplyr)
library(tidyr)
library(rstan)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
library(egg)
source("../R/seir.R")
source("../script/script_data.R")

load("../stanfit/stanfit_region_R0.rda")
load("../simulate/simulate_seir_stanfit_R0.rda")

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
  dplyr::select(time, cases, region) %>%
  tidyr::spread(region, cases) %>%
  arrange(time)

pardata <- read.csv("../analysis_S0/analysis_R0_region.csv")

g1 <- ggplot(simulate_seir_stanfit_R0) +
  geom_raster(aes(R0, I0, fill=cog)) +
  geom_point(data=pardata, aes(R0, I0), shape=21, size=2, col="white") +
  scale_x_continuous("Basic reproduction number", expand=c(0,0)) +
  scale_y_log10("Initial infected, I(0)", expand=c(0,0)) +
  coord_cartesian(xlim=c(14.5, NA)) +
  scale_fill_viridis_c("Center of\ngravity",
                       breaks=month_break[4:6],
                       labels=month_label_nl[4:6],
                       option="A") +
  theme(
    legend.position = c(0.85, 0.75)
  )

ss <- summary(stanfit_region_R0)

deltadata <- data.frame(
  delta=ss$summary[grepl("delta\\[", rownames(ss$summary)),6]
)

simdata <- lapply(c(15, 16, 17, 18, 19), function(x) {
  out <- simulate_seir(
    S0=ss$summary[grepl("S0", rownames(ss$summary)),6],
    I0=1e-4,
    delta=deltadata$delta,
    R0=x
  )
  
  data.frame(
    time=data_spread$time,
    C=out$Cvec,
    S=out$Svec,
    R0=x
  )
}) %>%
  bind_rows

g2 <- ggplot(simdata) + 
  geom_line(aes(time, C, col=R0, group=R0), lwd=1) +
  scale_x_continuous("Year", expand=c(0, 0),
                     limits=c(2024.25-1/104, 2025.231+1/104),
                     breaks=month_break,
                     labels=month_label_nl) +
  scale_y_continuous("Incidence", expand=c(0, 0), limits=c(0, 0.02)) +
  scale_color_viridis_c("Basic\nreproduction\nnumber",
                        option="E") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    legend.position = c(0.85, 0.75)
  )

g3 <- ggplot(simdata) + 
  geom_line(aes(time, S, col=R0, group=R0), lwd=1) +
  scale_x_continuous("Year", expand=c(0, 0),
                     limits=c(2024.25-1/104, 2025.231+1/104),
                     breaks=month_break,
                     labels=month_label_nl) +
  scale_y_continuous("Susceptible fraction", expand=c(0, 0)) +
  scale_color_viridis_c("Basic\nreproduction\nnumber",
                        option="E") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    legend.position = "none"
  )

gcomb <- ggarrange(g1, g2, g3, nrow=1,
                   labels=c("A", "B", "C"))

ggsave("figure_summary.pdf",
       gcomb, width=12, height=5)

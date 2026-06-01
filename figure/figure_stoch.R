library(dplyr)
library(mgcv)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(gridExtra)
source("../script/script_data.R")

load("../analysis_stoch/analysis_stoch_baseline.rda")
load("../analysis_stoch/analysis_stoch_R0.rda")
load("../analysis_stoch/analysis_stoch_I0.rda")
load("../analysis_stoch/analysis_stoch_I0_R0.rda")

pertussis_korea_spatial_region <- pertussis_korea_spatial %>%
  merge(population)

data <- pertussis_korea_spatial_region %>%
  group_by(region) %>%
  filter(
    time >= 2024 + (4-1)/12, # april
    time < 2025 + (4-1)/12
  )

data_spread <- data %>%
  select(time, cases, region) %>%
  spread(region, cases) %>%
  arrange(time)

cordata <- cor(as.matrix(data_spread[,-1]))

corvec <- cordata[lower.tri(cordata)]
corvec[is.na(corvec)] <- 0

mean(corvec)

analysis_all <- bind_rows(
  analysis_stoch_baseline %>% mutate(group = "Baseline"),
  analysis_stoch_R0 %>% mutate(group = "R0"),
  analysis_stoch_I0 %>% mutate(group = "I0"),
  analysis_stoch_I0_R0 %>% mutate(group = "I0_R0")
) %>%
  mutate(
    group=factor(group,
                 levels=c("Baseline", "R0", "I0", "I0_R0"))
  )

g1 <- ggplot(analysis_all) +
  geom_histogram(aes(cor, fill=group, col=group), alpha=0.6, position="identity",
                 binwidth=0.01) +
  geom_vline(xintercept=mean(corvec), lty=2) +
  scale_x_continuous("Mean correlation coefficient", limits=c(0,1), expand=c(0, 0)) +
  scale_y_continuous("Counts") +
  scale_fill_discrete(
    labels = c(
      "Baseline" = "Baseline",
      "R0" = expression(R[0]),
      "I0" = expression(I(0)),
      "I0_R0" = expression(R[0]*","~I(0))
    )
  ) +
  scale_color_discrete(
    labels = c(
      "Baseline" = "Baseline",
      "R0" = expression(R[0]),
      "I0" = expression(I(0)),
      "I0_R0" = expression(R[0]*","~I(0))
    )
  ) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )

ggsave("figure_stoch.pdf", g1, width=6, height=4)

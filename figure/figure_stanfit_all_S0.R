library(dplyr)
library(sf)
library(tidyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(gridExtra)

analysis_S0_delta <- read.csv("../analysis_S0/analysis_S0_delta.csv")
analysis_S0_region <- read.csv("../analysis_S0/analysis_S0_region.csv")

analysis_S0_all <- bind_rows(
  analysis_S0_delta,
  analysis_S0_region
)

data_vacc <- read.csv("../data_processed/vacc_2015.csv") %>%
  select(region, vacc) %>%
  mutate(
    region=gsub("경기도", "경기", region),
    region=gsub("강원도", "강원", region),
    region=ifelse(region=="인천-남구", "인천-미추홀구", region),
    region=ifelse(region=="경남-진해구", "경남-창원시 진해구", region),
    region=ifelse(region=="경북-군위군", "대구-군위군", region),
    region=ifelse(region=="충북-청주시 청원군", "충북-청주시 청원구", region)
  )

data_comb <- merge(analysis_S0_all, data_vacc)

g1 <- ggplot(merge(analysis_S0_region, data_vacc)) +
  geom_smooth(aes(S0*100, vacc), method="lm",
              fullrange=TRUE, color="#EF6351", fill="#EF6351") +
  geom_point(aes(S0*100, vacc)) +
  scale_x_continuous("Estimated susceptible (%)") +
  scale_y_continuous("Vaccine coverage among 3 year olds, 2015 (%)") +
  ggtitle("Municipalities with >400 cases") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=0.7),
    legend.position = "none"
  )


g2 <- ggplot(data_comb) +
  geom_smooth(aes(S0*100, vacc), method="lm",
              fullrange=TRUE, color="#EF6351", fill="#EF6351") +
  geom_point(aes(S0*100, vacc)) +
  scale_x_continuous("Estimated susceptible (%)") +
  scale_y_continuous("Vaccine coverage among 3 year olds, 2015 (%)") +
  ggtitle("All municipalities") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=0.7),
    legend.position = "none"
  )

gcomb <- ggarrange(g1, g2, nrow=1, labels=c("A", "B"))

ggsave("figure_stanfit_all_S0.pdf", gcomb, width=8, height=4)

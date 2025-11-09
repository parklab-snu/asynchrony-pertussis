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

pred <- stanfit_all_delta$par[grepl("C\\[", names(stanfit_all_delta$par))]

fitdata <- data.frame(
  pred=pred,
  region=rep(data_pop$region, each=standata$N),
  time=rep(data_spread$time, standata$Nregion)
)

data_merge <- merge(data,fitdata) %>%
  mutate(
    region=factor(region,
                  levels=pertussis_korea_spatial_y$region)
  )

g1 <- ggplot(data_merge) +
  geom_raster(aes(year+week/52-1/52, region, fill=pmin(cases/pop, 8e-5)*1e5)) +
  geom_vline(xintercept = head(month_break, -1), lty=2, col="white") +
  scale_x_continuous("Year", expand=c(0, 0),
                     limits=c(2024.25-1/104, 2025.231+1/104),
                     breaks=month_break,
                     labels=month_label_nl) +
  scale_y_discrete("Municipality ordered by latitude") +
  scale_fill_viridis_c("Cases per\n100,000",
                       breaks=c(0, 2, 4, 6, 8),
                       labels=c(0, 2, 4, 6, ">8")) +
  ggtitle("Observed") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=0.7),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

g2 <- ggplot(data_merge) +
  geom_raster(aes(year+week/52-1/52, region, fill=pmin(pred/pop, 8e-5)*1e5)) +
  geom_vline(xintercept = head(month_break, -1), lty=2, col="white") +
  scale_x_continuous("Year", expand=c(0, 0),
                     limits=c(2024.25-1/104, 2025.231+1/104),
                     breaks=month_break,
                     labels=month_label_nl) +
  scale_y_discrete("Municipality ordered by latitude") +
  scale_fill_viridis_c("Cases per\n100,000",
                       breaks=c(0, 2, 4, 6, 8),
                       labels=c(0, 2, 4, 6, ">8")) +
  ggtitle("Fitted") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=0.7),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

data_Rsquared <- data_merge %>%
  group_by(region) %>%
  summarize(
    Rsquared=cor(log(cases+1), log(pred+1))^2
  )

g3 <- ggplot(data_Rsquared) +
  geom_histogram(aes(Rsquared), bins=20, color="black", fill="white") +
  geom_vline(xintercept=median(data_Rsquared$Rsquared, na.rm=TRUE), lty=2) +
  scale_x_continuous("R squared", limits=c(0, 1)) +
  scale_y_continuous("Frequency", expand=c(0, 0), limits=c(0, 20)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1)
  )

gcomb <- ggarrange(g1, g2, g3, nrow=1, labels=c("A", "", "B"))

ggsave("figure_stanfit_all_delta.pdf", gcomb, width=10, height=5)

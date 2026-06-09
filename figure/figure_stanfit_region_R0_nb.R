library(dplyr)
library(tidyr)
library(rstan)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
library(egg)
source("../script/script_data.R")
source("../R/seir.R")

load("../stanfit/stanfit_region_R0_nb.rda")

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

ss <- summary(stanfit_region_R0_nb)

Cmat <- matrix(ss$summary[grepl("C\\[", rownames(ss$summary)),6],
               standata$N, standata$Nregion,
               byrow=TRUE)

fitdata <- data.frame(
  pred=ss$summary[grepl("C\\[", rownames(ss$summary)),6],
  pred_lwr=ss$summary[grepl("C\\[", rownames(ss$summary)),4],
  pred_upr=ss$summary[grepl("C\\[", rownames(ss$summary)),8],
  region=rep(data_pop$region, standata$N),
  time=rep(data_spread$time, each=standata$Nregion)
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

quantile(data_Rsquared$Rsquared, c(0.025, 0.5, 0.975))

g3 <- ggplot(data_Rsquared) +
  geom_histogram(aes(Rsquared), bins=20, color="black", fill="white") +
  geom_vline(xintercept=median(data_Rsquared$Rsquared), lty=2) +
  scale_x_continuous("R squared") +
  scale_y_continuous("Frequency", expand=c(0, 0), limits=c(0, 9),
                     breaks=0:4*2) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1)
  )

deltadata <- data.frame(
  delta=ss$summary[grepl("delta\\[", rownames(ss$summary)),6],
  delta_lwr=ss$summary[grepl("delta\\[", rownames(ss$summary)),4],
  delta_upr=ss$summary[grepl("delta\\[", rownames(ss$summary)),8],
  time=data_spread$time
)

data_merge_cog <- data_merge %>%
  group_by(region) %>%
  summarize(
    cog=sum(cases*time)/sum(cases),
    cog_pred=sum(pred*time)/sum(pred),
    size=sum(cases)/pop[1],
    x=x[1],
    y=y[1],
    pop=pop[1]
  )

rho <- round(cor(data_merge_cog$cog, data_merge_cog$cog_pred),3)

g4 <- ggplot(data_merge_cog) +
  geom_point(aes(cog, cog_pred)) +
  geom_abline(intercept=0, slope=1) +
  annotate("text", x=2024.95, y=2024.5, label=expression(paste(rho==0.997)),
           hjust=1, family="Times", vjust=0) +
  scale_x_continuous("Observed center of gravity",
                     breaks=month_break,
                     labels=month_label_nl) +
  scale_y_continuous("Predicted center of gravity",
                     breaks=month_break,
                     labels=month_label_nl) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1)
  )

pardata <- data.frame(
  R0_est=ss$summary[grepl("R0\\[", rownames(ss$summary)),6],
  R0_lwr=ss$summary[grepl("R0\\[", rownames(ss$summary)),4],
  R0_upr=ss$summary[grepl("R0\\[", rownames(ss$summary)),8],
  I0_est=ss$summary[grepl("I0\\[", rownames(ss$summary)),6],
  I0_lwr=ss$summary[grepl("I0\\[", rownames(ss$summary)),4],
  I0_upr=ss$summary[grepl("I0\\[", rownames(ss$summary)),8],
  rho_est=ss$summary[grepl("rho\\[", rownames(ss$summary)),6],
  rho_lwr=ss$summary[grepl("rho\\[", rownames(ss$summary)),4],
  rho_upr=ss$summary[grepl("rho\\[", rownames(ss$summary)),8],
  region=data_pop$region
) %>%
  merge(data_merge_cog) %>%
  mutate(
    region=factor(region,
                  levels=pertussis_korea_spatial_y$region)
  )

range(pardata$R0_est)
mean(pardata$R0_est)

ss$summary[grepl("S0", rownames(ss$summary)),6]
ss$summary[grepl("S0", rownames(ss$summary)),4]
ss$summary[grepl("S0", rownames(ss$summary)),8]

g5 <- ggplot(pardata) +
  geom_point(aes(R0_est, region)) +
  geom_errorbarh(aes(xmin=R0_lwr, xmax=R0_upr, y=region), height=0) +
  geom_vline(xintercept=mean(ss$summary[grepl("R0\\[", rownames(ss$summary)),6]), lty=2)  +
  scale_x_continuous("Basic reproduction number") +
  scale_y_discrete("Municipality ordered by latitude") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=0.7),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

g6 <- ggplot(deltadata) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(time, ymin=delta_lwr, ymax=delta_upr), alpha=0.2, fill="#EF6351") +
  geom_line(aes(time, delta), color="#EF6351") +
  scale_x_continuous("Year", expand=c(0, 0),
                     limits=c(2024.25-1/104, 2025.231+1/104),
                     breaks=month_break,
                     labels=month_label_nl) +
  scale_y_log10(expression("Multiplicative changes in R"),
                breaks=c(0.25, 0.5, 1, 2),
                expand=c(0, 0),
                limits=c(0.15, 2.1)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1)
  )

gcomb1 <- ggarrange(g1, g2, g3, g4, nrow=1, labels=c("A", "", "B", "C"))
gcomb2 <- ggarrange(g5, g6, nrow=1, labels=c("D", "E"),
                    widths=c(1, 2))

gfinal <- arrangeGrob(gcomb1, gcomb2, ncol=1)

ggsave("figure_stanfit_region_R0_nb.pdf", gfinal, width=10, height=6)

g_supp_1a <- ggplot(pardata) +
  geom_point(aes(I0_est, region)) +
  geom_errorbarh(aes(xmin=I0_lwr, xmax=I0_upr, y=region), height=0) +
  geom_vline(xintercept=mean(ss$summary[grepl("I0\\[", rownames(ss$summary)),6]), lty=2)  +
  scale_x_log10("Initial infected") +
  scale_y_discrete("Municipality ordered by latitude") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=0.7),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

g_supp_1b <- ggplot(pardata) +
  geom_point(aes(rho_est, region)) +
  geom_errorbarh(aes(xmin=rho_lwr, xmax=rho_upr, y=region), height=0) +
  geom_vline(xintercept=mean(ss$summary[grepl("rho\\[", rownames(ss$summary)),6]), lty=2)  +
  scale_x_continuous("Reporting rate") +
  scale_y_discrete("Municipality ordered by latitude") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=0.7),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

gcomb_supp_1 <- ggarrange(g_supp_1a, g_supp_1b, nrow=1,
                          labels=c("A", "B"))

ggsave("figure_stanfit_region_R0_nb_supp_1.pdf", gcomb_supp_1, width=8, height=4)

library(dplyr)
library(tidyr)
library(rstan)
library(ggplot2); theme_set(theme_bw())
library(egg)
source("../script/script_data.R")
source("../R/seir.R")

load("../stanfit/stanfit_region.rda")
load("../simulate/simulate_seir_stanfit.rda")

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
  S0=ss$summary[grepl("S0\\[", rownames(ss$summary)),6],
  I0=ss$summary[grepl("I0\\[", rownames(ss$summary)),6],
  rho=ss$summary[grepl("rho\\[", rownames(ss$summary)),6],
  region=data_pop$region
) %>%
  merge(data_merge_cog)

g5 <- ggplot(simulate_seir_stanfit) +
  geom_raster(aes(S0, I0, fill=cog)) +
  geom_point(data=pardata, aes(S0, I0, fill=cog), shape=21, size=2) +
  scale_x_continuous("Initial susceptible, S(0)", expand=c(0,0),
                     limits=c(0.129, NA)) +
  scale_y_log10("Initial infected, I(0)", expand=c(0,0)) +
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

g6 <- ggplot(simdata) + 
  geom_line(aes(time, C, col=S0, group=S0), lwd=1) +
  scale_x_continuous("Year", expand=c(0, 0),
                     limits=c(2024.25-1/104, 2025.231+1/104),
                     breaks=month_break,
                     labels=month_label_nl) +
  scale_y_continuous("Incidence", expand=c(0, 0), limits=c(0, 0.029)) +
  scale_color_viridis_c("S(0)",
                        option="E") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    legend.position = c(0.8, 0.6)
  )

g7 <- ggplot(deltadata) +
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
gcomb2 <- ggarrange(g5, g6, g7, nrow=1, labels=c("D", "E", "F"),
                    widths=c(1, 2, 1))

gfinal <- arrangeGrob(gcomb1, gcomb2, ncol=1)

ggsave("figure_stanfit_region.pdf", gfinal, width=10, height=6)

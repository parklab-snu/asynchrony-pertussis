library(dplyr)
library(mgcv)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(gridExtra)
source("../script/script_data.R")

load("../analysis_synchrony/analysis_synchrony.rda")
load("../analysis_synchrony/analysis_synchrony_Rt.rda")

analysis_R_t <- read.csv("../analysis_R_t/analysis_R_t.csv") %>%
  merge(population) %>%
  group_by(region) %>%
  arrange(region, time) %>%
  mutate(
    ccases=cumsum(cases)/pop,
    R=pmin(R, 5)
  )

analysis_R_t_filter <- analysis_R_t %>%
  group_by(region) %>%
  filter(
    sum(cases) > 400,
    time >= 2024 + (5-1)/12, # may
    time < 2025 + (3-1)/12 # march
  )

analysis_R_t_filter_summ <- analysis_R_t_filter %>%
  group_by(time) %>%
  summarize(
    median=median(R),
    lwr=quantile(R, 0.025),
    upr=quantile(R, 0.975)
  )

g1 <- ggplot(analysis_R_t_filter) +
  geom_ribbon(data=analysis_R_t_filter_summ, 
            aes(time, ymin=lwr, ymax=upr), color="#EF6351", fill="#EF6351", 
            alpha=0.4, lty=2) +
  geom_line(aes(time, R, group=region), alpha=0.3) +
  geom_line(data=analysis_R_t_filter_summ, 
            aes(time, median), color="#EF6351", lwd=2) +
  geom_hline(yintercept=1, lty=2) +
  scale_x_continuous(expand=c(0, 0),
                     breaks=month_break,
                     labels=month_label) +
  scale_y_log10("Effective reproduction number, R(t)", expand=c(0, 0),
                breaks=c(0.5, 1, 2, 4)) +
  coord_cartesian(ylim=c(0.3, 5)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

regionvec <- unique(analysis_R_t_filter$region)
Nregion <- length(regionvec)

corvec_Rt <- corvec_cases <- rep(0, Nregion*(Nregion-1)/2)

counter <- 1

for (i in 1:(Nregion-1)) {
  dd_i <- analysis_R_t_filter %>%
    filter(region==regionvec[i])
  for (j in (i+1):Nregion) {
    dd_j <- analysis_R_t_filter %>%
      filter(region==regionvec[j])
    
    corvec_Rt[counter] <- cor(dd_i$R,
                              dd_j$R)
    
    corvec_cases[counter] <- cor(log(dd_i$cases+1),
                                 log(dd_j$cases+1))
    
    counter <- counter + 1
  }
}

cordata <- data.frame(
  corvec_Rt=corvec_Rt,
  corvec_cases=corvec_cases
)

sum(cordata$corvec_Rt>cordata$corvec_cases)
nrow(cordata) 

g2 <- ggplot(cordata) +
  geom_point(aes(corvec_cases, corvec_Rt)) +
  geom_abline(lty=2) +
  scale_x_continuous("Pairwise correlation, cases", limits=c(0, 1), expand=c(0,0),
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_y_continuous("Pairwise correlation, R(t)", limits=c(0, 1), expand=c(0,0),
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1))  +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1)
  )

gfit <- gam(log(R)~-1+region+ccases+s(time), data=analysis_R_t_filter)
# -0.0011864
gfit_summ <- summary(gfit)

intercept <- data.frame(
  intercept=head(gfit_summ$p.coeff, -1)
)

newdata <- data.frame(
  time=seq(min(analysis_R_t_filter$time), max(analysis_R_t_filter$time), by=0.01),
  ccases=0,
  region="강원-원주시"
)

gfit_pred <- predict(gfit, newdata=newdata, type = "terms", se.fit = TRUE)

gfit_data <- data.frame(
  time=newdata$time,
  pred=gfit_pred$fit[,3],
  upr=gfit_pred$fit[,3]+1.96*gfit_pred$se.fit[,3],
  lwr=gfit_pred$fit[,3]-1.96*gfit_pred$se.fit[,3]
)

g3 <- ggplot(analysis_synchrony_Rt) +
  geom_ribbon(data=analysis_synchrony, aes(dist, ymin=lwr, ymax=upr), alpha=0.2, fill="gray") +
  geom_line(data=analysis_synchrony, aes(dist, median), color="gray") +
  geom_hline(data=analysis_synchrony[1,], aes(yintercept=cbar), lty=2, col="gray") +
  geom_ribbon(aes(dist, ymin=lwr, ymax=upr), alpha=0.2, fill="#EF6351") +
  geom_line(aes(dist, median), color="#EF6351") +
  geom_hline(data=analysis_synchrony_Rt[1,], aes(yintercept=cbar), lty=2) +
  scale_x_continuous("Distance (km)", expand=c(0, 0)) +
  scale_y_continuous("Correlation", limits=c(0, 1), expand=c(0, 0)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=0.7),
    legend.position = "none"
  )

g4 <- ggplot(intercept) +
  geom_violin(aes(x="Intercept", y=exp(intercept)), fill="lightblue") +
  geom_hline(yintercept=1, lty=2) +
  scale_y_log10("Multiplicative effect on R",
                breaks=c(0.5, 1, 2, 4), 
                limits=c(0.3, 5),
                expand=c(0, 0)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

g5 <- ggplot(analysis_R_t_filter) +
  geom_line(aes(time, exp(ccases*tail(gfit_summ$p.coeff, 1)), group=region), col="#EF6351",
            alpha=0.3) +
  geom_ribbon(data=gfit_data, aes(time, ymin=exp(lwr), ymax=exp(upr)), fill="orange", alpha=0.4) +
  geom_line(data=gfit_data, aes(time, exp(pred)), col="orange", lwd=1.2) +
  geom_hline(yintercept=1, lty=2) +
  scale_x_continuous(expand=c(0, 0),
                     breaks=month_break,
                     labels=month_label_nl) +
  scale_y_log10("Multiplicative effect on R",
                breaks=c(0.5, 1, 2, 4), 
                limits=c(0.3, 5),
                expand=c(0, 0)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

gcomb1 <- ggarrange(g1, g2, nrow=1, labels=c("A", "B"), widths=c(3, 1))
gcomb2 <- ggarrange(g3, g4, g5, nrow=1, widths=c(8, 1,8), labels=c("C", "D", ""))

gfinal <- arrangeGrob(gcomb1, gcomb2, ncol=1)

ggsave("figure_R_t.pdf", gfinal, width=8, height=6)
ggsave("figure_R_t.png", gfinal, width=8, height=6)

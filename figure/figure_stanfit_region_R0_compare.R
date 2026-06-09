library(dplyr)
library(tidyr)
library(sf)
library(geosphere)
library(rstan)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
library(egg)
source("../script/script_data.R")
source("../R/seir.R")

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

map_korea_ssg <- st_read('../data_shape/sig.shp')

map_korea_ssg$SIG_CD <- iconv(map_korea_ssg$SIG_CD,
                              from = 'CP949',
                              to = 'UTF-8',
                              sub = NA,
                              mark = TRUE,
                              toRaw = FALSE)

map_korea_ssg_shp <-  as(map_korea_ssg, 'Spatial')

map_korea_ssg_df <- fortify(map_korea_ssg_shp) %>%
  mutate(
    SIG_CD=map_korea_ssg$SIG_CD[as.numeric(id)]
  ) %>%
  mutate(
    SIG_CD=gsub("^51", "42", SIG_CD),
    SIG_CD=ifelse(SIG_CD=="27720", "47720", SIG_CD),
    SIG_CD=ifelse(SIG_CD=="28177", "28170", SIG_CD)
  )

map_korea_ssg_area <- map_korea_ssg %>%
  mutate(
    area = as.numeric(st_area(geometry))/1e6
  ) %>%
  as.data.frame %>%
  mutate(
    SIG_CD=gsub("^51", "42", SIG_CD),
    SIG_CD=ifelse(SIG_CD=="27720", "47720", SIG_CD),
    SIG_CD=ifelse(SIG_CD=="28177", "28170", SIG_CD)
  ) %>%
  select(
    SIG_CD, area
  )

data_pop2 <- data_pop %>%
  merge(
    pertussis_korea_spatial_y %>%
      select(region, SIG_CD)
  ) %>%
  merge(
    map_korea_ssg_area
  )

ss <- summary(stanfit_region_R0)

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
  merge(data_pop2) %>%
  mutate(
    region=factor(region,
                  levels=pertussis_korea_spatial_y$region)
  )

range(pardata$R0_est)
mean(pardata$R0_est)

g_supp_2a <- ggplot(pardata) +
  geom_smooth(aes(pop, R0_est), method="lm", col="#EF6351", fill="#EF6351") +
  geom_point(aes(pop, R0_est)) +
  scale_x_log10("Population size") +
  scale_y_continuous("Basic reproduction number") +
  theme(
    panel.grid = element_blank()
  )

g_supp_2b <- ggplot(pardata) +
  geom_smooth(aes(pop, I0_est), method="lm", col="#EF6351", fill="#EF6351") +
  geom_point(aes(pop, I0_est)) +
  scale_x_log10("Population size") +
  scale_y_log10("Initial infected") +
  theme(
    panel.grid = element_blank()
  )

g_supp_2c <- ggplot(pardata) +
  geom_smooth(aes(pop, rho_est), method="lm", col="#EF6351", fill="#EF6351") +
  geom_point(aes(pop, rho_est)) +
  scale_x_log10("Population size") +
  scale_y_continuous("Reporting rate") +
  theme(
    panel.grid = element_blank()
  )

g_supp_2d <- ggplot(pardata) +
  geom_smooth(aes(pop/area, R0_est), method="lm", col="#EF6351", fill="#EF6351") +
  geom_point(aes(pop/area, R0_est)) +
  scale_x_log10(expression("Population density per " * km^2)) +
  scale_y_continuous("Basic reproduction number") +
  theme(
    panel.grid = element_blank()
  )

cor.test(pardata$R0_est, log(pardata$pop/pardata$area))
cor.test(pardata$R0_est, pardata$pop/pardata$area)

g_supp_2e <- ggplot(pardata) +
  geom_smooth(aes(pop/area, I0_est), method="lm", col="#EF6351", fill="#EF6351") +
  geom_point(aes(pop/area, I0_est)) +
  scale_x_log10(expression("Population density per " * km^2)) +
  scale_y_log10("Initial infected") +
  theme(
    panel.grid = element_blank()
  )

g_supp_2f <- ggplot(pardata) +
  geom_smooth(aes(pop/area, rho_est), method="lm", col="#EF6351", fill="#EF6351") +
  geom_point(aes(pop/area, rho_est)) +
  scale_x_log10(expression("Population density per " * km^2)) +
  scale_y_continuous("Reporting rate") +
  theme(
    panel.grid = element_blank()
  )

pardata_age <- pardata %>%
  merge(data_processed_age)

g_supp_2g <- ggplot(pardata_age)+
  geom_smooth(aes(prop_school, R0_est), method="lm", col="#EF6351", fill="#EF6351") +
  geom_point(aes(prop_school, R0_est)) +
  scale_x_log10(expression("School-aged population fraction")) +
  scale_y_continuous("Basic reproduction number") +
  theme(
    panel.grid = element_blank()
  )

g_supp_2h <- ggplot(pardata_age)+
  geom_smooth(aes(prop_school, I0_est), method="lm", col="#EF6351", fill="#EF6351") +
  geom_point(aes(prop_school, I0_est)) +
  scale_x_log10(expression("School-aged population fraction")) +
  scale_y_log10("Initial infected") +
  theme(
    panel.grid = element_blank()
  )

g_supp_2i <- ggplot(pardata_age)+
  geom_smooth(aes(prop_school, rho_est), method="lm", col="#EF6351", fill="#EF6351") +
  geom_point(aes(prop_school, rho_est)) +
  scale_x_log10(expression("School-aged population fraction")) +
  scale_y_continuous("Reporting rate") +
  theme(
    panel.grid = element_blank()
  )

gcomb_supp_2 <- ggarrange(g_supp_2a, g_supp_2b, g_supp_2c, 
                          g_supp_2d, g_supp_2e, g_supp_2f, 
                          g_supp_2g, g_supp_2h, g_supp_2i,
                          nrow=3, labels=c("A", "B", "C",
                                           "D", "E", "F",
                                           "G", "H", "I"))

ggsave("figure_stanfit_region_R0_supp_2.pdf", gcomb_supp_2, width=8, height=8)

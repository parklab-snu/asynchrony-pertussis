library(dplyr)
library(sf)
library(geosphere)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(gridExtra)
source("../script/script_data.R")

load("../analysis_synchrony/analysis_synchrony.rda")

g1 <- ggplot(pertussis_korea_total) +
  geom_line(aes(year+week/52-1/52, cases)) +
  geom_point(aes(year+week/52-1/52, cases), size=0.5) +
  scale_x_continuous("Year", expand=c(0, 0),
                     breaks=month_break,
                     labels=month_label,
                     limits=c(2024-1/104, 2025+44/52+1/104)) +
  scale_y_continuous("Cases", expand=c(0, 0),
                     limits=c(0, 3600)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

## this puts jeju in the bottom...good
g2 <- ggplot(pertussis_korea_spatial_arr) +
  geom_raster(aes(year+week/52-1/52, region, fill=pmin(rel_cases, 8e-5)*1e5)) +
  geom_vline(xintercept = head(month_break, -1), lty=2, col="white") +
  scale_x_continuous("Year", expand=c(0, 0),
                     breaks=month_break,
                     labels=month_label) +
  scale_y_discrete("Municipality ordered by latitude") +
  scale_fill_viridis_c("Cases per\n100,000",
                       breaks=c(0, 2, 4, 6, 8),
                       labels=c(0, 2, 4, 6, ">8")) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=0.7),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank()
  )

gcomb1 <- ggarrange(g1, g2, nrow=2, heights=c(1, 3),
          labels=c("A", "B"))

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
  )%>%
  mutate(
    SIG_CD=gsub("^51", "42", SIG_CD),
    SIG_CD=ifelse(SIG_CD=="27720", "47720", SIG_CD),
    SIG_CD=ifelse(SIG_CD=="28177", "28170", SIG_CD)
  )

pertussis_korea_SIG <- pertussis_korea_spatial_arr %>%
  filter(!is.na(SIG_CD)) %>%
  group_by(SIG_CD) %>%
  summarize(
    cog=sum(time*cases)/sum(cases),
    pop=unique(pop),
    rel_cases=sum(cases)/pop,
    min_time=min(time[cases/pop>1e-5])
  )

map_korea_ssg_df_cases <- map_korea_ssg_df %>%
  merge(
    pertussis_korea_SIG
  ) %>%
  arrange(id, order)

g3 <- ggplot(map_korea_ssg_df_cases) +
  geom_polygon(aes(x=long, y=lat, group = group, fill=pmin(rel_cases, 0.004)*100000)) +
  scale_fill_viridis_c("Cases per 100,000", option="B", end=0.8,
                       breaks=c(0, 100, 200, 300, 400),
                       labels=c(0, 100, 200, 300, ">400")) +
  scale_y_continuous(expand=c(0, 0)) +
  guides(fill=guide_colorbar(title.position = 'bottom')) +
  coord_quickmap() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.key.width = unit(0.9, "cm"),
    legend.title.align = 0.5
  )

g4 <- ggplot(map_korea_ssg_df_cases) +
  geom_polygon(aes(x=long, y=lat, group = group, fill=cog))+
  scale_fill_gradientn("Center of gravity", colors=c("#D55E00", "#56B4E9", "#009E73"),
                       na.value = "gray20",
                       breaks=month_break[3:9],
                       labels=month_label_nl[3:9]) +
  scale_y_continuous(expand=c(0, 0)) +
  guides(fill=guide_colorbar(title.position = 'bottom')) +
  coord_quickmap() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.key.width = unit(0.9, "cm"),
    legend.title.align = 0.5
  )

pertussis_korea_spatial_region1 <- pertussis_korea_spatial %>%
  merge(population) %>%
  group_by(region1, time) %>%
  summarize(
    cases=sum(cases),
    pop=sum(pop)
  ) %>%
  merge(data_region_eng) %>%
  filter(region1 %in% c("서울", "부산", "인천", "대구")) %>%
  mutate(
    region_eng=factor(region1, 
                      levels=c("서울", "부산", "인천", "대구"),
                      labels=c("Seoul", "Busan", "Incheon", "Daegu"))
  )

g5 <- ggplot(pertussis_korea_spatial_region1) +
  geom_line(data=pertussis_korea_total, aes(year+week/52-1/52,
                                            cases/51.5e6*1e5), color="#EF6351",
            lty=2) +
  geom_line(aes(time, cases/pop*1e5)) +
  geom_point(aes(time, cases/pop*1e5), size=0.5) +
  scale_x_continuous("Year", expand=c(0, 0),
                     breaks=month_break[c(1, 3, 5, 7, 9, 11)],
                     labels=month_label_nl[c(1, 3, 5, 7, 9, 11)],
                     limits=c(2024-1/104, 2025+44/52+1/104)) +
  scale_y_continuous("Cases per 100,000", expand=c(0, 0)) +
  facet_wrap(~region_eng, nrow=1) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=0.7),
    axis.title.x = element_blank(),
    strip.background = element_blank()
  )

gcomb2 <- ggarrange(g3, g4, nrow=1, labels=c("D", "E"),
                    draw=FALSE)

gcomb3 <- ggarrange(g5, labels="C")

cor.test(filter(pertussis_korea_SIG, is.finite(min_time))$cog, 
         filter(pertussis_korea_SIG, is.finite(min_time))$min_time, use="complete.obs")

g6 <- ggplot(analysis_synchrony) +
  geom_ribbon(aes(dist, ymin=lwr, ymax=upr), alpha=0.2, fill="#EF6351") +
  geom_line(aes(dist, median), color="#EF6351") +
  geom_hline(data=analysis_synchrony[1,], aes(yintercept=cbar), lty=2) +
  scale_x_continuous("Distance (km)", expand=c(0, 0)) +
  scale_y_continuous("Correlation", limits=c(0, 1), expand=c(0, 0)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=0.7),
    legend.position = "none"
  )

g7 <- ggplot(pertussis_korea_SIG %>% filter(is.finite(min_time))) +
  geom_point(aes(min_time, cog), shape=21) +
  geom_smooth(aes(min_time, cog), method="lm",
              fullrange=TRUE, color="#EF6351", fill="#EF6351") +
  scale_x_continuous("Timing of introduction",
                     breaks=month_break,
                     labels=month_label_nl, expand=c(0, 0), limits=c(2023, 2026)) +
  scale_y_continuous("Center of gravity", expand=c(0, 0), limits=c(2023, 2026),
                     breaks=month_break,
                     labels=month_label_nl) +
  scale_size_area("Population\nsize") +
  coord_cartesian(xlim=c(2023.95, 2024.95), ylim=c(2024.3, 2025.5)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=0.7),
    legend.position = "none"
  )

gcomb4 <- ggarrange(g5, g6, g7, labels=c("C", "F", "G"), nrow=1, widths=c(3, 1, 1))

gfinal <- arrangeGrob(gcomb1, gcomb2, nrow=1, widths=c(1, 1))
gfinal3 <- arrangeGrob(gfinal, gcomb4, nrow=2, heights=c(2, 1))

ggsave("figure_data_spatial.pdf", gfinal3, width=15, height=8)
ggsave("figure_data_spatial.png", gfinal, width=15, height=8)

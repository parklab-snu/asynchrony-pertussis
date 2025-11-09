library(tidyverse)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(geofacet)
source("../script/script_data.R")

## based on this
## https://statkclee.github.io/spatial/spatial-geofacet.html

sido_grid_tbl <- tribble(~"region1", ~"fullname", ~"code", ~"row", ~"col",
                         "서울", "서울특별시",11,1,2,
                         "인천", "인천광역시",23,1,1,
                         "경기", "경기도",31,1,3,
                         "강원", "강원도",32,1,4,
                         "세종", "세종특별자치시",29,2,2,
                         "충북", "충청북도",33,2,3,
                         "충남", "충청남도",34,3,2,
                         "대전", "대전광역시",25,3,3,
                         "대구", "대구광역시",22,3,4,
                         "경북", "경상북도",37,2,4,
                         "울산", "울산광역시",26,3,5,
                         "부산", "부산광역시",21,4,5,
                         "경남", "경상남도",38,4,4,
                         "전북", "전라북도",35,4,3,
                         "광주", "광주광역시",24,5,2,
                         "전남", "전라남도",36,5,3,
                         "제주", "제주특별자치도",39,6,2) %>% 
  select(region1, code, row, col) %>%
  merge(data_region_eng) %>%
  select(-region1) %>%
  rename(
    name=region_eng
  )

pertussis_korea_spatial_region1 <- pertussis_korea_spatial %>%
  merge(population) %>%
  group_by(region1, time) %>%
  summarize(
    cases=sum(cases),
    pop=sum(pop)
  ) %>%
  merge(data_region_eng)

pertussis_korea_total2 <- pertussis_korea_total %>%
  mutate(
    pop=sum(population$pop)
  )

g1 <- ggplot(pertussis_korea_spatial_region1) +
  geom_line(data=pertussis_korea_total2, aes(year+week/52-1/52, cases/pop*1e5),
            col="red", lty=2) +
  geom_line(aes(time, cases/pop*1e5)) +
  scale_y_continuous("Cases per 100,000", expand=c(0, 0), limits=c(0, 23)) +
  scale_x_continuous("Year", expand=c(0, 0),
                     breaks=2024:2025) +
  facet_geo(~region_eng, grid=sido_grid_tbl) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(hjust=0),
    axis.title.x = element_blank(),
    strip.background = element_blank()
  )

ggsave("figure_data_region.pdf", g1, width=8, height=8)

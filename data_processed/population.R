library(dplyr)
library(readxl)

# https://jumin.mois.go.kr/

data <- read_xlsx("../data/popsize.xlsx",skip=2)

region_data <- data.frame(
  region_orig=c("서울특별시", "부산광역시", "대구광역시", "인천광역시", 
                "광주광역시", "대전광역시", "울산광역시", "경기도", 
                "강원특별자치도", "충청북도", "충청남도", "전북특별자치도", 
                "전라남도", "경상북도", "경상남도", "제주특별자치도"),
  region1=c("서울", "부산", "대구", "인천", 
            "광주", "대전", "울산", "경기", 
            "강원", "충북", "충남", "전북", 
            "전남", "경북", "경남", "제주")
)

data2 <- data %>%
  filter(grepl(" ", 행정기관)) %>%
  mutate(
    region_orig=gsub(" .*", "", 행정기관),
    region2=gsub("^[^ ]+ ", "", 행정기관)
  ) %>%
  merge(region_data) %>%
  select(
    region1, region2, 총인구수
  ) %>%
  mutate(
    region=paste0(region1, "-", region2)
  ) %>%
  select(
    region, 총인구수
  )

data3 <- data %>%
  filter(행정기관=="세종특별자치시",
         행정기관코드=="3611000000") %>%
  mutate(
    region="세종-세종시"
  ) %>%
  select(
    region, 총인구수
  )

population <- bind_rows(
  data2, data3
) %>%
  rename(
    pop=총인구수
  ) %>%
  mutate(
    pop=as.numeric(gsub(",", "", pop))
  )

write.csv(population, file="population.csv")

library(dplyr)

pertussis_korea_spatial <- read.csv("../data_processed/pertussis_korea_spatial.csv") %>%
  mutate(
    time=year+week/52-1/52
  )

population <- read.csv("../data_processed/population.csv") %>%
  select(-X)

pertussis_korea_spatial_y <- pertussis_korea_spatial %>%
  group_by(region) %>%
  filter(1:n()==1) %>%
  arrange(y)

pertussis_korea_spatial_arr <- pertussis_korea_spatial %>%
  mutate(
    region=factor(
      region,
      levels=pertussis_korea_spatial_y$region
    )
  ) %>%
  group_by(region) %>%
  merge(population) %>%
  mutate(
    rel_cases=cases/pop
  )

pertussis_korea_total <- pertussis_korea_spatial %>%
  group_by(year, week) %>%
  summarize(
    cases=sum(cases)
  )

month_break <- (c(
  as.Date(paste0("2024-", 1:6*2, "-01")),
  as.Date(paste0("2025-", 1:6*2, "-01"))
) - as.Date("2024-01-01"))/365+2024

month_label <- c(
  paste0(c("Feb", "Apr", "Jun", "Aug", "Oct", "Dec"), ", 2024"),
  paste0(c("Feb", "Apr", "Jun", "Aug", "Oct", "Dec"), ", 2025")  
)

month_label_nl <- gsub(", ", "\n", month_label)

korea_main_loc <- pertussis_korea_spatial %>%
  filter(region1 %in% c("서울", "부산", "대구", "인천",
                        "광주", "대전", "울산", "제주",
                        "세종")) %>%
  group_by(region1) %>%
  summarize(
    x=mean(x),
    y=mean(y)
  )
  
data_region_eng <- data.frame(
  region1=c("강원", "경기", "경남", "경북", "광주", "대구", 
            "대전", "부산", "서울", "세종", "울산", "인천", "전남", 
            "전북", "제주", "충남", "충북"),
  region_eng=c("Gangwon",   # 강원
               "Gyeonggi",  # 경기
               "Gyeongsangnam-do",  # 경남
               "Gyeongsangbuk-do",  # 경북
               "Gwangju",   # 광주
               "Daegu",     # 대구
               "Daejeon",   # 대전
               "Busan",     # 부산
               "Seoul",     # 서울
               "Sejong",    # 세종
               "Ulsan",     # 울산
               "Incheon",   # 인천
               "Jeollanam-do",  # 전남
               "Jeollabuk-do",  # 전북
               "Jeju",      # 제주
               "Chungcheongnam-do",  # 충남
               "Chungcheongbuk-do")  # 충북
)

vacc_2015 <- read.csv("../data_processed/vacc_2015.csv")

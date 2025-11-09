library(dplyr)
library(readxl)

# https://github.com/cubensys/Korea_District

data_2024 <- read_xlsx("../data/pertussis_korea_spatial_2024.xlsx", 
                       skip=5)

datalist_2024 <- vector('list', nrow(data_2024))

for (i in 1:nrow(data_2024)) {
  
  if(!data_2024[i,2] %in% c("서울", "부산", "대구", "인천", 
                        "광주", "대전", "울산", "경기",
                        "강원", "충북", "충남", "전북",
                        "전남", "경북", "경남", "제주",
                        "세종")) {
    datalist_2024[[i]] <- data.frame(
      cases=as.numeric(unname(unlist(data_2024[i,-c(1:3)]))),
      week=1:52,
      region1=unname(unlist(data_2024[i,1])),
      region2=unname(unlist(data_2024[i,2])),
      year=2024
    )
  }
}

data_2025 <- read_xlsx("../data/pertussis_korea_spatial_2025.xlsx", 
                       skip=5)

datalist_2025 <- vector('list', nrow(data_2025))

for (i in 1:nrow(data_2025)) {
  
  if(!data_2025[i,2] %in% c("서울", "부산", "대구", "인천", 
                            "광주", "대전", "울산", "경기",
                            "강원", "충북", "충남", "전북",
                            "전남", "경북", "경남", "제주",
                            "세종")) {
    datalist_2025[[i]] <- data.frame(
      cases=as.numeric(unname(unlist(data_2025[i,-c(1:3)]))),
      week=1:52,
      region1=unname(unlist(data_2024[i,1])),
      region2=unname(unlist(data_2024[i,2])),
      year=2025
    )
  }
}

data_coord <- read.csv("../data/data_coord.csv") %>%
  mutate(
    region=paste0(region1, "-", region2)
  )

data_all <- bind_rows(
  bind_rows(datalist_2024),
  bind_rows(datalist_2025)
) %>%
  filter(!is.na(cases)) %>%
  mutate(
    region=paste0(region1, "-", region2)
  )

leftout <- unique(data_all$region)[!unique(data_all$region) %in% data_coord$region]

pertussis_korea_spatial <- data_all %>%
  merge(data_coord) %>%
  bind_rows(
    data_all %>%
      filter(region %in% leftout) %>%
      mutate(
        x=(data_coord %>% filter(region2=="부천시"))$x,
        y=(data_coord %>% filter(region2=="부천시"))$y
      )
  )

write.csv(pertussis_korea_spatial, file="pertussis_korea_spatial.csv")

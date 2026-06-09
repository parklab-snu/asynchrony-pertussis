library(tidyr)
library(dplyr)
library(readxl)

metro <- c(
  "서울특별시", "부산광역시", "대구광역시", "인천광역시",
  "광주광역시", "대전광역시", "울산광역시",
  "세종특별자치시"
)

do <- c(
  "경기도", "강원도", "충청북도", "충청남도",
  "전라북도", "전라남도", "경상북도", "경상남도",
  "제주특별자치도"
)

## this function was written by Codex
convert_region <- function(x) {
  # remove full-width spaces and ordinary whitespace
  clean <- trimws(x, whitespace = "[ \t\r\n\u3000]")
  
  # rows with no leading indentation are top-level regions
  is_top <- !grepl("^[ \t\r\n\u3000]", x)
  
  sido_map <- c(
    "서울특별시" = "서울",
    "부산광역시" = "부산",
    "대구광역시" = "대구",
    "인천광역시" = "인천",
    "광주광역시" = "광주",
    "대전광역시" = "대전",
    "울산광역시" = "울산",
    "세종특별자치시" = "세종",
    "경기도" = "경기",
    "강원도" = "강원",
    "충청북도" = "충북",
    "충청남도" = "충남",
    "전라북도" = "전북",
    "전라남도" = "전남",
    "경상북도" = "경북",
    "경상남도" = "경남",
    "제주특별자치도" = "제주"
  )
  
  # optional rename, if needed
  name_map <- c(
    "통합창원시" = "창원시"
  )
  
  out <- character()
  current_sido <- NA_character_
  current_full_sido <- NA_character_
  current_si <- NA_character_
  
  for (i in seq_along(clean)) {
    nm <- clean[i]
    
    if (nm == "전국") next
    
    if (nm %in% names(name_map)) {
      nm <- name_map[[nm]]
    }
    
    if (is_top[i]) {
      current_full_sido <- nm
      current_sido <- sido_map[[nm]]
      current_si <- NA_character_
      next
    }
    
    if (current_full_sido %in% metro) {
      out <- c(out, paste0(current_sido, "-", nm))
      next
    }
    
    # For 도-level regions: 시/군 are direct children;
    # 구 after a 시 belongs to the most recent 시.
    if (grepl("구$", nm) && !is.na(current_si)) {
      out <- c(out, paste0(current_sido, "-", current_si, " ", nm))
    } else {
      out <- c(out, paste0(current_sido, "-", nm))
      
      if (grepl("시$", nm)) {
        current_si <- nm
      } else {
        current_si <- NA_character_
      }
    }
  }
  
  out
}

# https://kosis.kr/statHtml/statHtml.do?sso=ok&returnurl=https%3A%2F%2Fkosis.kr%3A443%2FstatHtml%2FstatHtml.do%3FtblId%3DDT_1B040M5%26orgId%3D101%26utm_source%3Dchatgpt.com%26
data <- read_xlsx("../data/age.xlsx") 

region_raw <- unname(unlist(data[,2]))

which_remove <- region_raw %in% c(metro, do)
which_remove[1] <- TRUE

region_con <- convert_region(region_raw)

total <- unname(unlist(data[,4]))[!which_remove]
school_age <- rowSums(data[,6:8])[!which_remove]

data_processed_age <- data.frame(
  region=region_con,
  shool=school_age,
  prop_school=school_age/total
)

write.csv(data_processed_age, file="data_processed_age.csv")

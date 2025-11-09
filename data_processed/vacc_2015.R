library(readxl)
library(dplyr)

data <- read_xlsx("../data_pdf/korea_dtap_vacc_2015.xlsx")

vacc_2015 <- data %>%
  mutate(
    region=paste0(region1, "-", si, " ", region),
    region=gsub("NA ", "", region),
    region=gsub(" NA", "", region)
  )

write.csv(vacc_2015,
          file="vacc_2015.csv")

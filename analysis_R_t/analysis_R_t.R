library(dplyr)
library(mgcv)
source("../script/script_data.R")

# weekly
# https://pmc.ncbi.nlm.nih.gov/articles/PMC12205447/
mean_gen <- 9.47/7
sd_gen <- 6.22/7 

shape <- (mean_gen / sd_gen)^2
scale <- (sd_gen^2) / mean_gen

gen <- diff(pgamma(c(0, 1.5, 2.5, 3.5, 4.5, 5.5), shape = shape, scale = scale))
gen <- gen/sum(gen)

analysis_R_t <- lapply(split(pertussis_korea_spatial, pertussis_korea_spatial$region), function(x) {
  x2 <- x %>%
    arrange(time)
  
  cases <- x2$cases
  
  cases2 <- exp(predict(gam(cases~s(time), data=x2, family=poisson)))
  
  n <- length(gen)
  
  R <- tail(cases2, -n)/sapply(1:(length(cases2)-n), function(z) sum(cases2[z:(z+n-1)] * rev(gen)))
  
  data.frame(
    time=x2$time,
    region=x2$region,
    SIG_CD=x2$SIG_CD,
    x=x2$x,
    y=x2$y,
    cases=cases,
    R=c(rep(NA,n), R)
  )
}) %>%
  bind_rows

write.csv(analysis_R_t, file="analysis_R_t.csv")

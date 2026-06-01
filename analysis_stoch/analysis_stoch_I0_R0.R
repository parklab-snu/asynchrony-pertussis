library(tidyr)
library(dplyr)
library(rstan)
source("../R/seir_stoch.R")
source("../script/script_data.R")

load("../stanfit/stanfit_region_R0.rda")

pertussis_korea_spatial_region <- pertussis_korea_spatial %>%
  merge(population)

data <- pertussis_korea_spatial_region %>%
  group_by(region) %>%
  filter(
    time >= 2024 + (4-1)/12, # april
    time < 2025 + (4-1)/12
  )

data_spread <- data %>%
  select(time, cases, region) %>%
  spread(region, cases) %>%
  arrange(time)

data_filter <- data %>%
  group_by(region) %>%
  filter(
    1:n()==1
  )

popsize <- data_filter$pop

ss <- summary(stanfit_region_R0)
R0vec <- c(
  ss$summary[grepl("R0\\[", rownames(ss$summary)),6],
  stanfit_all_R0_delta$par[grepl("R0\\[", names(stanfit_all_R0_delta$par))]
)
I0vec <- c(
  ss$summary[grepl("I0\\[", rownames(ss$summary)),6],
  stanfit_all_R0_delta$par[grepl("I0\\[", names(stanfit_all_R0_delta$par))]
)
delta <- ss$summary[grepl("delta\\[", rownames(ss$summary)),6]
S0 <- ss$summary[grepl("S0", rownames(ss$summary)),6]
rhovec <- c(
  ss$summary[grepl("rho", rownames(ss$summary)),6],
  stanfit_all_R0_delta$par[grepl("rho\\[", names(stanfit_all_R0_delta$par))]
)
rho <- median(rhovec)

nsim <- 500
nregion <- nrow(data_filter)

reslist <- vector('list', nsim)
for (i in 1:nsim) {
  print(i)
  allout <- sapply(1:nregion, function(x) {
    C <- simulate_seir_stoch(S0=S0,
                             I0=sample(I0vec,1),
                             R0=sample(R0vec,1),
                             pop=popsize[x],
                             delta=delta)$Cvec
    
    C <- rpois(1:length(C), lambda=C*rho)
  })
  
  allcor <- cor(log(allout+1))
  
  corvec <- allcor[lower.tri(allcor)]
  corvec[is.na(corvec)] <- 0
  
  reslist[[i]] <- data.frame(
    cor=mean(corvec, na.rm=TRUE),
    type="Stochastic (I0,R0)"
  )
}

analysis_stoch_I0_R0 <- reslist %>%
  bind_rows

save("analysis_stoch_I0_R0", file="analysis_stoch_I0_R0.rda")

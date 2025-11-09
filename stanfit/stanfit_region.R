library(dplyr)
library(tidyr)
library(rstan)
source("../script/script_data.R")

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

data_spread <- data %>%
  select(time, cases, region) %>%
  spread(region, cases) %>%
  arrange(time)

standata <- list(
  N=nrow(data_spread),
  Nregion=ncol(data_spread)-1,
  cases=unname(as.matrix(data_spread[,-1])),
  pop=data_pop$pop,
  sigma=-log(1-7/8),
  gamma=-log(1-7/15),
  R0=17
)

model <- stan_model("../stanmodel/seir_region.stan")

stanfit_region <- sampling(model,
                           data = standata,
                           seed=117,
                           chain=4,
                           cores=4,
                           iter=8000,
                           control=list(max_treedepth=15,
                                        adapt_delta=0.9))

save("stanfit_region", file="stanfit_region.rda")

ss <- summary(stanfit_region)

which.min(ss$summary[,"n_eff"])

plot(ss$summary[,"n_eff"], log="y")

(ss$summary[196:220,"n_eff"])

Cmat <- matrix(ss$summary[grepl("C\\[", rownames(ss$summary)),6],
       standata$N, standata$Nregion,
       byrow=TRUE)

cases <- unname(as.matrix(data_spread[,-1]))

plot(standata$cases[,6])
lines(Cmat[,6])

plot(ss$summary[grepl("S0\\[", rownames(ss$summary)),6])
plot(ss$summary[grepl("I0\\[", rownames(ss$summary)),6])

plot(ss$summary[grepl("delta\\[", rownames(ss$summary)),6])
lines(ss$summary[grepl("delta\\[", rownames(ss$summary)),4])
lines(ss$summary[grepl("delta\\[", rownames(ss$summary)),8])

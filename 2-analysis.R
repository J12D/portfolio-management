library(reshape2)
library(ggplot2)

source("0-helper.R")
source("1-data.R")

# Mean
asset_returns <- returns %>% apply(2,mean) * 252
factors %>% apply(2,mean)

# Variance & Correlation
var(returns)
var(factors)
cor(returns)
cor(factors)
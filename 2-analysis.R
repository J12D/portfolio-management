library(reshape2)
library(ggplot2)

source("0-helper.R")
source("1-data.R")

# Mean
asset_returns <- returns %>% apply(2,mean) * 252
asset_returns

# Variance & Correlation
var(returns)
var(factors)
cor(returns) %T>% print %>% corrplot
cor(factors) %T>% print %>% corrplot


## ---- linear-regression --------------------
d <- merge.xts(returns,factors) %>% na.omit

# build functions of linear models where we regress each asset on all factors
models <- list(DAX = "DAX", DJI = "Dow.Jones", NKK = "Nikkei", VIX = "VIX") %>%
  lapply(function(col)paste(col,"~ Mkt.RF + SMB + HML + RF") %>% as.formula) %>%
  lapply(function(formula)lm(formula,data = d))

# p values of all regressions for all factors
models %>% lapply(function(x)x %>% summary %>% coef) #%>% .[,4]



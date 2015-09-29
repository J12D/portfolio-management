library(reshape2)
library(ggplot2)

source("0-helper.R")
source("1-data.R")

# Mean
jdreturns <- returns[,1:4]
asset_returns <- jdreturns %>% apply(2,mean) * 252
asset_returns

# Variance & Correlation
var(returns)
var(factors)
cor(returns) %T>% print %>% corrplot
cor(factors) %T>% print %>% corrplot

weights <- (var(jdreturns) %>% solve %*% asset_returns)/as.numeric(rep(1,4) %*% (var(jdreturns) %>% solve) %*% asset_returns)

## ---- linear-regression --------------------
d <- merge.xts(returns,factors) %>% na.omit

# build functions of linear models where we regress each asset on all factors
models <- list(DAX = "DAX", DJI = "Dow.Jones", NKK = "Nikkei", VIX = "VIX") %>%
  lapply(function(col)paste(col,"~ Mkt.RF + SMB + HML + RF") %>% as.formula) %>%
  lapply(function(formula)lm(formula,data = d))

# p values of all regressions for all factors
models %>% lapply(function(x)x %>% summary %>% coef) #%>% .[,4]


## ---- Portfolio Analytics -----------
library(PortfolioAnalytics)
library(DEoptim)

pspec <- portfolio.spec(assets = colnames(returns)[1:4])
pspec %<>% add.constraint(type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
pspec %<>% add.constraint(type = "box", min = 0.05, max = 0.4)
pspec %<>% add.constraint(type = "turnover", turnover_target = 0.1)
pspec %<>% add.objective(type = "risk", name = "StdDev")
optimize.portfolio(returns, pspec)
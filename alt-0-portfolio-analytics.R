source("2-analysis.R")

## ---- Portfolio Analytics -----------
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)

# pspec <- portfolio.spec(assets = colnames(returns))
# pspec %<>% add.constraint(type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
# pspec %<>% add.constraint(type = "box", min = 0.05, max = 1.0)
# pspec %<>% add.constraint(type = "turnover", turnover_target = 0.1)
# pspec %<>% add.objective(type = "risk", name = "StdDev")
# result <- optimize.portfolio(returns, pspec)
# create.EfficientFrontier(returns, pspec, type = "mean-var") %>% chart.EfficientFrontier
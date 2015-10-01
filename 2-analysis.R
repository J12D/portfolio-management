library(reshape2)
library(ggplot2)
library(corrplot)

source("0-helper.R")
source("1-data.R")

# Mean
mu <- returns["/2012"] %>% apply(2,mean) * 252
mu

# Standard Deviation
sigma <- returns["/2012"] %>% apply(2,sd) * sqrt(252)
sigma

# Variance & Correlation
var(returns)
var(factors)
cor(returns) %T>% print %>% corrplot
cor(factors) %T>% print %>% corrplot

mean_variance <- function(mu,sigma) {
  information_matrix <- sigma %>% solve
  (information_matrix %*% mu)/as.numeric(rep(1,length(mu)) %*% information_matrix %*% mu)
}
mean_variance(mu,cov(returns))

## ---- Massage -----------------------
# shrink
library(tawny)
cov.shrink(returns)
mean_variance(mu,cov.shrink(returns))

# compute hayashi-yoshida adjustment for two timeseries
hayashi_yoshida <- function(ts_fixing_pre, ts_fixing_post) {
  ts_fixing_post %>% lag %>% head
  aligned <- merge.xts(ts_fixing_pre,ts_fixing_post)
  aligned %<>% na.omit
  cov(aligned[,1],aligned[,2])
}

# add a vector at a given position to a square matrix, both on row and on column (for hayashi)
addCross <- function(m,vec,pos) {
  l <- dim(m)[1]
  row <- c(rep(0, (pos - 1) * l), vec, rep(0, (l - pos) * l)) %>% matrix(nrow = l, byrow = T)
  col <- c(rep(0, (pos - 1) * l), vec, rep(0, (l - pos) * l)) %>% matrix(ncol = l)
  m + row + col
}

# compute hayashi-yoshida adjustment between DAX, Dow Jones, VIX and Nikkei
hayashi_results <- list(returns[,"DAX"], returns[,"Dow Jones"], returns[,"VIX"]) %>% lapply(function(x)hayashi_yoshida(returns$Nikkei,x)) %>% unlist
adjustment <- c(hayashi_results[1], hayashi_results[2], 0, hayashi_results[3])

# add adjustment onto covariance matrix
adjusted_cov <- returns %>% cov %>% addCross(adjustment,3)

# calculate correlation from adjusted covariance matrix (not perfect)
standard_devs <- returns %>% apply(2,sd)
adjusted_cor <- adjusted_cov / outer(standard_devs,standard_devs)
adjusted_cor %>% corrplot

mean_variance(mu,adjusted_cov)

## ----
# t_0: generate weights
# t_1: observe weights & generate new weights

generate_random_weight <- function(rand_gen, ntimes, nassets, sum=1) {
  m <- matrix(rand_gen(ntimes * nassets), ncol = nassets)
  apply(m,2,function(x)x/sum(x))
}

a <- matrix(rnorm(20),ncol=5)
a*(1+0.2)-1

1e6 * a * 1.2

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
library(ROI)

# pspec <- portfolio.spec(assets = colnames(returns))
# pspec %<>% add.constraint(type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
# pspec %<>% add.constraint(type = "box", min = 0.05, max = 1.0)
# pspec %<>% add.constraint(type = "turnover", turnover_target = 0.1)
# pspec %<>% add.objective(type = "risk", name = "StdDev")
# result <- optimize.portfolio(returns, pspec)
# create.EfficientFrontier(returns, pspec, type = "mean-var") %>% chart.EfficientFrontier

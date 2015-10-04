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

mean_variance_ibase <- function(mu, information_matrix) {
  (information_matrix %*% mu)/as.numeric(rep(1,length(mu)) %*% information_matrix %*% mu)
}

mean_variance_base <- function(mu, sigma) {
  mean_variance_ibase(mu, sigma %>% solve)
}

mean_variance_base(mu,cov(returns))

## ---- Massage -----------------------
# shrink
library(tawny)
cov.shrink(returns)
mean_variance_base(mu,cov.shrink(returns))

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
hayashi_results <- list(returns[,"DAX"], returns[,"Dow Jones"], returns[,"VIX"]) %>%
  lapply(function(x)hayashi_yoshida(returns$Nikkei,x)) %>%
  unlist
adjustment <- c(hayashi_results[1], hayashi_results[2], 0, hayashi_results[3])

# add adjustment onto covariance matrix
adjusted_cov <- returns %>% cov %>% addCross(adjustment,3)

# calculate correlation from adjusted covariance matrix (not perfect)
standard_devs <- returns %>% apply(2,sd)
adjusted_cor <- adjusted_cov / outer(standard_devs,standard_devs)
adjusted_cor %>% corrplot

mean_variance_base(mu,adjusted_cov)

## ----
# t_0: generate weights
# t_1: observe weights & generate new weights

generate_random_weight <- function(rand_gen, ntimes, nassets, sum=1) {
  m <- matrix(rand_gen(ntimes * nassets), ncol = nassets)
  apply(m,2,function(x)x/sum(x))
}

## ---- linear-regression --------------------
d <- merge.xts(returns,factors) %>% na.omit

# build functions of linear models where we regress each asset on all factors
models <- list(DAX = "DAX", DJI = "Dow.Jones", NKK = "Nikkei", VIX = "VIX") %>%
  lapply(function(col)paste(col,"~ Mkt.RF + SMB + HML + RF") %>% as.formula) %>%
  lapply(function(formula)lm(formula,data = d))

# p values of all regressions for all factors
models %>% lapply(function(x)x %>% summary %>% coef) #%>% .[,4]


## ---- Base Wrapper ------------------

## 2012-12-31
## 2013-01-31 -- 2015-06-30
require(tawny)

mean_variance_optimal <- function(mu, information_matrix, phi) {
  n <- length(mu)
  a3 <- sum(information_matrix %*% ones(n)) #1'S^(-1)1
  weights <- (information_matrix %*% ones(n)) / a3 + 
                1 / phi * (
                  a3 * information_matrix %*% mu -
                  sum(information_matrix %*% mu) * information_matrix %*% ones(n)
                ) / a3
  weights
}

# take vector of returns, return vector of weights
model <- function(returns) {
  weights <- mean_variance_optimal(apply(returns, 2, mean), cov(returns) %>% solve, 1)
  xts(weights %>% t, index(returns) %>% last)
}

# select an expanding window of returns, starting end of 2012 and feed it into the model
months_calibration <- index(returns["2012/"])[endpoints(returns["2012/"],"months")]

# returns for decision months
months_returns <- lag(assets[months_calibration],-1) / assets[months_calibration] - 1

weights <- months_calibration %>%
            lapply(function(x) returns[paste0("/", x)])  %>% # matrix of returns, expanding in time
            lapply(model) %>% # apply model to growing matrix timeseries
            Reduce(rbind,.) %>% # summarize weights vectors in one object
            .[-dim(.)[1],] # discard last row for now

# weights' * monthly_returns
rowSums(months_returns * weights, na.rm = T) %>%
  xts(index(weights)) %>%
  (function(x) cumprod(1 + x) - 1) %>%
  plotXTS

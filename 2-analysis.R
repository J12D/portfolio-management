library(reshape2)
library(ggplot2)
library(corrplot)

source("0-helper.R")
source("1-data.R")

## ---- Mean ----------
mean_returns <- function(shrink = 0, annualize = 252) {
  function(returns) {
    r <- returns %>% apply(2, mean) * annualize
    r * (1 - shrink) + Reduce(mean, r) * shrink
  }
}

## ---- Covariance -----------
hayashi_yoshida <- function(ts_fixing_pre, ts_fixing_post) {
  ts_fixing_post %>% lag %>% head
  aligned <- merge.xts(ts_fixing_pre,ts_fixing_post)
  aligned %<>% na.omit
  cov(aligned[,1],aligned[,2])
}

# add a vector at a given position to a square matrix, both on row and on column (for hayashi)
# 0 0 0                   0 1 0
# 0 0 0 + {1,2,3} @ 2 ==> 1 4 3
# 0 0 0                   0 3 0
addCross <- function(m,vec,pos) {
  l <- dim(m)[1]
  row <- c(rep(0, (pos - 1) * l), vec, rep(0, (l - pos) * l)) %>% matrix(nrow = l, byrow = T)
  col <- c(rep(0, (pos - 1) * l), vec, rep(0, (l - pos) * l)) %>% matrix(ncol = l)
  m + row + col
}

cov_returns <- function(lag_adjustment = F, shrink = F, annualize = 252) {
  function(returns) {
    c <- cov(returns)
    
    if (lag_adjustment != F) {
      hayashi_results <- returns[,-lag_adjustment] %>% 
        apply(2, function(x) hayashi_yoshida(returns[,lag_adjustment],x))
      
      adjustment <- c(hayashi_results[1:(lag_adjustment - 1)],
                      0,
                      hayashi_results[(lag_adjustment:length(hayashi_results))])
      
      # add adjustment onto covariance matrix
      c %<>% addCross(adjustment,3)
    }
    c
  }
}

# calculate correlation from adjusted covariance matrix (not perfect)
adjusted_cor <- function(adjusted_cov, returns) {
  standard_devs <- returns %>% apply(2,sd)
  adjusted_cov / outer(standard_devs,standard_devs)
}

# Compare correlation with and without Hayashi-Yoshida adjustment
cor(returns) %>% corrplot
returns %>% (cov_returns(lag_adjustment = 3)) %>% adjusted_cor(returns) %>% corrplot


## ---- Markowitz ------------------------

mean_variance_ibase <- function(mu, information_matrix) {
  (information_matrix %*% mu)/as.numeric(rep(1,length(mu)) %*% information_matrix %*% mu)
}

mean_variance_base <- function(mu, sigma) {
  mean_variance_ibase(mu, sigma %>% solve)
}

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


min_variance <- function(mean = mean_returns(), cov = cov_returns()) {
  function(returns) {
    mu <- returns %>% mean
    c <- returns %>% cov
    mean_variance_optimal(mu, c %>% solve, Inf) %>% t
  }
}

max_sharpe <- function(mean = mean_returns(), cov = cov_returns()) {
  function(returns) {
    mu <- returns %>% mean
    c <- returns %>% cov
    mean_variance_base(mu, c %>% solve) %>% t
  }
}


## ---- base-wrapper --------------------------

# select an expanding window of returns, starting end of 2012 and feed it into the model
months_calibration <- index(returns["2012/"])[endpoints(returns["2012/"],"months")]

evaluate_model <- function(model, lookback = "1 year", subset = "2012/", period = "months") {
  vars <- index(returns[subset])[endpoints(returns[subset], period)] %>% 
    lapply(function(date) returns[paste0("/", date)]) %>%
    lapply(function(returns) {
      date <- index(returns) %>% last %>% as.Date
      returns <- returns %>% xts::last(lookback)
      val <- model(returns)
      xts(val, date)
    }) %>%
    Reduce(rbind,.)
  vars
}

min_variance() %>% evaluate_model %>% plotXTS

# Max Sharpe with shrunken means
max_sharpe(mean = mean_returns(shrink = 1)) %>% evaluate_model %>% plotXTS

# Min Variance with hayashi yoshida adjustment
w <- min_variance(cov = cov_returns(lag_adjustment = 3)) %>% evaluate_model
w %>% plotXTS + ylim(c(-1,1.5))


## ---- performance-calculation --------------------------

portfolio_return <- function(weights, subset = "2012/", period = "months") {
  dates <- index(returns[subset])[endpoints(returns[subset], period)] 
  # returns for decision months
  periodical_returns <- lag(assets[dates],-1) / assets[dates] - 1
  
  portfolio_returns <- rowSums(periodical_returns * weights, na.rm = T) %>%
    xts(index(weights)) %>%
    (function(x) cumprod(1 + x))
  
  vals <- apply(assets[subset], 1, function(x) x/as.numeric(assets[subset][1,])) %>% t
  my_asset_returns <- xts(vals, index(assets[subset]))
  
  merge.xts(portfolio_returns, my_asset_returns) %>% na.omit %>% plotXTS
  
  weight_development <- function(initial_weights, asset_evolution, amt = 1) {
    units <- amt / as.numeric(asset_evolution[1,]) * as.numeric(initial_weights)
    vals <- apply(asset_evolution, 1, function(x) x * units) %>% t
    xts(coredata(vals),index(asset_evolution))
  }
  
  relative_weights <- function(x) {
    x/rowSums(x)
  }
  
  # first month
  # weight_development(weights[1,], assets['2012-01-31/2012-02-29'], 100) %>% rowSums
  # weight_development(weights[2,], assets['2012-02-29/2012-03-30'], 101.42630) %>% rowSums
  
  periods <- paste0(dates[1:(length(index(dates)) - 1)], "/", dates[2:length(index(dates))])
  period_assets <- periods %>% sapply(function(x)assets[x])
  
  p <- vector(mode = "list", length = dim(weights)[1])
  for (i in 1:dim(weights)[1]) {
    p[[i]] <- list(weights = weights[i], assets = period_assets[[i]])
  }
  
  pf <- Reduce(function(carry, period_slice) {
    if (is.null(carry)) {
      value <- 100
    } else {
      value <- rowSums(carry) %>% last
    }
    assets <- period_slice$assets
    weights <- period_slice$weights
    res <- weight_development(weights, assets, value)
    if (!is.null(carry)) {
      res <- rbind(carry,res)
    }
    res
  }, p, NULL)
  pf
}

drop_last <- function(x) {
  x[-dim(x)[1],]
}

rowSums.xts <- function(x) {
  xts(rowSums(x), index(x))
}

## ---- Pipelines ------------------

min_variance() %>%
  evaluate_model %>%
  drop_last %>%
  return_with_weights %>%
  rowSums.xts %>% 
  plotXTS


## ---- turnover -----------------------
## INCORRECT - consider change over infinitesimal time horizon over rebalancing

turnover <- function(weights) {
  to <- year(weights %>% index) %>% unique %>% lapply(function(year){
    t <- weights %>% diff %>% .[paste(year)] %>% abs %>% na.omit %>% sum(.)/2
    xts(t, as.Date(paste0(year,"-01-01")))
  }
  ) %>% Reduce(rbind,.)
  colnames(to) <- c("weights")
  to
}

turnover(weights)


## ---- random-weights -----------------------

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
res2 <- models %>% lapply(function(x)x %>% summary %>% coef) #%>% .[,4]
library(reshape2)
library(ggplot2)
library(corrplot)
library(tawny)
library(PortfolioAnalytics)
library(covRobust)
#library(FRAPO)

source("0-helper.R")
source("1-data.R")
source("mdd.R")
source("3-analysis2.R")

my_subset = "2012-12-30/2015-06-30"

## ---- Mean ----------
mean_returns <- function(shrink = 0, annualize = 252) {
  function(returns) {
    r <- returns %>% apply(2, mean) * annualize
    r * (1 - shrink) + Reduce(mean, r) * shrink
  }
}


## ---- Covariance -----------
hayashi_yoshida <- function(ts_fixing_pre, ts_fixing_post) {
  stopifnot(!is.null(ts_fixing_pre))
  ts_fixing_post %<>% as.xts
  ts_fixing_pre %<>% as.xts
  index(ts_fixing_post) <- index(ts_fixing_pre)
  
  aligned <- merge.xts(ts_fixing_pre, ts_fixing_post) %>% na.omit
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
    cov_func <- if (shrink) cov.shrink else cov
    c <- cov_func(returns)
    
    if (lag_adjustment != F) {
      hayashi_results <- returns[,-lag_adjustment] %>% 
        apply(2, function(x) hayashi_yoshida(returns[,lag_adjustment],x))
      
      adjustment <- append(hayashi_results, 0, after = lag_adjustment)
      # add adjustment onto covariance matrix
      c %<>% addCross(adjustment, lag_adjustment)
    }
    c * annualize
  }
}

# calculate correlation from adjusted covariance matrix (not perfect)
adjusted_cor <- function(adjusted_cov, returns) {
  standard_devs <- returns %>% apply(2,sd)
  adjusted_cov / outer(standard_devs,standard_devs)
}

## Compare correlation with and without Hayashi-Yoshida adjustment
#cor(returns) %>% corrplot
#returns %>% (cov_returns(lag_adjustment = 3)) %>% adjusted_cor(returns) %>% corrplot


## ---- Models ------------------------
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
    mean_variance_base(mu, c) %>% t
  }
}

BL_P <- t(matrix(c(1,0,0, 0,1,0, 0,0,1), nrow = 3, ncol = 3))
BL_v <- matrix(c(0.042,0.035,0.02))
max_sharpe_blacklitterman <- function(P = BL_P, v = BL_v) {
  function(returns) {
    mu <- black.litterman(returns["/2012"], P, Mu = NULL, Sigma = NULL, Views = v)$BLMu
    c <- black.litterman(returns["/2012"], P, Mu = NULL, Sigma = NULL, Views = v)$BLSigma
    mean_variance_base(mu, c)
  }
}

BL_P_vxx <- t(matrix(c(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1), nrow = 4, ncol = 4))
BL_v_vxx <- matrix(c(0.042,0.035,0.02, -0.9))
max_sharpe_blacklitterman_vxx <- function(P = BL_P_vxx, v = BL_v_vxx) {
  function(returns) {
    mu <- black.litterman(returns["/2012"], P, Mu = NULL, Sigma = NULL, Views = v)$BLMu
    c <- black.litterman(returns["/2012"], P, Mu = NULL, Sigma = NULL, Views = v)$BLSigma
    mean_variance_base(mu, c)
  }
}

max_sharpe_robust <- function() {
  function(returns) {
    moments <- cov.nnve(returns, k = 12, pnoise = 0.05, emconv = 0.001, bound = 1.5, extension = TRUE, devsm = 0.01)
    mu <- moments$mu
    c <- moments$cov
    mean_variance_base(mu, c) %>% t
  }
}

min_variance_robust <- function() {
  function(returns) {
    moments <- cov.nnve(returns, k = 12, pnoise = 0.05, emconv = 0.001, bound = 1.5, extension = TRUE, devsm = 0.01)
    mu <- moments$mu
    c <- moments$cov
    mean_variance_optimal(mu, c %>% solve, Inf) %>% t
  }
}


fixed_weights <- function(weights) {
  function(returns) {
    w <- t(weights)
    colnames(w) <- colnames(returns)
    w
  }
}

equal_risk_contribution <- function(cov = cov_returns()) {
  function(returns){
    c <- returns %>% cov
    ERC <- PERC(c)
    w <- Weights(ERC) / 100
    t(w)
  }
}


## ---- base-wrapper --------------------------
# select an expanding window of returns, starting end of 2012 and feed it into the model
months_calibration <- index(returns["2012/"])[endpoints(returns["2012/"], "months")]

evaluate_model <- function(model, ass = assets, lookback = "3 years", subset = my_subset, period = "months") {
  returns <- ass %>% ROC(type = "discrete") %>% na.omit
  vars <- index(returns[subset])[endpoints(returns[subset], period)] %>% 
    lapply(function(date) returns[paste0("/", date)]) %>%
    lapply(function(returns) {
      date <- index(returns) %>% last %>% as.Date
      returns <- returns %>% xts::last(lookback)
      val <- model(returns)
      xts::xts(val, date)
    }) %>%
    Reduce(rbind,.)
  vars
}

## ---- performance-calculation --------------------------

portfolio_return <- function(weights, ass = assets, subset = my_subset, period = "months", rf_allocation = NULL) {
  returns <- ass %>% ROC(type = "discrete") %>% na.omit
  
  dates <- index(returns[subset])[endpoints(returns[subset], period)] 
  # returns for decision months
  periodical_returns <- lag(ass[dates], -1) / ass[dates] - 1
  
  w <- weights[index(periodical_returns)]
  
  portfolio_returns <- rowSums(periodical_returns * w, na.rm = T) %>%
    xts(index(w)) %>%
    (function(x) cumprod(1 + x))

  vals <- apply(ass[subset], 1, function(x) x / as.numeric(ass[1,])) %>% t

  my_asset_returns <- xts(vals, index(ass[subset]))
  
  weight_development <- function(initial_weights, asset_evolution, amt = 1) {
    units <- amt / as.numeric(asset_evolution[1,]) * as.numeric(initial_weights)
    vals <- apply(asset_evolution, 1, function(x) x * units) %>% t
    xts(coredata(vals), index(asset_evolution))
  }
  
  relative_weights <- function(x) {
    x / rowSums(x)
  }

  periods <- paste0(dates[1:(length(index(dates)) - 1)], "/", dates[2:length(index(dates))])
  period_assets <- periods %>% sapply(function(x)ass[x] %>% drop_first)

  p <- vector(mode = "list", length = NROW(periods))
  for (i in 1:NROW(periods)) {
    per <- periods[i]
    p[[i]] <- list(weights = weights[per][1], assets = drop_first(ass[per]))
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
      res <- rbind(carry, res)
    }
    res
  }, p, NULL)
  if (!is.null(rf_allocation)) {
    
    weight <- rf_allocation[["weight"]]
    my_euribor <- rf_allocation[["rate"]][index(pf)]
    my_portfolio <- pf[index(my_euribor)]
    my_euribor <- cumprod(1 + my_euribor)*100
    
    mixed <- merge.xts(my_portfolio * (1 - weight), mixed = my_euribor * weight)
    mixed
  } else {
    pf 
  }
}

evaluate_turnover <- function(weights, period = "months", subset = my_subset, ass = assets) {
  stopifnot(any(period == "months", period == "years"))
  periods_per_year <- ifelse(period == "months", 12, 1)
  performance <- portfolio_return(weights, ass = ass, period = period, subset = subset)
  index(weights)[-1] %>% lapply(function(date){
    idx <- which(index(performance) == date)
    differences <- abs(performance[idx] - as.numeric(performance[idx + 1]))
    differences %>% sum
  }) %>% unlist %>% (function(weight_changes){
    sum(weight_changes) / (length(weight_changes) / periods_per_year)
  }) %>% (function(turnover) turnover / 2)
}

rowSums.xts <- function(x) {
  xts(rowSums(x), index(x))
}

zero_killer <- function(x) {
  pf_idx <- which(x < 0) %>% first
  if (!is.na(pf_idx)) {
    x[pf_idx:dim(x)[1]] <- 0 
  }
  x
}


## ---- Pipelines ------------------
pipeline <- function(model, ass = assets, rf_allocation = NULL, subset = my_subset) {
  model %>%
    evaluate_model(ass = ass) %>%
    drop_last %>%
    portfolio_return(ass = ass, rf_allocation = rf_allocation, subset = subset) %>%
    rowSums.xts %>%
    zero_killer
}

performance_plot <- function(model, ass = assets, rf_allocation = NULL, subset = my_subset) {
   model %>% pipeline(ass, rf_allocation = rf_allocation, subset = subset) %>% plotXTS(size = 1)
}

pgfplot <- function(model, name, ass = assets, rf_allocation = NULL, subset = my_subset) {
  model %>% pipeline(ass, rf_allocation = rf_allocation, subset = subset) %>% plotTable(name)
}

base_100 <- function(values) {
  values %>%
    apply(2,function(x) x / as.numeric(rowSums.xts(.))) %>% as.xts
}

decompose <- function(model, ass = assets, rf_allocation = NULL) {
  model %>% evaluate_model(ass = assets) %>% drop_last %>% portfolio_return(rf_allocation = rf_allocation)
}

compute_kpis <- function(model, ass = assets, rf_allocation = NULL, subset = my_subset) {
  weights <- model %>% evaluate_model(ass = ass) %>% drop_last
  value <- weights %>%
    portfolio_return(ass = ass, rf_allocation = rf_allocation, subset = subset) %>%
    rowSums.xts %>%
    zero_killer 
  returns <- value %>% ROC(type = "discrete") %>% na.omit
  
  w <- rf_allocation[["weight"]]
  
  max_dd <- tryCatch(getMDD(value), error = function(e){print(e);return(NA)})
  excess_mu <- mean(returns) * 252
  standard_dev <- sd(returns) * sqrt(252)
  sharpe <- excess_mu / standard_dev
  
  turnover <- evaluate_turnover(weights, ass = ass) * ifelse(is.null(rf_allocation),1,(1 - w))
  
  factor_merge <- merge.xts(fact[,c("DAX", "Dow.Jones", "Nikkei")], weights[,c("DAX", "Dow.Jones", "Nikkei")]) %>% na.omit
  f_returns <- factor_merge[,c("DAX", "Dow.Jones", "Nikkei")] * factor_merge[,c("DAX", "Dow.Jones", "Nikkei")]

  alpha <- sum(f_returns) / (NROW(f_returns) / 12)
  
  data.frame(sharpe = sharpe,
                 mu = excess_mu * 100,
              sigma = standard_dev * 100,
        maxDrawDown = max_dd,
           turnover = turnover,
              alpha = alpha * 100)
}

compute_kpis_fix <- function(value) {
  returns <- value %>% ROC(type = "discrete") %>% na.omit
  
  max_dd <- tryCatch(getMDD(value), error = function(e){print(e);return(NA)})
  excess_mu <- mean(returns) * 252
  standard_dev <- sd(returns) * sqrt(252)
  sharpe <- excess_mu / standard_dev
  data.frame(sharpe        = sharpe,
             mu            = excess_mu,
             sigma         = standard_dev,
             max_draw_down = max_dd,
             turnover      = 0,
             alpha = NA)
}

evaluate_fix_components <- function(weights, ass = assets, subset = my_subset) {
  a <- ass[subset]
  a %<>% apply(2, function(x) x * 100 / (coredata(x[1]))) %>% as.xts
  
  t(t(a) * drop(weights)) %>% as.xts
}

evaluate_fix <- function(weights, ass = assets, subset = "2013/2015-06-30") {
  a <- ass[subset]
  a %<>% apply(2, function(x) x * 100 / (coredata(x[1]))) %>% as.xts
  
  xts(a %*% weights, index(a))
}

## ---- linear-regression --------------------
timepoints <- index(assets[endpoints(assets["2009-02-01/2015-09-30"],"months")])
eu_factors <- eu_factors["2009-02-01/"]
index(eu_factors) <- timepoints
us_factors <- us_factors["2009-02-01/"]
index(us_factors) <- timepoints
jp_factors <- jp_factors["2009-02-01/"]
index(jp_factors) <- timepoints

returns_monthly <- assets[timepoints] %>% ROC(type = "discrete") %>% na.omit

us_d <- merge.xts(returns_monthly["/2012-01-01"][,"Dow Jones"] %>% lag(-1), us_factors) %>% na.omit
eu_d <- merge.xts(returns_monthly["/2012-01-01"][,"DAX"] %>% lag(-1), eu_factors) %>% na.omit
jp_d <- merge.xts(returns_monthly["/2012-01-01"][,"Nikkei"] %>% lag(-1), jp_factors) %>% na.omit

models <- list(EU = list("DAX", eu_d),
               US = list("Dow.Jones", us_d),
               JP = list("Nikkei", jp_d)) %>%
            lapply(function(li) {
              f <- paste(li[[1]], "~ Mkt.RF + SMB + HML + WML + RF") %>% as.formula
              lm(f, data = li[[2]])
            })
model_res <- models %>% lapply(function(x)x %>% summary %>% coef)
model_res

eu_factor_return <- predict(models$EU, eu_factors) %>% as.xts
us_factor_return <- predict(models$US, us_factors) %>% as.xts
jp_factor_return <- predict(models$JP, jp_factors) %>% as.xts
factor_returns <- merge.xts(eu_factor_return, us_factor_return, jp_factor_return) %>% lag(1) %>% na.omit %>% as.xts
index(factor_returns) <- returns_monthly %>% index

fact <- returns_monthly[,1:3] - factor_returns
fact <- fact / (NROW(fact) / 12)
factor_alpha <- apply(fact, 2, sum)

rm(list = c("eu_factor_return", "us_factor_return", "jp_factor_return", "factor_returns", "us_d", "eu_d", "jp_d"))
source("2-analysis.R")

portfolio_party <- function(model, name) {
  model %>% performance_plot
  model %>% compute_kpis
  model %>% decompose_plot(name)
  model %>% decompose_relw_plot(paste(name,"_relw"))
}

## ---- Minimum Variance ---------------
min_variance(cov = cov_returns(shrink = T)) %>% performance_plot


## ---- Maximum Sharpe ---------------
ms <-  max_sharpe(mean = mean_returns(shrink = 0.65),
           cov = cov_returns(shrink = T, lag_adjustment = 3))
ms %>% performance_plot
ms %>% compute_kpis

## ---- Fixed Weights ---------------
fw <- fixed_weights(c(1/3, 1/3, 1/3))
portfolio_party(fw, "equal")

## ---- Black Litterman ---------------
w <- returns %>% (max_sharpe_blacklitterman()) %>% t
bl <- fixed_weights(w)
portfolio_party(bl, "black_littie")

evaluate_fix(w) %>% plotXTS(size = 1)

## ---- Other Optimization ---------------
eff_portfolio(mean = mean_returns(shrink = 0.5),
              cov = cov_returns(shrink = 0.5), T, 3, 0.01, 0.4) %>% performance_plot

## ---- Equal Risk Contribution ---------------
equal_risk_contribution(cov = cov_returns(shrink = 0.5, lag_adjustment = 3)) %>% performance_plot

## ---- Fixed allocation, No rebalance ---------------
evaluate_fix(c(0.5,0.5,0.5,-0.5)) %>% plotXTS(size = 1)
evaluate_fix(c(0.5,0.5,0.5,-0.5)) %>% compute_kpis

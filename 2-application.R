source("2-analysis.R")


## ---- Minimum Variance ---------------
min_variance(cov = cov_returns(shrink = T)) %>% performance_plot


## ---- Maximum Sharpe ---------------
max_sharpe() %>% performance_plot #pgfplot("max_sharpe")
#max_sharpe() %>% pgfplot("max_sharpe")

max_sharpe(mean = mean_returns(shrink = 0.9),
           cov = cov_returns(shrink = T, lag_adjustment = 3)) %>% performance_plot
max_sharpe(mean = mean_returns(shrink = 0.9),
           cov = cov_returns(shrink = T, lag_adjustment = 3)) %>% compute_kpis

## ---- Fixed Weights ---------------
fixed_weights(c(1/3, 1/3, 1/3, 0)) %>% performance_plot
fixed_weights(c(1/3, 1/3, 1/3, 0)) %>% decompose_plot("equal")
fixed_weights(c(1/3, 1/3, 1/3, 0)) %>% decompose_relw_plot("equal_relw")
fixed_weights(c(1/3, 1/3, 1/3, 0)) %>% compute_kpis
#decompose_plot("fixed_decomposed")
#fixed_weights(c(2/3, 2/3, 2/3, -1)) %>% pgfplot("equal")
#fixed_weights(c(2/3, 2/3, 2/3, -1)) %>% compute_kpis


## ---- Black Litterman ---------------
w <- returns %>% (max_sharpe_blacklitterman()) %>% t
fixed_weights(w) %>% performance_plot



evaluate_fix(w) %>% plotXTS(size = 1)

## ---- Other Optimization ---------------
eff_portfolio(mean = mean_returns(shrink = 0.5),
              cov = cov_returns(shrink = 0.5), T, 3, 0.01, 0.4) %>% performance_plot

## ---- Equal Risk Contribution ---------------
equal_risk_contribution(cov = cov_returns(shrink = 0.5, lag_adjustment = 3)) %>% performance_plot

## ---- Fixed allocation, No rebalance ---------------
evaluate_fix(c(0.5,0.5,0.5,-0.5)) %>% plotXTS(size = 1)
<<<<<<< HEAD

=======
evaluate_fix(c(0.5,0.5,0.5,-0.5)) %>% compute_kpis

########

fixed_weights(c(1/4,1/4,1/4,1/4)) %>% compute_kpis
>>>>>>> 0eb2ca648ce412cb0786813869a94281a277bd74

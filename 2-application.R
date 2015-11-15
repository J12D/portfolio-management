source("2-analysis.R")

PARTY <- T
my_time <- "2012-12-30/2015-11-01"

run_job <- function(job) {
  model <- job[["model"]]
  assets <- job[["assets"]]
  period <- job[["period"]]
  rf_allocation <- job[["rf_allocation"]]
  subset <- job[["time"]]
  stopifnot(period %in% c("months", "years", "fix"))
  
  if (period == "fix") {
    weights <- model(assets[subset] %>% ROC(type = "discrete"))
    values <- weights %>% evaluate_fix_components(ass = assets, subset = subset)
  }
  else {
    weights <- model %>% evaluate_model(ass = assets, period = period, subset = subset)
    values <- weights %>% drop_last %>% portfolio_return(ass = assets, period = period, subset = subset, rf_allocation = rf_allocation)
  }
  
  value <- values %>% rowSums.xts
  kpis <- compute_kpis(value, values, weights, job)
  
  list(weights = weights, values = values, value = value, kpis = kpis, job = job)
}

rebase_to_date <- function(job_result, date) {
  job_result$value[paste0(date,"/")]/as.numeric(job_result$value[date]) * 100
}

with_model <- function(job, model) {
  job[["model"]] <- model
  job
}

with_allocation <- function(job, euribor_allocation) {
  job[["rf_allocation"]] <- list(weight = euribor_allocation, rate = euribor)
  job
}

on_assets <- function(job, assets) {
  job[["assets"]] <- assets
  job
}

plot_weights <- function(job_result) {
  job_result[["weights"]] %>% plotXTS
}

plot_values <- function(job_result) {
  job_result[["values"]] %>% plotXTS
}

plot_value <- function(job_result) {
  job_result[["value"]] %>% plotXTS
}

base <- list(model = NULL, rf_allocation = NULL, assets = assets, period = "months", time = my_time)
#base <- list(model = NULL, rf_allocation = NULL, assets = assets, period = "months", time = "2012-12-30/2015-11-01")

annual_base <- as.list(base)
annual_base["period"] <- "years"

fix_base <- as.list(base)
fix_base["period"] <- "fix"

portfolio_party <- function(job_result, name) {
  if (PARTY) {
    job_result[["values"]] %>% plotTable(name)
    job_result[["values"]] %>% base_100 %>% plotTable(paste0(name,"_relw"))
    job_result[["value"]] %>% plotTable(paste0(name,"_p")) 
  }
}


## ---- 1-Minimum Variance ---------------
minv <- min_variance(cov = cov_returns(shrink = T, lag_adjustment = 3))
minv_res <- annual_base %>% with_model(minv) %>% with_allocation(0.45) %>% run_job
minv_res %>% portfolio_party("minv")

## ---- 2-Maximum Sharpe ---------------
ms <-  max_sharpe(mean = mean_returns(shrink = 0.5),
           cov = cov_returns(lag_adjustment = 3, shrink = T))
ms_res <- annual_base %>% with_model(ms) %>% run_job
ms_res %>% portfolio_party("ms")

## ---- 3-Fixed Weights ---------------
fw <- fixed_weights(rep(1/3, 3))
fw_res <- fix_base %>% with_model(fw) %>% run_job
fw_res %>% portfolio_party("fw")

## ---- 4-Fixed Weights Leveraged ------
fw_lev <- fixed_weights(c(0.36, 0.36, 0.36, -0.08))
fw_lev_res <- fix_base %>% with_model(fw_lev) %>% on_assets(assets_vxx) %>% run_job
fw_lev_res %>% portfolio_party("fw_lev")

## ---- 5-Black Litterman ---------------
w <- max_sharpe_blacklitterman()(returns)
bl_w <- fixed_weights(c(w))
bl_w_res <- fix_base %>% with_model(bl_w) %>% run_job
bl_w_res %>% portfolio_party("bl")

## ---- 6-Equal Risk Contribution ---------------
library(FRAPO)

erc <- equal_risk_contribution(cov = cov_returns(shrink = T, lag_adjustment = 3))
erc_res <- base %>% with_model(erc) %>% with_allocation(0.3) %>% run_job
erc_res %>% portfolio_party("erc")
detach("package:FRAPO", unload = TRUE)

## ---- 7-Robust MinVP ---------------
minv_rob <- min_variance_robust()
minv_rob_res <- annual_base %>% with_model(minv_rob) %>% run_job
minv_rob_res %>% portfolio_party("minv_rob")

## ---- 8-Robust MVP ---------------
msr_rob <- max_sharpe_robust()
msr_rob_res <- annual_base %>% with_model(msr_rob) %>% run_job
msr_rob_res %>% portfolio_party("msr_rob")

## ---- 9-MVP Max Allocation ---------------
mv_max_alloc <- eff_portfolio(mean = mean_returns(shrink = 0.5),
                     cov = cov_returns(shrink = T), no_shorts = F, max.allocation = 0.5)

mv_max_alloc_res <- annual_base %>% with_model(mv_max_alloc) %>% on_assets(assets_vxx) %>% with_allocation(0.75) %>% run_job
mv_max_alloc_res %>% portfolio_party("mva")

## ---- 10-MVP Max Allocation No Short ---------------
mv_max_alloc_no_short <- eff_portfolio(mean = mean_returns(shrink = 0.5),
                     cov = cov_returns(shrink = T), no_shorts = T, max.allocation = 0.5)

mv_max_alloc_no_short_res <- annual_base %>% with_model(mv_max_alloc_no_short) %>% on_assets(assets_vxx) %>% with_allocation(0.35) %>% run_job
mv_max_alloc_no_short_res %>% portfolio_party("mvo")


## ---- Produce Tables -----------
library(xtable)

models <- list("Minimum Variance" = minv_res,
               "Maximum Sharpe" = ms_res,
               "Fixed Weights" = fw_res,
               "Fixed Weights Leveraged" = fw_lev_res,
               "Black Litterman" = bl_w_res,
               "Equal Risk" = erc_res,
               "Min Variance Robust" = minv_rob_res,
               "Maximum Sharpe Robust" = msr_rob_res,
               "MV Max Allocation" = mv_max_alloc_res,
               "MV Max Allocation & Short-Constraint" = mv_max_alloc_no_short_res)

comp <- models %>% lapply(function(x)x$kpis) %>% do.call(rbind, .)
xtable(comp)

###
comp_res <- models %>% lapply(function(x)x$value) %>% do.call(merge.xts, .)
c <- merge.xts(comp_res, msci) %>%
  .["2015-06-30/"] %>% na.omit %>%
  apply(2,function(x)x/coredata(x[1])*100) %>% as.xts %>% 
  plotTable("comparison_bonus")
###

models %>%
  lapply(function(x) x$value %>% ROC(type = "discrete") %>% information_ratio(ROC(msci, type = "discrete"))) %>%
  do.call(rbind, .)

### bonus information ratio
merged_returns <- merge.xts(comp_res, msci) %>% na.omit %>% ROC(type = "discrete") %>% na.omit %>% as.xts
merged_returns["2015-06-30/2015-10-30"] %>% lapply(function(x) {
    information_ratio(x, merged_returns[,NCOL(merged_returns)]) 
  }) %>% do.call(rbind, .)


## ---- bonus -----

b_res <- models %>%
  lapply(function(x)rebase_to_date(x, "2015-06-30")) %>%
  lapply(function(x)compute_kpis(x,NULL, NULL, fix_base)) %>%
  do.call(rbind, .)

xtable(b_res[,1:3])
#http://economistatlarge.com/portfolio-theory/r-optimized-portfolio
library(quadprog)
eff_portfolio <- function(mean = mean_returns(), cov = cov_returns(),
                         no_shorts = F, risk.premium.up = 3, risk.increment = 0.001,
                         max.allocation = NULL) {
  function(returns) {
    mu <- returns %>% mean
    c <- returns %>% cov

    n <- ncol(c)
    
    # Create Amat & bvec assuming only equality constraint, short-selling allowed, no allocation constraints
    Amat <- matrix(1, nrow = n)
    bvec <- 1
    meq <- 1
    
    # Modify Amat & bvec if short-selling not allowed
    if (no_shorts) {
      Amat <- cbind(1, diag(n))
      bvec <- c(bvec, rep(0, n))
    }
    
    # Modify Amat & bvec for max allocation constraint
    if (!is.null(max.allocation)) {
      if (max.allocation > 1 | max.allocation < 0) {
        stop("max.allocation must be greater than 0 and less than 1")
      }
      if (max.allocation * n < 1) {
        stop("Need to set max.allocation higher; not enough assets to add to 1")
      }
      Amat <- cbind(Amat, -diag(n))
      bvec <- c(bvec, rep(-max.allocation, n))
    }
    
    # Calculate #loops
    loops <- risk.premium.up / risk.increment + 1
    loop <- 1
    
    # Initialize a matrix to contain allocation and statistics
    eff <- matrix(nrow = loops, ncol = n+3)
    # Now I need to give the matrix column names
    colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
    
    # Loop through the quadratic program solver
    for (i in seq(from = 0, to = risk.premium.up, by = risk.increment)) {
      dvec <- mu * i # This moves the solution along the EF
      sol <- solve.QP(c, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)
      eff[loop, "Std.Dev"] <- sqrt(sum(sol$solution * colSums((c * sol$solution))))
      eff[loop, "Exp.Return"] <- as.numeric(sol$solution %*% mu)
      eff[loop, "sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
      eff[loop, 1:n] <- sol$solution
      loop <- loop + 1
    }
    eff <- as.data.frame(eff)
    eff <- eff[eff$sharpe == max(eff$sharpe), 1:n]
    rownames(eff) <- NULL
    eff[1,]
  }
}
require(quadprog)
efficient_frontier <- function(mu, sigma, ) {
  
}


eff_portfolio <- function(mu, sigma, mu_target, shorts_allowed=TRUE) {
    stopifnot(length(mu) == NROW(sigma))
    stopifnot(all((sigma %>% eigen)$values >= 0)) # positive definite matrix
    stopifnot(shorts_allowed %>% is.logical)
    
    n <- length(mu)
    
    if (shorts_allowed) {
      top <- cbind(2 * sigma, mu, ones(n))
      bot <- cbind(rbind(mu, ones(n)), matrix(0, 2, 2))
      A <- rbind(top, bot)
      b.target <- as.matrix(c(zeros(n), mu_target, 1))
      x <- solve(A, b.target)
      weights <- x[1:n]
    } else {
      Dmat <- 2 * sigma
      dvec <- zeros(n)
      Amat <- cbind(ones(n), mu, diag(1, n))
      bvec <- c(1, mu_target, zeros(n))
      result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 2)
      weights <- round(result$solution, 6)
    }
    
    names(weights) <- colnames(returns)
    mu_pf <- crossprod(mu, weights)
    sd_pf <- sqrt(weights %*% sigma %*% weights)
    res <- list("call" = match.call(),
                "mu" = as.vector(mu_pf),
                "sd" = as.vector(sd_pf),
                "weights" = weights)
    class(res) <- "pf_results"
    res
}
efficient_frontier <- function(r) {
  x <- sapply(1:10000,function(x)runif(3,-1,1)) %>% apply(2,function(x)x/sum(x))
  
  mu_a <- r %>% apply(2,mean) * 252
  #mu_a
  
  # Standard Deviation
  sigma_a <- r %>% cov * sqrt(252)
  #sigma_a
  
  mu_p <- t(x) %*% mu_a
  sigma_p <- sqrt(diag(t(x) %*% sigma_a %*% x))
  
  qplot(sigma_p,mu_p,xlim = c(0,0.8), ylim = c(0,0.5), xlab = "Volatility (in %)", ylab = "Expected return (in %)")
  
}

a <- returns[,1:3]

apply(assets[,1:3],1,function(x)x/as.numeric(assets[1,1:3])) %>% t %>% plotXTS
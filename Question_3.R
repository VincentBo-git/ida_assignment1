library(maxLik)
load("dataex3.RData")

# Set the initial value of sigma
sigma <- 1.5

# Create the log-likelihood function
loglikelihood <- function(mu,x,r, sigma2) {
  
  pdf <- dnorm(x, mean = mu, sd = sigma)
  cdf <- pnorm(x, mean = mu, sd = sigma)
  loglik <- sum(r * log(pdf) + (1 - r) * log(cdf))
  return(-loglik)
}

# Get the result of MLE
mle_est <- optim(par = 0, fn = loglikelihood, x=dataex3$X, r=dataex3$R, sigma2 = sigma2)
mu_hat <- mle_est$par
print(mu_hat)
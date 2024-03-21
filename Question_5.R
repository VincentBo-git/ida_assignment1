library(maxLik)
load("dataex5.RData")

# Create pi function
pi <- function(b, x) {
  exp(b[1] + b[2] * x) / (1 + exp(b[1] + b[2] * x))
}

em_algorithm <- function(data, eps){
  
  # Set the initial value
  beta <- c(beta0 = 0, beta1 = 0)
  diff <- eps+1
  
  # Separate missing data and observed data
  missing_i <- which(is.na(dataex5$Y))
  data_obs <- dataex5[-missing_i, ]
  data_mis <- dataex5[missing_i, ]
  xobs <- data_obs[,1]
  yobs <- data_obs[,2]
  xmis <- data_mis[,1]
  
  while (diff > eps)  {
    
    # Store previous beta
    beta_pre <- beta
    
    # E-Step
    Q_function <- function(beta, beta_pre){
      
      piobs <- pi(beta,xobs)
      pimis <- pi(beta,xmis)
      pimis_pre <- pi(beta_pre,xmis)
      q <- sum(yobs * log(piobs) + (1 - yobs) * log(1 -  piobs))+
        sum(pimis_pre * log(pimis) + (1 - pimis_pre) * log(1 - pimis))
      
      return(-q)
    }
    
    # M-Step: Update parameters
    opt_res <- optim(par = beta, Q_function,  beta_pre = beta_pre)
    beta <- opt_res$par
    
    # Check for convergence
    diff <- sum(abs(beta - beta_pre))
  }
  return(beta)
}

# Set initial value of eps
eps <- 1e-5

em_algorithm(dataex5, eps)

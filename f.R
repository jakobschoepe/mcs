f <- function(i) {
  # Store the random number generator state
  seed <- .Random.seed
  
  # Predefine a normal copula
  tmp1 <- copula::normalCopula(param = c(.1, .1, .1), dim = 3L, dispstr = "un")
  tmp2 <- copula::mvdc(copula = tmp1, margins = c("norm", "binom", "binom"), paramMargins = list(list(mean = 0, sd = 1), list(size = 1, prob = .3), list(size = 1, prob = .5)))
  
  # Generate random variables from marginal distributions
  simdata <- data.table::as.data.table(copula::rMvdc(n = n, mvdc = tmp2))
  
  # Predefine true coefficients
  b0 <- -2.3
  b1 <- -0.3
  b2 <- -0.2
  
  # Predefine linear combination
  b <- b0 + b1 * simdata$V1 + b2 * simdata$V2
  
  # 
  pr <- 1 / (1 + exp(-b))
  
  # Generate random variables from a binomial distribution
  simdata$y <- rbinom(n = n, size = 1, prob = pr)
  
  # Fit the binomial logistic regression model
  
  
  
  return(x = fit)
}
  

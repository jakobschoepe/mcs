f <- function(i, param, dim, dispstr, margins, paramMargins, n, betas, link) {
  # Check passed arguments to smoothly run subsequent commands
  if (!is.vector(x = param)) {
    stop("\"param\" must be a real vector")
  }
  
  else if (length(x = param) != dim) {
    stop("\"param\" must be a real vector of length \"dim\"")
  }
  
  else if (!is.integer(x = dim)) {
    stop("\"dim\" must be a positive integer")
  }
  
  else if (dim != length(x = param)) {
    stop("\"dim\" is misspecified")
  }
           
  else if (length(x = dim) > 1) {
    stop("\"dim\" must be a positive integer")
  }
  
  else if (!is.character(x = dispstr)) {
    stop("\"dispstr\" must be a character value")
  }
           
  else if (!(dispstr %in% c("ex", "ar1", "toep", "un"))) {
    stop("\"dispstr\" is misspecified. Currently available structures are: \"ex\" for exchangeable, \"ar1\" for AR(1), \"toep\" for Toeplitz or \"un\" for unstructured")
  }
  
  else if(!is.vector(x = margins)) {
    stop("\"margins\" must be a character vector")
  }
    
  else if(!is.list(x = paramMargins)) {
    stop("\"paramMargins\" must be a list")
  }
  
  else if (!is.integer(x = n)) {
    stop("\"n\" must be a positive integer")
  }
    
  else if (length(x = n) > 1) {
    stop("\"n\" must be a positive integer")
  }
           
  else if (!is.numeric(x = betas)) {
    stop("\"betas\" must be a numeric vector")
  }
           
  else if (!is.character(link)) {
    stop("\"link\" must be a character value")
  }
           
  else if(link == "logit" | link == "log") {
    stop("\"link\" is misspecified. Currently available link functions are: \"logit\" and \"log\"")
  } 
  
  else {  
    # Store the random number generator state
    seed <- .Random.seed
  
    # Predefine a normal copula
    tmp1 <- copula::normalCopula(param = param, dim = dim, dispstr = dispstr)
    tmp2 <- copula::mvdc(copula = tmp1, margins = margins, paramMargins = paramMargins)
  
    # Generate random variables from marginal distributions
    simdata <- data.table::as.data.table(copula::rMvdc(n = n, mvdc = tmp2))
  
    # Predefine linear combination
    b <- model.matrix(~ ., data = simdata) %*% betas
  
    #  
    if (link == "logit") {
      pr <- 1 / (1 + exp(-b))
    }
  
    if (link == "log") {
      pr <- exp(b)
    }
           
    # Generate random variables from a binomial distribution
    simdata$y <- rbinom(n = n, size = 1, prob = pr) 
    }
  return(x = list(seed = seed, data = simdata))
}
  

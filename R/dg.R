#' @title Generating pseudo-random data for Monte Carlo simulations using a normal copula.
#' @description \code{dg} generates pseudo-random data for Monte Carlo simulations 
#' @param i 
#' @param param A numeric vector specifying the dispersion matrix (see \code{copula} package).
#' @param dim An integer specifying the dimension of the copula (see \code{copula} package).
#' @param dispstr A character string specifying the dispersion structure (see \code{copula} package): autoregressive of order 1 (ar1), exchangeable (ex), Toeplitz (toep), and unstructured (un).
#' @param margins A character vector specifying the marginal distributions (see \code{copula} package).
#' @param paramMargins An object of class \code{list} giving the parameter values of the marginal distributions (see \code{copula} package).
#' @param n An integer specifying the number of observations to be generated.
#' @param f A model formula.
#' @param betas A numeric vector specifying the true model parameters/estimands.
#' @param link A character string specifying the link function for model fitting ("logit" or "log").
#' @details \code{dg} was built as part of the design of a Monte Carlo simulation, and serves a special-purpose only.
#' @return A list containing the following elements:
#' \item{seed}{An integer vector containing the random number generator state.}
#' \item{data}{A data table containing the pseudo-random generated data.}
#' @references Yan J (2007) Enjoy the joy of copulas: With a package copula. J Stat Softw, 21:1-21

dg <- function(i, param, dim, dispstr, margins, paramMargins, n, f, betas, link) {
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
  
  else if (length(x = betas) != dim) {
    stop("\"betas\" must be equal to \"dim\"")
  }
           
  else if (!is.character(link)) {
    stop("\"link\" must be a character value")
  }
           
  else if (link != "logit" & link != "log") {
    stop("\"link\" is misspecified. Currently available link functions are: \"logit\" and \"log\"")
  } 
  
  else if (!exists(".Random.seed")) {
    stop("Please set a seed for the pseudo-random number generator")
  }
  
  else {  
    # Store the random number generator state
    seed <- .Random.seed
  
    # Predefine a normal copula
    tmp1 <- copula::normalCopula(param = param, dim = dim, dispstr = dispstr)
    tmp2 <- copula::mvdc(copula = tmp1, margins = margins, paramMargins = paramMargins)
  
    # Generate random variables from marginal distributions
    simdata <- data.table::as.data.table(copula::rMvdc(n = n, mvdc = tmp2))
  
    # Predefine linear combinations
    b <- model.matrix(f, data = simdata) %*% betas
  
    #  
    if (link == "logit") {
      pr <- 1 / (1 + exp(-b))
    }
  
    if (link == "log") {
      pr <- exp(b)
    }
           
    # Generate random variables from a binomial distribution
    simdata$y <- rbinom(n = n, size = 1, prob = pr) 
    return(x = list(seed = seed, data = simdata))
    }
}
  

#' @title Generating pseudo-random data for Monte Carlo simulations using a normal copula.
#' @description \code{dg} generates pseudo-random data for Monte Carlo simulations using a normal copula with user-defined marginal probability distributions.
#' @usage dg(i, param, dim, dispstr, margins, paramMargins, n, f, betas, link) 
#' @param i Index of the repetition (can be ignored).
#' @param param A numeric vector specifying the dispersion matrix (see \code{copula} package).
#' @param dim An integer specifying the dimension of the copula (see \code{copula} package).
#' @param dispstr A character string specifying the dispersion structure (see \code{copula} package): autoregressive of order 1 (ar1), exchangeable (ex), Toeplitz (toep), and unstructured (un).
#' @param margins A character vector specifying the marginal probability distributions (see \code{copula} package).
#' @param paramMargins An object of class \code{list} giving the parameter values of the marginal probability distributions (see \code{copula} package).
#' @param n An integer specifying the number of observations to be generated.
#' @param f A model formula.
#' @param thetas A numeric vector specifying the true model parameters.
#' @param link A character string specifying the link function for model fitting ("log" or "logit").
#' @return A list containing the following elements:
#' \item{seed}{An integer vector containing the state of the random number generator that was used to generate the data.}
#' \item{data}{A data table containing pseudo-random generated data.}
#' @references Yan J (2007) Enjoy the joy of copulas: With a package copula. J Stat Softw, 21:1-21
#' @note Please note that \code{dg} was built as part of the design of a Monte Carlo simulation, and therefore serves a special-purpose only.
#' @author Jakob Schöpe
#' @example
#' param <- c(0.0,
#'            0.3, 0.1,
#'            0.0, 0.2, 0.5)
#'
#' margins <- c("norm", "binom", "binom", "norm")
#' paramMargins <- list(list(mean = 0, sd = 1), list(size = 1, prob = 0.3), list(size = 1, prob = .1), list(mean = 20, sd = 4))
#' f <- ~ X1 + X2 + X3 + X4
#' thetas <- c(-3.10, 0.00, -0.45, 0.22, -0.16)
#'
#' myData <- dg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100L, f = f, thetas = thetas, link = "logit")

dg <- function(i, param, dim, dispstr, margins, paramMargins, n, transf, f, thetas, link) {
  # Check passed arguments to smoothly run subsequent commands
  if (!is.integer(x = dim)) {
    stop("\"dim\" must be a positive integer")
  }
  
  else if (length(x = dim) != 1L) {
    stop("single positive integer for \"dim\" expected")
  }
  
  else if (!is.numeric(x = param)) {
    stop("\"param\" must be a real vector")
  }
  
  else if (length(x = param) != (dim * (dim - 1)) / 2) {
    stop("\"param\" must be a real vector of length ", (dim * (dim - 1)) / 2, " or \"dim\" has been misspecified")
  }
     
  else if (!is.character(x = dispstr)) {
    stop("\"dispstr\" must be a character string")
  }
  
  else if (length(x = dispstr) != 1L) {
    stop("single character string for \"dispstr\" expected")
  }
           
  else if (!(dispstr %in% c("ar1", "ex", "toep", "un"))) {
    stop("\"dispstr\" is misspecified")
  }
  
  else if (!is.character(x = margins)) {
    stop("\"margins\" must be a character vector")
  }
    
  else if (length(x = margins) != dim) {
    stop("\"margins\" must be a character vector of length \"dim\"")
  }    
    
  else if (!is.list(x = paramMargins)) {
    stop("\"paramMargins\" must be a list")
  }
  
  else if (length(x = paramMargins) != dim) {
    stop("\"paramMargins\" must be a list of length \"dim\"")
  }
  
  else if (!is.integer(x = n)) {
    stop("\"n\" must be a positive integer")
  }
    
  else if (length(x = n) != 1L) {
    stop("single positive integer for \"n\" expected")
  }
  
  else if (!inherits(x = f, what = "formula")) {
    stop("\"f\" must be of class \"formula\"")
  }
           
  else if (!is.numeric(x = thetas)) {
    stop("\"thetas\" must be a numeric vector")
  }
  
  else if (length(x = thetas) != dim + 1) {
    stop("\"thetas\" must be a numeric vector of length \"dim\"")
  }
           
  else if (!is.character(x = link)) {
    stop("\"link\" must be a character string")
  }
  
  else if (length(x = link) != 1L) {
    stop("single character string for \"link\" expected")
  }
           
  else if (!(link %in% c("log", "logit"))) {
    stop("\"link\" is misspecified. Currently available link functions are: \"log\" and \"logit\"")
  } 
  
  else if (!exists(x = ".Random.seed")) {
    stop("state for the pseudo-random number generator has not been set")
  }
  
  else {  
    # Store the random number generator state
    seed <- .Random.seed
  
    # Predefine a normal copula
    copula_tmp <- copula::normalCopula(param = param, dim = dim, dispstr = dispstr)
    mvdc_tmp <- copula::mvdc(copula = copula_tmp, margins = margins, paramMargins = paramMargins)
  
    # Generate random variables from marginal probability distributions
    data_tmp <- data.table::as.data.table(copula::rMvdc(n = n, mvdc = mvdc_tmp))
    
    # Rename generated random variables
    data.table::setnames(x = data_tmp, old = colnames(data_tmp), new = sapply(X = 1:ncol(data_tmp), FUN = function(i) {sprintf("X%d", i)}))
    
    # Transform generated random variables if necessary
    if (!missing(transf)) {
      data_tmp <- eval(parse(text = paste0("transform(data_tmp, ", transf, ")")))
    }
    
    # Predefine linear combinations
    b <- model.matrix(f, data = data_tmp) %*% thetas
  
    # Compute individual probabilities for Y  
    if (link == "log") {
      pr <- exp(b)
    }
    
    if (link == "logit") {
      pr <- 1 / (1 + exp(-b))
    }
             
    # Generate a random variable from a binomial probability distribution
    data_tmp$Y <- rbinom(n = n, size = 1, prob = pr)
    
    return(x = list(seed = seed, data = data_tmp))
  }
}
  

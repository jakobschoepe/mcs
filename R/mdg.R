#' @title Generating multiple pseudo-random data sets for Monte Carlo simulations using \code{dg}.
#' @description \code{mdg} is a wrapper function to generate multiple pseudo-random data sets for Monte Carlo simulations using \code{dg}.
#' @usage mdg(X, seed,...) 
#' @param X An integer specifying the number of 
#' @param seed An integer vector containing the last state of the random number generator ("Mersenne-Twister") that should be used 
#' @param ... Arguments for code{dg}
#' @return A list containing the following elements:
#' \item{seed}{An integer vector containing the last state of the random number generator that should be used as seed to generate further data sets.}
#' \item{dg}{A list containing returns from \code{dg}.}
#' @note Please note that \code{mdg} was built as part of the design of a Monte Carlo simulation, and therefore serves a special-purpose only.
#' @author Jakob Schöpe
#' @example
#' param <- c(0.0,
#'            0.3, 0.1,
#'            0.0, 0.2, 0.5)
#'
#' margins <- c("norm", "binom", "binom", "norm")
#' paramMargins <- list(list(mean = 0, sd = 1), list(size = 1, prob = 0.3), list(size = 1, prob = .1), list(mean = 20, sd = 4))
#' f <- ~ V1 + V2 + V3 + V4
#' thetas <- c(-3.10, 0.00, -0.45, 0.22, -0.16)
#'
#' set.seed(seed = 123, kind = "Mersenne-Twister")
#' myData <- mdg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = f, thetas = thetas, link = "logit")

mdg <- function(X, seed) {
  if (!missing(x = seed)) {
    if (is.integer(x = seed) & length(x = seed) == 626) {
      RNGkind(kind = "Mersenne-Twister")
      .Random.seed <<- seed
    }
    
    else {
      stop("\"seed\" has been misspecified")
    }
  }
  
  else if (!exists(x = ".Random.seed")) {
    stop("state for the pseudo-random number generator has not been set")
  }
  
  else {
    # Iterate dg() to generate multiple pseudo-random data sets
    data_tmp <- lapply(X = 1:X, FUN = function(i) {
        tmp <- dg(param = param, dim = dim, dispstr = dispstr, margins = margins, paramMargins = paramMargins, n = n, f = f, thetas = thetas, link = link)
        return(tmp)
      }
    )
    
    # Store the last state of the pseudo-random number generator for possible continuation.
    seed <- .Random.seed
    
    return(x = list(seed = seed, dg = data_tmp)
  }
}

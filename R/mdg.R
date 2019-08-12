#' @title Generating multiple pseudo-random data sets for Monte Carlo simulations using \code{dg}.
#' @description \code{mdg} is a wrapper function to generate multiple pseudo-random data sets for Monte Carlo simulations using \code{dg}.
#' @usage mdg(X, seed,...) 
#' @param X An integer indicating the number of pseudo-random data sets to generate.
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

mdg <- function(X, export, path1, path2, seed, ...) {
  if (!is.integer(x = X)) {
    stop("\"X\" must be a positive integer")
  }
  
  else if (sign(x = X) == -1) {
    stop("\"X\" must be a positive integer")
  }
  
  else if (length(x = X) != 1L) {
    stop("single positive integer for \"X\" expected")  
  }
  
  else if (!is.logical(x = export)) {
    stop("\"export\" must be a logical constant")
  }
  
  else if (length(x = export) != 1L) {
    stop("single logical constant for \"X\" expected")  
  }
  
  else if(isTRUE(export)) {
    if (!is.character(x = path1) | !is.character(x = path2)) {
      stop("\"path1\" and \"path2\" must be character strings")
    }
  
    else if (length(x = path1) != 1L | length(x = path2) != 1L) {
      stop("single character string for \"path1\" and \"path2\" expected")
    }  
  }
   
  else if (!missing(x = seed)) {
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
        tmp <- dg(...)
        if (isTRUE(x = export)) {
          readr::write_csv(x = tmp$data, path = sprintf(path1, i))
          readr::write_csv(x = tmp$seed, path = sprintf(path2, i))
        }
        return(tmp$data)
      }
    )
    
    # Store the last state of the pseudo-random number generator for possible continuation.
    seed <- .Random.seed
    
    return(x = list(seed = seed, dg = data_tmp))
  }
}

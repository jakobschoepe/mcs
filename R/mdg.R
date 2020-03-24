#' @title Generating multiple pseudo-random data sets using \code{dg()}.
#' @description \code{mdg()} is a wrapper function to generate multiple pseudo-random data sets using \code{dg()}.
#' @usage mdg(X, export = FALSE, seed, ...) 
#' @param X An integer indicating the number of pseudo-random data sets to generate.
#' @param export A logical constant indicating if generated pseudo-random data sets should be exported to the working directory. 
#' @param seed A optional numeric vector containing the state of the pseudo-random number generator ("Mersenne-Twister"). 
#' @param ... Additional arguments for code{dg()}.
#' @return A list containing the following elements:
#' \item{data}{A list containing pseudo-random generated data sets.}
#' \item{seed}{A numeric vector containing the last state of the pseudo-random number generator.}
#' @note Please note that \code{mdg()} was built as part of the design of a Monte Carlo simulation, and therefore serves a special-purpose only.
#' @author Jakob Schöpe
#' @examples
#' param <- c(0.0,
#'            0.3, 0.1,
#'            0.0, 0.2, 0.5)
#'
#' margins <- c("norm", "binom", "binom", "norm")
#' paramMargins <- list(list(mean = 0, sd = 1), list(size = 1, prob = 0.3), list(size = 1, prob = .1), list(mean = 20, sd = 4))
#' f <- ~ X1 + X2 + X3 + X4
#' thetas <- c(-3.10, 0.00, -0.45, 0.22, -0.16)
#'
#' set.seed(seed = 123, kind = "Mersenne-Twister")
#' myData <- mdg(X = 10L, param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100L, f = f, thetas = thetas, link = "logit")
#' @export

mdg <- function(X, export = FALSE, seed, ...) {
  # Check passed arguments to smoothly run subsequent commands
  if (length(x = X) != 1L) {
    stop("single positive integer for \"X\" expected")  
  }
  
  else if (!is.integer(x = X) | sign(x = X) != 1) {
    stop("\"X\" must be a positive integer")
  }
  
  else if (length(x = export) != 1L) {
    stop("single logical constant for \"export\" expected")  
  }
    
  else if (!is.logical(x = export)) {
    stop("\"export\" must be a logical constant")
  }
    
  else if (!exists(x = ".Random.seed")) {
    stop("state for the pseudo-random number generator has not been set")
  }
  
  else {
    if (!missing(x = seed)) {
      if (!is.integer(x = seed) | length(x = seed) != 626) {
        stop("\"seed\" has been misspecified")
      }
    
      else {
        RNGkind(kind = "Mersenne-Twister")
        .Random.seed <<- seed
      }
    }
    
    # Iterate dg() to generate multiple pseudo-random data sets
    data_tmp <- lapply(X = 1:X, FUN = function(i) {
        tmp <- dg(...)
        if (isTRUE(x = export)) {
          readr::write_csv(x = tmp$data, path = sprintf(paste0("SimData", format(Sys.time(), "_%Y%m%d_%H%M%S_"), "%d.csv"), i))
        }
        return(tmp$data)
    })
    
    # Store the last state of the pseudo-random number generator for possible continuation
    seed <- .Random.seed
    
    return(x = list(data = data_tmp, seed = seed))
  }
}

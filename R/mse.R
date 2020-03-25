#' @title Estimating the mean squared error of an estimator of interest
#' @description \code{mse()} estimates the mean squared error of an estimator of interest in the context of a Monte Carlo simulation.
#' @usage mse(x, true)
#' @param x A numeric matrix containing the estimates of the estimator of interest.
#' @param true A numeric vector giving the estimand.
#' @return A numeric matrix giving the estimated mean squared error of the estimator of interest, and the corresponding Monte Carlo standard error.
#' @note Please note that \code{mse()} was built as part of the design of a Monte Carlo simulation, and therefore serves a special-purpose only.
#' @author Jakob Schöpe
#' @references Morris TP, White IR, Crowther MJ (2019) Using simulation studies to evaluate statistical methods. Stat Med 38:2074-2102
#' @export

mse <- function(x, true) {
  if (!is.matrix(x = x)) {
    stop("\"object\" must be a numeric matrix")
  }
  
  else if (!is.numeric(x = x)) {
    stop("\"object\" must be a numeric matrix")
  }
  
  else if (!is.vector(x = true, mode = "numeric")) {
    stop("\"true\" must be a numeric vector")
  }
  
  else if (length(true) != ncol(x)) {
    stop("\"true\" must be a numeric vector of length ", ncol(x))
  }
  
  else {
    n <- nrow(x)
    tv <- rep(x = true, each = n)
    tmp1 <- colSums((x - tv)^2) / n
    tmp2 <- sqrt(x = colSums(((x - tv)^2 - rep(x = tmp1, each = n))^2) / (n * (n - 1)))
    tmp3 <- cbind(tmp1, tmp2)
    rownames(tmp3) <- colnames(x)
    colnames(tmp3) <- c("MSE", "SE")
    return(tmp3)
  }
}

#' @title Estimating the empirical standard error of an estimator of interest
#' @description \code{empSE()} estimates the empirical standard error of an estimator of interest in the context of a Monte Carlo simulation.
#' @usage empSE(x)
#' @param x A numeric matrix containing the estimates of the estimator of interest.
#' @return A numeric matrix giving the estimated empirical standard error of the estimator of interest, and the corresponding Monte Carlo standard error.
#' @references Morris TP, White IR, Crowther MJ (2019) Using simulation studies to evaluate statistical methods. Stat Med 38:2074-2102
#' @notes Please note that \code{empSE()} was built as part of the design of a Monte Carlo simulation, and therefore serves a special-purpose only.
#' @author Jakob Schöpe
#' @export

empSE <- function(x) {
  if (!is.matrix(x = x)) {
    stop("\"object\" must be a numeric matrix")
  }
  
  else if (!is.numeric(x = x)) {
    stop("\"object\" must be a numeric matrix")
  }
  
  else {
    n <- nrow(x)
    cm <- rep(x = colMeans(x), each = n)
    tmp1 <- sqrt(colSums((x - cm)^2) / (n - 1))
    tmp2 <- tmp1 / sqrt(2 * (n - 1)) 
    tmp3 <- cbind(tmp1, tmp2)
    rownames(tmp3) <- colnames(x)
    colnames(tmp3) <- c("EmpSE", "SE")
    return(tmp3)
  }
}

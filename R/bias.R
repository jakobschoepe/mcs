#' @title Assess bias of an estimator of interest 
#' @description
#' @usage bias(x, true)
#' @param x A matrix containing the estimators of interest.
#' @param true A numerical vector giving the true estimand(s).
#' @details
#' @return
#' @notes Please note that \code{bias} was built as part of the design of a Monte Carlo simulation, and therefore serves a special-purpose only.
#' @references Morris TP, White IR, Crowther MJ (2019) Using simulation studies to evaluate statistical methods. Stat Med 38:2074-2102
#' @author Jakob Schöpe

bias <- function(x, true) {
  if (!is.matrix(x = x)) {
    stop("\"x\" must be a numeric matrix")
  }
  
  else if (!is.numeric(x = x)) {
    stop("\"x\" must be a numeric matrix")
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
    cm <- rep(x = colMeans(x), each = n)
    tmp1 <- colSums(x - tv) / n
    tmp2 <- sqrt(x = colSums((x - cm)^2) / (n * (n - 1)))
    tmp3 <- cbind(tmp1, tmp2)
    rownames(tmp3) <- colnames(x)
    colnames(tmp3) <- c("Bias", "SE")
    return(tmp3)
  }
}

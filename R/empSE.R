#' @title Estimating the empirical standard error of an estimator of interest
#' @description \code{empSE()} estimates the empirical standard error of an estimator of interest in the context of a Monte Carlo simulation.
#' @usage empSE(x)
#' @param x A numeric matrix containing the estimates of the estimator of interest.
#' @details The empirical standard error of an estimator of interest is derived from
#' \begin{equation*}
#' \sqrt{\frac{1}{n_{sim} - 1}\displaystyle\sum_{i=1}^{n_{sim}} (\hat{\theta}_{i} - \bar{\theta})^2}
#' \end{equation*}
#' where $n_{sim}$ is the number of repetitions of the Monte Carlo simulation, $\hat{\theta}_{i}$ is the estimate of the estimator of interest of the $i^{th}$ repetition, and $\bar{\theta}$ is the arithmetic mean of $\hat{\theta}_{i}$. The corresponding Monte Carlo standard error is derived from
#' \begin{equation*}
#' \frac{\widehat{EmpSE}}{\sqrt{2(n_{sim} - 1)}}
#' \end{equation*}
#' where $\widehat{EmpSE}$ is the estimated empirical standard error of the estimator of interest, and $n_{sim}$ is the number of repetitions of the Monte Carlo simulation.
#' @return A numeric matrix giving the estimated empirical standard error of the estimator of interest, and the corresponding Monte Carlo standard error.
#' @references Morris TP, White IR, Crowther MJ (2019) Using simulation studies to evaluate statistical methods. Stat Med 38:2074-2102
#' @notes Please note that \code{empSE()} was built as part of the design of a Monte Carlo simulation, and therefore serves a special-purpose only.
#' @author Jakob Schöpe
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

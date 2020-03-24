#' @title Estimating the coverage probability of a confidence interval estimator of interest
#' @description \code{coverage()} estimates the coverage probability of a confidence interval estimator of interest in the context of a Monte Carlo simulation.
#' @usage coverage(x, true)
#' @param x A numeric matrix containing the estimated limits of the confidence interval estimator of interest.
#' @param true A numeric vector giving the estimand.
#' @details The coverage probability of a confidence interval estimator of interest is derived from
#' \begin{equation*}
#' \frac{1}{n_{sim}}\displaystyle\sum_{i=1}^{n_{sim}} 1(\hat{\theta}_{low,i}\leq\theta\leq\hat{\theta}_{upp,i})
#' \end{equation*}
#' where $n_{sim}$ is the number of repetitions of the Monte Carlo simulation, $\hat{\theta}_{low,i}$ and $\hat{\theta}_{upp,i}$ are the estimated limits of the confidence interval estimator of interest of the $i^{th}$ repetition, and $\theta$ is the estimand. The corresponding Monte Carlo standard error is derived from
#' \begin{equation*}
#' \sqrt{\frac{\widehat{Cover.} \times (1 - \widehat{Cover.})}{n_{sim}}}
#' \end{equation*}
#' where $\widehat{Cover.}$ is the estimated coverage probability of the confidence interval estimator of interest, and $n_{sim}$ is the number of repetitions of the Monte Carlo simulation.
#' @return A numeric matrix giving the estimated coverage probability of the confidence interval estimator of interest, and the corresponding Monte Carlo standard error.
#' @references Morris TP, White IR, Crowther MJ (2019) Using simulation studies to evaluate statistical methods. Stat Med 38:2074-2102
#' @notes Please note that \code{coverage()} was built as part of the design of a Monte Carlo simulation, and therefore serves a special-purpose only.
#' @author Jakob Schöpe

coverage <- function(x, true) {
  if (!is.matrix(x = x)) {
    stop("\"x\" must be a numeric matrix")
  }
  
  else if (!is.numeric(x = x)) {
    stop("\"x\" must be a numeric matrix")
  }
  
  else if (!is.vector(x = true, mode = "numeric")) {
    stop("\"true\" must be a numeric vector")
  }
  
  else if (length(x = true) != (ncol(x = x) / 2)) {
    stop("\"true\" must be a numeric vector of length ", (ncol(x = x) / 2))
  }
  
  else {
    n <- nrow(x = x)
    varnames <- unique(x = substr(x = colnames(x = x), start = 1, stop = nchar(x = colnames(x = x)) - 4))
    tmp1 <- sapply(X = 1:length(x = varnames), function(i) {
    tmp1 <- sum(x[,grep(pattern = paste0(varnames[[i]], ".cil"), colnames(x))] <= true[[i]] & x[,grep(pattern = paste0(varnames[[i]], ".ciu"), colnames(x))] >= true[[i]]) / n 
    })
    tmp2 <- sqrt((tmp1 * (1 - tmp1)) / n)
    tmp3 <- cbind(tmp1, tmp2)
    rownames(tmp3) <- varnames
    colnames(tmp3) <- c("Coverage", "SE")
    return(tmp3)
  }
}

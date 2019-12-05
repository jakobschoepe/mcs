#' @title Assess the coverage of estimated confidence intervals
#' @description 
#' @usage coverage(x, varnames, true)
#' @param x A numeric matrix containing the confidence interval limits.
#' @param varnames A character vector giving the names i
#' @param true A numeric vector giving the true value(s).
#' @details
#' @return
#' @references Morris TP, White IR, Crowther MJ (2019) Using simulation studies to evaluate statistical methods. Stat Med 38:2074-2102
#' @notes Please note that \code{coverage} was built as part of the design of a Monte Carlo simulation, and therefore serves a special-purpose only.
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

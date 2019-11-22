#' @title
#' @description
#' @usage
#' @param
#' @param
#' @param
#' @details
#' @return
#' @references
#' @notes
#' @author Jakob Schöpe

coverage <- function(x, varnames, true) {
  if (!is.matrix(x = x)) {
    stop("\"x\" must be a numeric matrix")
  }
  
  else if (!is.numeric(x = x)) {
    stop("\"x\" must be a numeric matrix")
  }
  
  else if (!is.vector(x = varnames, mode = "character")) {
    stop("\"x\" must be a character vector")
  }
  
  else if (length(x = varnames) != (ncol(x = x) / 2)) {
    stop("\"varnames\" must be a character vector of length ", (ncol(x = x) / 2))
  }
  
  else if (!is.vector(x = true, mode = "numeric")) {
    stop("\"true\" must be a numeric vector")
  }
  
  else if (length(x = true) != (ncol(x = x) / 2)) {
    stop("\"true\" must be a numeric vector of length ", (ncol(x = x) / 2))
  }
  
  else {
    n <- nrow(x = x)
    tv <- rep(x = true, each = n)
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

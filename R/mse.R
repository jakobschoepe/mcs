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

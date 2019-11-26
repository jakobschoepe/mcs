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

m <- matrix(0, nrow=16, ncol=16)
m[2,1] <- 0.30
m[3,1] <- -0.50
m[9,1] <- -0.30
m[16,1] <- 0.30
m[9,3] <- 0.80
m[11,3] <- -0.40
m[6,4] <- 0.40
m[9,4] <- 0.50
m[14,4] <- -0.20
m[8,7] <- -0.60
m[10,7] <- 0.30
m[12,8] <- 0.20
m[16,8] <- 0.50
m[16,12] <- 0.50
m <- m + t(m)
diag(m) <- 1
param <- m[lower.tri(m)]

#param <- c(0.3, -0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.6, -0.3, 0, 0.8, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0, 0, 0, -0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0.5, 0, 0, 0)
margins <- c("norm", "binom", "binom", "beta", "norm", "binom", "gamma", "norm", "binom", "binom", "gamma", "pois", "pois", "norm", "beta", "norm")
paramMargins <- list(list(mean = 15, sd = 3), list(size = 1, prob = 0.3), list(size = 1, prob = 0.7), list(shape1 = 20, shape2 = 2), list(mean = 80, sd = 5), list(size = 1, prob = 0.1), list(shape = 2, scale = 2), list(mean = 60, sd = 10), list(size = 1, prob = 0.2), list(size = 1, prob = 0.5), list(shape = 1, scale = 2), list(lambda = 4), list(lambda = 2), list(mean = 150, sd = 15), list(shape1 = 15, shape2 = 1), list(mean = 40, sd = 3))
f <- ~ X1 + X4 + X6 + X8 + X9 + X10 + X11 + X13
transf <- "X4 = X4 * 50, X11 = X11 * 20, X15 = X15 * 100"
thetas <- c(3.950, 0.0370, -0.0130, -0.3600, 0.0220, 0.6930, 0.2000, -0.0021, 0.0400)

setwd(dir = "/Users/administrator/Documents/SimData")
set.seed(seed = 7957, kind = "Mersenne-Twister")
simData_0250_12 <- mdg(X = 750L, export = TRUE, param = param, dim = 16L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 250L, f = f, thetas = thetas_12, link = "logit")
simData_0250_24 <- mdg(X = 750L, export = TRUE, seed = simData_250_12$seed, param = param, dim = 16L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 250L, f = f, thetas = thetas_24, link = "logit")
simData_0250_48 <- mdg(X = 750L, export = TRUE, seed = simData_250_24$seed, param = param, dim = 16L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 250L, f = f, thetas = thetas_48, link = "logit")
simData_0500_12 <- mdg(X = 750L, export = TRUE, seed = simData_250_48$seed, param = param, dim = 16L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 500L, f = f, thetas = thetas_12, link = "logit")
simData_0500_24 <- mdg(X = 750L, export = TRUE, seed = simData_500_12$seed, param = param, dim = 16L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 500L, f = f, thetas = thetas_24, link = "logit")
simData_0500_48 <- mdg(X = 750L, export = TRUE, seed = simData_500_24$seed, param = param, dim = 16L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 500L, f = f, thetas = thetas_48, link = "logit")
simData_1000_12 <- mdg(X = 750L, export = TRUE, seed = simData_500_48$seed, param = param, dim = 16L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 1000L, f = f, thetas = thetas_12, link = "logit")
simData_1000_24 <- mdg(X = 750L, export = TRUE, seed = simData_1000_12$seed, param = param, dim = 16L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 1000L, f = f, thetas = thetas_24, link = "logit")
simData_1000_48 <- mdg(X = 750L, export = TRUE, seed = simData_1000_24$seed, param = param, dim = 16L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 1000L, f = f, thetas = thetas_48, link = "logit")

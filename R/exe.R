library()

param <- c(0.3, -0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.6, -0.3, 0, 0.8, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0, 0, 0, -0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0.5, 0, 0, 0)
margins <- c("norm", "binom", "binom", "beta", "norm", "binom", "gamma", "norm", "binom", "binom", "gamma", "pois", "pois", "norm", "beta", "norm")
paramMargins <- list(list(mean = 15, sd = 3), list(size = 1, prob = 0.3), list(size = 1, prob = 0.7), list(shape1 = 20, shape2 = 2), list(mean = 80, sd = 5), list(size = 1, prob = 0.1), list(shape = 2, scale = 2), list(mean = 60, sd = 10), list(size = 1, prob = 0.2), list(size = 1, prob = 0.5), list(shape = 1, scale = 2), list(lambda = 4), list(lambda = 1), list(mean = 150, sd = 15), list(shape1 = 15, shape2 = 1), list(mean = 40, sd = 3))
n <- rep(x = c(250, 500, 1000), each = 4500)
f <- V1 + V4 + V6 + V8 + V9 + V10 + V11 + V13

thetas <- c()


set.seed(seed = 7957, kind = "Mersenne-Twister")

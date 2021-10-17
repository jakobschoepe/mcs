test_that(desc = "mse() throws error messages",
          code = {expect_error(object = mse(x = NULL, true = NULL),
                               regexp = "\"x\" must be a numeric matrix");
                  expect_error(object = mse(x = matrix(data = 1:5, nrow = 5), true = NULL),
                               regexp = "\"true\" must be a numeric vector");
                  expect_error(object = mse(x = matrix(data = 1:5, nrow = 5), true = 1:2),
                               regexp = "\"true\" must be a numeric vector of length 1")})

test_that(desc = "Mean squared error and its Monte Carlo standard error are equal to 6 and 2.94957624",
          code = {expect_equal(object = mse(x = matrix(data = 1:5, nrow = 5), true = 1),
                               expected = array(data = c(6, 2.94957624), dim = c(1,2), dimnames = list(NULL, c("MSE", "SE"))))})

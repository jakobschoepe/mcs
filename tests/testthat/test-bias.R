test_that(desc = "bias() throws error messages",
          code = {expect_error(object = bias(x = NULL, true = NULL),
                               regexp = "\"x\" must be a numeric matrix");
                  expect_error(object = bias(x = matrix(data = 1:5, nrow = 5), true = NULL),
                               regexp = "\"true\" must be a numeric vector");
                  expect_error(object = bias(x = matrix(data = 1:5, nrow = 5), true = 1:2),
                               regexp = "\"true\" must be a numeric vector of length 1")})

test_that(desc = "Bias and its Monte Carlo standard error are equal to 2 and 0.70710678",
          code = {expect_equal(object = bias(x = matrix(data = 1:5, nrow = 5), true = 1),
                               expected = array(data = c(2, 0.70710678), dim = c(1,2), dimnames = list(NULL, c("Bias", "SE"))))})

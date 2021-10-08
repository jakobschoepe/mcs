test_that(desc = "Bias and its Monte Carlo standard error are equal to 2 and 0.70710678",
          code = {expect_equal(object = bias(x = matrix(1:5, nrow = 5), true = 1),
                                         expected = array(c(2, 0.70710678), dim = c(1,2), dimnames = list(NULL, c("Bias", "SE"))))})

test_that(desc = "Mean squared error and its Monte Carlo standard error are equal to 6 and 2.94957624",
          code = {expect_equal(object = mse(x = matrix(data = 1:5, nrow = 5), true = 1),
                               expected = array(data = c(6, 2.94957624), dim = c(1,2), dimnames = list(NULL, c("MSE", "SE"))))})

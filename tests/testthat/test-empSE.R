test_that(desc = "empSE() throws error message",
          code = {expect_error(object = empSE(x = NULL),
                               regexp = "\"x\" must be a numeric matrix")})

test_that(desc = "Empirical standard error and its Monte Carlo standard error are equal to 1.58113883 and 0.55901699",
          code = {expect_equal(object = empSE(x = matrix(data = 1:5, nrow = 5)),
                               expected = array(data = c(1.58113883, 0.55901699), dim = c(1,2), dimnames = list(NULL, c("EmpSE", "SE"))))})

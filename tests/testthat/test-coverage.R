test_that(desc = "coverage() throws error messages",
          code = {expect_error(object = coverage(x = NULL, true = NULL),
                               regexp = "\"x\" must be a numeric matrix");
                  expect_error(object = coverage(x = array(data = 1:10, dim = c(5,2), dimnames = list(NULL, c("x.cil", "x.ciu"))), true = NULL),
                               regexp = "\"true\" must be a numeric vector");
                  expect_error(object = coverage(x = array(data = 1:10, dim = c(5,2), dimnames = list(NULL, c("x.cil", "x.ciu"))), true = 1:2),
                               regexp = "\"true\" must be a numeric vector of length 1")})

test_that(desc = "Coverage and its Monte Carlo standard error are equal to 0.8 and 0.17888543",
          code = {expect_equal(object = coverage(x = array(data = 1:10, dim = c(5,2), dimnames = list(NULL, c("x.cil", "x.ciu"))), true = 4),
                               expected = array(data = c(0.8, 0.17888543), dim = c(1,2), dimnames = list("x", c("Coverage", "SE"))))})

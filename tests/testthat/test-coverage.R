test_that(desc = "Coverage and its Monte Carlo standard error are equal to 0.8 and 0.17888543",
          code = {expect_equal(object = coverage(x = array(data = 1:10, dim = c(5,2), dimnames = list(NULL, c("x.cil", "x.ciu"))), 4),
                               expected = array(data = c(0.8, 0.17888543), dim = c(1,2), dimnames = list("x", c("Coverage", "SE"))))})

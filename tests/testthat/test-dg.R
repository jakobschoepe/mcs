context(desc = "Data generating")

test_that(desc = "dg throws an error if arguments are misspecified",
          code = {expect_error(object = dg(param = NULL, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "\"param\" must be a real vector")
                  expect_error(object = dg(param = NULL, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "\"param\" must be a real vector of length \"dim\"")
                  expect_error(object = dg(param = param, dim = NULL, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "\"dim\" must be a positive integer")
                  expect_error(object = dg(param = param, dim = NULL, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "single positive integer for \"dim\" expected")
                  expect_error(object = dg(param = param, dim = NULL, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "\"dim\" is misspecified")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = NULL, margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "\"dispstr\" must be a character value")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = NULL, margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "single character value for \"dispstr\" expected")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = NULL, margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "\"dispstr\" is misspecified. Currently available structures are: \"ex\" for exchangeable, \"ar1\" for AR(1), \"toep\" for Toeplitz or \"un\" for unstructured")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = "un", margins = NULL, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "\"margins\" must be a character vector")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = "un", margins = NULL, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "\"margins\" must be a character vector of length \"dim\"")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = NULL, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "\"paramMargins\" must be a list")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = NULL, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "\"paramMargins\" must be a list of length \"dim\"")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = NULL, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "\"n\" must be a positive integer")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = NULL, f = ~ V1 + V2 + V3 + V4, betas = betas, link = "logit"),
                               regexp = "single positive integer for \"n\" expected")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = NULL, betas = betas, link = "logit"),
                               regexp = "\"f\" must be of class \"formula\"")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = NULL, link = "logit"),
                               regexp = "\"betas\" must be a numeric vector")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = NULL, link = "logit"),
                               regexp = "\"betas\" must be a numeric vector of length \"dim\"")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = NULL),
                               regexp = "\"link\" must be a character value")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = NULL),
                               regexp = "single character value for \"link\" expected")
                  expect_error(object = dg(param = param, dim = 4L, dispstr = "un", margins = margins, paramMargins = paramMargins, n = 100, f = ~ V1 + V2 + V3 + V4, betas = betas, link = NULL),
                               regexp = "\"link\" is misspecified. Currently available link functions are: \"log\" and \"logit\"")
          }
)          

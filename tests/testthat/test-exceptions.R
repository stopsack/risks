context("exception handling")

data(breastcancer)
dat <- breastcancer
dat$rand <- runif(n = 192, min = 0, max = 1)
dat$bin <- rep(1:2, times = 96)

test_that("approach = 'auto' handles difficult data", {
  expect_output(suppressWarnings(print(summary(riskratio(formula = death ~
                                                           stage + receptor,
                    data = dat[seq(from = 1, to = 192, by = 3),])))),
                "marginal standardization of a logistic")
})

test_that("nothing works", {
  expect_error(suppressWarnings(print(riskratio(formula = death ~
                                                  stage + receptor,
                                                data = dat[1:50,]))),
               "No model")
  expect_equal(return_failure(family = "poisson",
                              classname = "robpoisson")$converged,
               FALSE)
  expect_error(suppressWarnings(print(riskratio(formula = death ~ stage + receptor,
                                                data = dat[55:75, ],
                                                approach = "all"))),
               "No model")
  expect_error(suppressWarnings(print(summary(riskratio(formula = death ~ receptor + stage,
                                                        data = dat[55:75, ],
                                                        approach = "all")))),
               "No model")
})

test_that("bad parameter values are caught", {
  expect_error(estimate_risk(formula = death ~ stage + receptor,
                             estimand = "NONSENSE",
                             data = dat),
               "should be one of")
  expect_error(riskratio(formula = death ~ stage + receptor,
                         approach = "NONSENSE",
                         data = dat),
               "Approach 'NONSENSE' is not implemented")
  expect_error(riskdiff(formula = death ~ stage + receptor,
                         approach = "logistic",
                         data = dat),
               "Approach 'logistic' is not implemented")
  expect_error(riskratio(formula = death ~ stage + receptor, data = dat,
                         approach = "margstd_boot", variable = "NONSENSE"),
               "Variable 'NONSENSE' is not part of the model")
  expect_error(riskratio(formula = death ~ stage + receptor,
                         approach = "margstd_boot", at = "NONSENSE",
                         data = dat),
               "'at' has fewer than 2 levels. Contrasts cannot be estimated.")
  expect_error(riskratio(formula = death ~ stage + receptor,
                         approach = "margstd_boot", at = c("NONSENSE1", "2"),
                         data = dat),
               "Some of the levels, specificied via 'at ='")
  expect_warning(riskratio(formula = death ~ rand, data = dat,
                           approach = "margstd_boot", variable = "rand",
                           at = c(-1, 1)),
                 "out-of-range predictions for the variable 'rand'.")

  expect_error(riskratio(formula = death ~ rand, data = dat,
                         approach = "margstd_delta",
                         at = c(0.1, 0.3)),
               "Levels for marginal standardization")
})

context("exception handling")

dat <- data.frame(
  death    = c(rep(1, 54), rep(0, 138)),
  stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
               rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
  receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
               rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
               rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)),
  rand     = runif(n = 192, min = 0, max = 1),
  bin      = rep(1:2, n = 96))

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
               "No model converged")
  expect_equal(return_failure(family = "poisson",
                              classname = "robpoisson")$converged,
               FALSE)
  expect_error(suppressWarnings(print(riskratio(formula = death ~ receptor,
                                                data = dat[1:50],
                                                approach = "all"))),
               "No model converged")
  expect_error(suppressWarnings(print(summary(riskratio(formula = death ~ receptor,
                                                        data = dat[1:50],
                                                        approach = "all")))),
               "No model converged")
})

test_that("bad parameter values are caught", {
  expect_error(estimate_risk(formula = death ~ stage + receptor,
                             estimate = "NONSENSE",
                             data = dat),
               "Unknown estimate")
  expect_error(riskratio(formula = death ~ stage + receptor,
                         approach = "NONSENSE",
                         data = dat),
               "Approach 'NONSENSE' is not implemented")
  expect_error(riskdiff(formula = death ~ stage + receptor,
                         approach = "logistic",
                         data = dat),
               "Wacholder")
  expect_warning(tidy(riskdiff(formula = death ~ stage + receptor, data = dat),
                      exponentiate = TRUE),
                 "model did not use a log or logit link")
  expect_error(riskratio(formula = death ~ stage + receptor, data = dat,
                         approach = "margstd", variable = "NONSENSE"),
               "Variable 'NONSENSE' is not part of the model")
  expect_error(riskratio(formula = death ~ stage + receptor,
                         approach = "margstd", at = "NONSENSE",
                         data = dat),
               "Because 'at' has less than 2 levels")
  expect_error(riskratio(formula = death ~ stage + receptor,
                         approach = "margstd", at = c("NONSENSE1", "2"),
                         data = dat),
               "Some of the levels, specificied via 'at ='")
  expect_error(riskratio(formula = death ~ rand, data = dat,
                         approach = "margstd", variable = "rand"),
               "Variable 'rand' is not a factor, ")
  expect_error(riskratio(formula = death ~ rand, data = dat,
                         approach = "margstd"),
               "No exposure variable identified")
  expect_warning(riskratio(formula = death ~ rand, data = dat,
                           approach = "margstd", variable = "rand",
                           at = c(-1, 1)),
                 "out-of-range predictions for the variable 'rand'.")
  # implicit binary variable passes:
  expect_output(print(riskratio(death ~ bin, data = dat, approach = "margstd")),
                "bin2")
})

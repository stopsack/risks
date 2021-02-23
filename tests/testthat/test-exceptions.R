context("exception handling")

dat <- data.frame(
  death    = c(rep(1, 54), rep(0, 138)),
  stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
               rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
  receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
               rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
               rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

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
})

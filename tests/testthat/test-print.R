context("printing functions")

dat <- data.frame(
  death    = c(rep(1, 54), rep(0, 138)),
  stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
               rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
  receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
               rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
               rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

test_that("printing functions give output", {
  fit_all <- riskratio(formula = death ~ stage + receptor, data = dat,
                       approach = "all")
  fit_risks <- riskratio(formula = death ~ stage + receptor, data = dat)
  fit_margstd <- riskratio(formula = death ~ stage + receptor, data = dat,
                           approach = "margstd", variable = "stage",
                           at = c("Stage I", "Stage III"))
  fit_robpoisson <- riskratio(formula = death ~ stage + receptor, data = dat,
                              approach = "robpoisson")
  expect_output(print.summary.risks(summary(fit_all)), "All fitted models")
  expect_output(print.risks(fit_risks), "Risk ratio model")
  expect_output(print.risks(fit_risks), "(from Poisson model)")
  expect_output(print.risks(fit_risks), "188 Residual")
  expect_output(print.summary.risks(summary(fit_risks)),
                "model with starting values")
  expect_output(print.summary.risks(summary.risks(fit_risks)),
                "Confidence interval")
  expect_output(print.risks(fit_margstd), "Risk ratio model")
  expect_output(print.risks(fit_margstd), "(no starting values)")
  expect_output(print(summary.margstd(fit_margstd)), "logit")
  expect_output(print(summary.robpoisson(fit_robpoisson)),
                "poisson")
})

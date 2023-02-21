context("printing functions")

data(breastcancer)
dat <- breastcancer

test_that("printing functions give output", {
  fit_all <- riskratio(formula = death ~ stage + receptor, data = dat,
                       approach = "all")
  fit_risks <- riskratio(formula = death ~ stage + receptor, data = dat)
  fit_risks_startp <- riskratio(formula = death ~ stage + receptor, data = dat,
                                approach = "glm_startp")
  fit_margstd_boot <- riskratio(formula = death ~ stage + receptor, data = dat,
                                approach = "margstd_boot", variable = "stage",
                                at = c("Stage I", "Stage III"))
  fit_margstd_delta <- riskratio(formula = death ~ stage + receptor, data = dat,
                                 approach = "margstd_delta", variable = "stage",
                                 at = c("Stage I", "Stage III"))
  fit_robpoisson <- riskratio(formula = death ~ stage + receptor, data = dat,
                              approach = "robpoisson")
  fit_logistic <- riskratio(formula = death ~ stage + receptor, data = dat,
                            approach = "logistic")
  expect_output(print.summary.risks(summary(fit_all)), "All fitted models")
  expect_output(print.risks(fit_risks), "Risk ratio model")
  expect_output(print.risks(fit_risks), "no starting values")
  expect_output(print.risks(fit_risks), "188 Residual")
  expect_output(print.summary.risks(summary(fit_risks)),
                "model with delta method")
  expect_output(print.summary.risks(summary.risks(fit_risks)),
                "Confidence interval")
  expect_output(print.summary.risks(summary.risks(fit_risks_startp,
                                                  default = FALSE)),
                "profiling")
  expect_output(print.risks(fit_margstd_boot), "Risk ratio model")
  expect_output(print.risks(fit_margstd_boot), "(no starting values)")
  expect_output(print(summary.margstd_boot(fit_margstd_boot)), "logit")
  expect_output(print.risks(fit_margstd_delta), "Risk ratio model")
  expect_output(print.risks(fit_margstd_delta), "(no starting values)")
  expect_output(print(summary.margstd_boot(fit_margstd_delta)), "logit")
  expect_output(print(summary.robpoisson(fit_robpoisson)),
                "poisson")
  expect_output(print(summary(fit_logistic)),
                "logistic model")

  if(requireNamespace("addreg", quietly = TRUE)) {
    fit_addreg <- riskdiff(formula = death ~ stage + receptor, data = dat,
                           approach = "glm_cem")
    expect_output(print(summary(fit_addreg)),
                  "addreg::addreg")
  }
})

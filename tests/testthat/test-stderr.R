context("summary(), standard errors")

data(breastcancer)
dat <- breastcancer

test_that("Standard errors for RR(receptorLow) are the same", {
  rr_glm        <- riskratio(formula = death ~ receptor, data = dat,
                             approach = "glm")
  rr_glm_startp  <- riskratio(formula = death ~ receptor, data = dat,
                             approach = "glm_startp")
  rr_glm_startd  <- riskratio(formula = death ~ receptor, data = dat,
                              approach = "glm_startd")
  rr_robpoisson <- riskratio(formula = death ~ receptor, data = dat,
                             approach = "robpoisson")
  rr_duplicate  <- riskratio(formula = death ~ receptor, data = dat,
                             approach = "duplicate")
  rr_margstd_boot <- riskratio(formula = death ~ receptor, data = dat,
                               approach = "margstd_boot")
  rr_margstd_delta <- riskratio(formula = death ~ receptor, data = dat,
                                approach = "margstd_delta")
  rr_mh <- rr_rd_mantel_haenszel(data = dat, exposure = receptor,
                                 outcome = death,
                                 estimand = "rr")$std.error[1]

  se_glm <- summary(rr_glm)$coefficients["receptorLow", "Std. Error"]
  tol <- 0.01

  expect_equal(se_glm, summary(rr_glm_startp)$coefficients["receptorLow",
                                                          "Std. Error"],
               tolerance = tol)
  expect_equal(se_glm, summary(rr_glm_startd)$coefficients["receptorLow",
                                                          "Std. Error"],
               tolerance = tol)
  expect_equal(se_glm, summary(rr_robpoisson)$coefficients["receptorLow",
                                                           "Std. Error"],
               tolerance = tol)
  expect_equal(se_glm, summary(rr_duplicate)$coefficients["receptorLow",
                                                          "Std. Error"],
               tolerance = tol)
  expect_equal(se_glm, summary(rr_margstd_boot,
                               bootrepeats = 500)$coefficients["receptorLow",
                                                               "Std. Error"],
               tolerance = 0.03)
  expect_equal(se_glm, summary(rr_margstd_delta)$coefficients["receptorLow",
                                                               "Std. Error"],
               tolerance = tol)
  expect_equal(se_glm, rr_mh,
               tolerance = tol)

  if(requireNamespace("logbin", quietly = TRUE)) {
    rr_cem        <- riskratio(formula = death ~ receptor, data = dat,
                               approach = "glm_cem")
    rr_cem_startp <- riskratio(formula = death ~ receptor, data = dat,
                               approach = "glm_cem_startp")
    expect_equal(se_glm, summary(rr_cem)$coefficients["receptorLow",
                                                      "Std. Error"],
                 tolerance = tol)
    expect_equal(se_glm, summary(rr_cem_startp)$coefficients["receptorLow",
                                                             "Std. Error"],
                 tolerance = tol)
  }
})

test_that("Standard errors for RD(receptorLow) are the same", {
  rd_glm        <- riskdiff(formula = death ~ receptor, data = dat,
                            approach = "glm")
  rd_glm_startp <- riskdiff(formula = death ~ receptor, data = dat,
                            approach = "glm_startp")
  rd_robpoisson <- riskdiff(formula = death ~ receptor, data = dat,
                            approach = "robpoisson")
  rd_margstd_boot <- riskdiff(formula = death ~ receptor, data = dat,
                            approach = "margstd_boot")
  sum_margstd_boot <- summary(rd_margstd_boot, bootrepeats = 500)
  rd_margstd_delta <- riskdiff(formula = death ~ receptor, data = dat,
                            approach = "margstd_delta")
  sum_margstd_delta <- summary(rd_margstd_delta, bootrepeats = 500)
  rd_mh <- rr_rd_mantel_haenszel(data = dat, exposure = receptor,
                                 outcome = death,
                                 estimand = "rd")$std.error[1]

  se_glm <- summary(rd_glm)$coefficients["receptorLow", "Std. Error"]
  tol <- 0.01

  expect_equal(se_glm, summary(rd_glm_startp)$coefficients["receptorLow",
                                                          "Std. Error"],
               tolerance = tol)
  expect_equal(se_glm, summary(rd_robpoisson)$coefficients["receptorLow",
                                                           "Std. Error"],
               tolerance = tol)
  expect_equal(se_glm, sum_margstd_boot$coefficients["receptorLow", "Std. Error"],
               tolerance = 0.03)
  expect_output(print(sum_margstd_boot), "Confidence intervals for coefficients")
  expect_equal(se_glm, sum_margstd_delta$coefficients["receptorLow", "Std. Error"],
               tolerance = tol)
  expect_output(print(sum_margstd_delta), "Confidence intervals for coefficients")
  expect_equal(se_glm, rd_mh,
               tolerance = tol)

  if(requireNamespace("addreg", quietly = TRUE)) {
    rd_cem        <- riskdiff(formula = death ~ receptor, data = dat,
                              approach = "glm_cem")
    rd_cem_startp <- riskdiff(formula = death ~ receptor, data = dat,
                              approach = "glm_cem_startp")
    expect_equal(se_glm, summary(rd_cem)$coefficients["receptorLow",
                                                      "Std. Error"],
                 tolerance = tol)
    expect_equal(se_glm, summary(rd_cem_startp)$coefficients["receptorLow",
                                                             "Std. Error"],
                 tolerance = tol)
  }
})


test_that("Standard errors work with continuous exposure, margstd", {
  dat$cont <- runif(n = 192, min = -1, max = 1)

  expect_output(print(summary(riskratio(death ~ cont + receptor,
                                data = dat, approach = "margstd_boot"))),
                "bootstrap repeats")
  expect_output(print(summary(riskdiff(death ~ cont,
                                        data = dat, approach = "margstd_boot"))),
                "bootstrap repeats")
  expect_output(print(summary(riskratio(death ~ cont + receptor,
                                        data = dat, approach = "margstd_delta"))),
                "delta")
  expect_output(print(summary(riskdiff(death ~ cont,
                                       data = dat, approach = "margstd_delta"))),
                "delta")
})

context("summary(), standard errors")

test_that("Standard errors for RR(receptorLow) are the same", {
  dat <- data.frame(
    death    = c(rep(1, 54), rep(0, 138)),
    stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
                 rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
    receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
                 rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
                 rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

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
  rr_cem        <- riskratio(formula = death ~ receptor, data = dat,
                             approach = "glm_cem")
  rr_cem_startp  <- riskratio(formula = death ~ receptor, data = dat,
                             approach = "glm_cem_startp")
  rr_margstd    <- riskratio(formula = death ~ receptor, data = dat,
                             approach = "margstd")
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
  expect_equal(se_glm, summary(rr_cem)$coefficients["receptorLow",
                                                    "Std. Error"],
               tolerance = tol)
  expect_equal(se_glm, summary(rr_cem_startp)$coefficients["receptorLow",
                                                          "Std. Error"],
               tolerance = tol)
  expect_equal(se_glm, summary(rr_margstd,
                               bootrepeats = 500)$coefficients["receptorLow",
                                                               "Std. Error"],
               tolerance = 0.03)
  expect_equal(se_glm, rr_mh,
               tolerance = tol)

})

test_that("Standard errors for RD(receptorLow) are the same", {
  dat <- data.frame(
    death    = c(rep(1, 54), rep(0, 138)),
    stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
                 rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
    receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
                 rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
                 rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

  rd_glm        <- riskdiff(formula = death ~ receptor, data = dat,
                            approach = "glm")
  rd_glm_startp <- riskdiff(formula = death ~ receptor, data = dat,
                            approach = "glm_startp")
  rd_robpoisson <- riskdiff(formula = death ~ receptor, data = dat,
                            approach = "robpoisson")
  rd_cem        <- riskdiff(formula = death ~ receptor, data = dat,
                            approach = "glm_cem")
  rd_cem_startp <- riskdiff(formula = death ~ receptor, data = dat,
                            approach = "glm_cem_startp")
  rd_margstd    <- riskdiff(formula = death ~ receptor, data = dat,
                            approach = "margstd")
  sum_margstd <- summary(rd_margstd, bootrepeats = 500)
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
  expect_equal(se_glm, summary(rd_cem)$coefficients["receptorLow",
                                                    "Std. Error"],
               tolerance = tol)
  expect_equal(se_glm, summary(rd_cem_startp)$coefficients["receptorLow",
                                                          "Std. Error"],
               tolerance = tol)
  expect_equal(se_glm, sum_margstd$coefficients["receptorLow", "Std. Error"],
               tolerance = 0.03)
  expect_output(print(sum_margstd), "Confidence intervals for coefficients")
  expect_equal(se_glm, rd_mh,
               tolerance = tol)
})

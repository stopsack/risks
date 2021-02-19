context("summary(), standard errors")

test_that("Standard errors for RR(receptorLow) are the same", {
  dat <- data.frame(
    death    = c(rep(1, 54), rep(0, 138)),
    stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
                 rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
    receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
                 rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
                 rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

  rd_glm        <- riskratio(formula = death ~ receptor, data = dat, approach = "glm")
  rd_glm_start  <- riskratio(formula = death ~ receptor, data = dat, approach = "glm_start")
  rd_robpoisson <- riskratio(formula = death ~ receptor, data = dat, approach = "robpoisson")
  rd_cem        <- riskratio(formula = death ~ receptor, data = dat, approach = "glm_cem")
  rd_cem_start  <- riskratio(formula = death ~ receptor, data = dat, approach = "glm_cem_start")
  rd_margstd    <- riskratio(formula = death ~ receptor, data = dat, approach = "margstd")

  se_glm <- summary(rd_glm)$coefficients["receptorLow", "Std. Error"]
  tol <- 0.01

  expect_equal(se_glm, summary(rd_glm_start)$coefficients["receptorLow",  "Std. Error"], tolerance = tol)
  expect_equal(se_glm, summary(rd_glm_start)$coefficients["receptorLow",  "Std. Error"], tolerance = tol)
  expect_lt(se_glm,    summary.robpoisson(rd_robpoisson)$coefficients["receptorLow", "Std. Error"])
  expect_equal(se_glm, summary(rd_cem)$coefficients["receptorLow",        "Std. Error"], tolerance = tol)
  expect_equal(se_glm, summary(rd_cem_start)$coefficients["receptorLow",  "Std. Error"], tolerance = tol)
  expect_equal(se_glm, summary(rd_margstd, bootrepeats = 500)$coefficients["receptorLow", "Std. Error"],
               tolerance = 0.03)
})

test_that("Standard errors for RD(receptorLow) are the same", {
  dat <- data.frame(
    death    = c(rep(1, 54), rep(0, 138)),
    stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
                 rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
    receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
                 rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
                 rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

  rd_glm        <- riskdiff(formula = death ~ receptor, data = dat, approach = "glm")
  rd_glm_start  <- riskdiff(formula = death ~ receptor, data = dat, approach = "glm_start")
  rd_robpoisson <- riskdiff(formula = death ~ receptor, data = dat, approach = "robpoisson")
  rd_cem        <- riskdiff(formula = death ~ receptor, data = dat, approach = "glm_cem")
  rd_cem_start  <- riskdiff(formula = death ~ receptor, data = dat, approach = "glm_cem_start")
  rd_margstd    <- riskdiff(formula = death ~ receptor, data = dat, approach = "margstd")

  se_glm <- summary(rd_glm)$coefficients["receptorLow", "Std. Error"]
  tol <- 0.01

  expect_equal(se_glm, summary(rd_glm_start)$coefficients["receptorLow",  "Std. Error"], tolerance = tol)
  expect_equal(se_glm, summary(rd_glm_start)$coefficients["receptorLow",  "Std. Error"], tolerance = tol)
  expect_lt(se_glm,    summary(rd_robpoisson)$coefficients["receptorLow", "Std. Error"])
  expect_equal(se_glm, summary(rd_cem)$coefficients["receptorLow",        "Std. Error"], tolerance = tol)
  expect_equal(se_glm, summary(rd_cem_start)$coefficients["receptorLow",  "Std. Error"], tolerance = tol)
  expect_equal(se_glm, summary(rd_margstd, bootrepeats = 500)$coefficients["receptorLow", "Std. Error"],
               tolerance = 0.03)
})

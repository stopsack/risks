context("coefficients")

test_that("RD coefficients for receptorLow are the same", {
  dat <- data.frame(
    death    = c(rep(1, 54), rep(0, 138)),
    stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
                 rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
    receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
                 rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
                 rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

  rd_glm        <- coef(riskdiff(formula = death ~ receptor, data = dat, approach = "glm"))
  rd_glm_start  <- coef(riskdiff(formula = death ~ receptor, data = dat, approach = "glm_start"))
  rd_robpoisson <- coef(riskdiff(formula = death ~ receptor, data = dat, approach = "robpoisson"))
  rd_cem        <- coef(riskdiff(formula = death ~ receptor, data = dat, approach = "glm_cem"))
  rd_cem_start  <- coef(riskdiff(formula = death ~ receptor, data = dat, approach = "glm_cem_start"))
  rd_margstd    <- coef(riskdiff(formula = death ~ receptor, data = dat, approach = "margstd"))

  tol <- 0.000001

  expect_equal((23/48) - (31/144), as.numeric(rd_glm["receptorLow"]),        tolerance = tol)
  expect_equal((23/48) - (31/144), as.numeric(rd_glm_start["receptorLow"]),  tolerance = tol)
  expect_equal((23/48) - (31/144), as.numeric(rd_robpoisson["receptorLow"]), tolerance = tol)
  expect_equal((23/48) - (31/144), as.numeric(rd_cem["receptorLow"]),        tolerance = tol)
  expect_equal((23/48) - (31/144), as.numeric(rd_cem_start["receptorLow"]),  tolerance = tol)
  expect_equal((23/48) - (31/144), as.numeric(rd_margstd["receptorLow"]),    tolerance = tol)
  expect_equal(rd_glm, rd_glm_start, tolerance = 0.03)
  expect_equal(rd_glm, rd_robpoisson, tolerance = 0.03)
  expect_equal(rd_glm, rd_cem, tolerance = 0.01)
  expect_equal(rd_glm, rd_cem_start, tolerance = 0.01)
  expect_equal(rd_glm[c("stageStage II", "stageStage III")],
               rd_margstd[c("stageStage II", "stageStage III")], tolerance = 0.03)
})

test_that("RR coefficients for receptorLow are the same", {
  dat <- data.frame(
    death    = c(rep(1, 54), rep(0, 138)),
    stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
                 rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
    receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
                 rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
                 rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

  rd_glm        <- coef(riskratio(formula = death ~ receptor, data = dat, approach = "glm"))
  rd_glm_start  <- coef(riskratio(formula = death ~ receptor, data = dat, approach = "glm_start"))
  rd_robpoisson <- coef(riskratio(formula = death ~ receptor, data = dat, approach = "robpoisson"))
  rd_cem        <- coef(riskratio(formula = death ~ receptor, data = dat, approach = "glm_cem"))
  rd_cem_start  <- coef(riskratio(formula = death ~ receptor, data = dat, approach = "glm_cem_start"))
  rd_margstd    <- coef(riskratio(formula = death ~ receptor, data = dat, approach = "margstd"))

  tol <- 0.000001

  expect_equal(log((23/48) / (31/144)), as.numeric(rd_glm["receptorLow"]),        tolerance = tol)
  expect_equal(log((23/48) / (31/144)), as.numeric(rd_glm_start["receptorLow"]),  tolerance = tol)
  expect_equal(log((23/48) / (31/144)), as.numeric(rd_robpoisson["receptorLow"]), tolerance = tol)
  expect_equal(log((23/48) / (31/144)), as.numeric(rd_cem["receptorLow"]),        tolerance = tol)
  expect_equal(log((23/48) / (31/144)), as.numeric(rd_cem_start["receptorLow"]),  tolerance = tol)
  expect_equal(log((23/48) / (31/144)), as.numeric(rd_margstd["receptorLow"]),    tolerance = tol)
  expect_equal(rd_glm, rd_glm_start, tolerance = 0.03)
  expect_equal(rd_glm, rd_robpoisson, tolerance = 0.03)
  expect_equal(rd_glm, rd_cem, tolerance = 0.01)
  expect_equal(rd_glm, rd_cem_start, tolerance = 0.01)
  expect_equal(rd_glm[c("stageStage II", "stageStage III")],
               rd_margstd[c("stageStage II", "stageStage III")], tolerance = 0.03)
})

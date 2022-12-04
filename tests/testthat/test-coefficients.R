context("coefficients")

test_that("RD coefficients for receptorLow are the same", {
  dat <- data.frame(
    death    = c(rep(1, 54), rep(0, 138)),
    stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
                 rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
    receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
                 rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
                 rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

  rd_glm        <- coef(riskdiff(formula = death ~ receptor, data = dat,
                                 approach = "glm"))
  rd_glm_startp  <- coef(riskdiff(formula = death ~ receptor, data = dat,
                                 approach = "glm_startp"))
  rd_robpoisson <- coef(riskdiff(formula = death ~ receptor, data = dat,
                                 approach = "robpoisson"))
  rd_margstd_boot <- coef(riskdiff(formula = death ~ receptor, data = dat,
                                   approach = "margstd_boot"))
  rd_margstd_delta <- coef(riskdiff(formula = death ~ receptor, data = dat,
                                    approach = "margstd_delta"))
  rd_mh <- rr_rd_mantel_haenszel(data = dat, exposure = receptor,
                                 outcome = death,
                                 estimand = "rd")$estimate[1]

  tol <- 0.000001

  expect_equal((23/48) - (31/144), as.numeric(rd_glm["receptorLow"]),
               tolerance = tol)
  expect_equal((23/48) - (31/144), as.numeric(rd_glm_startp["receptorLow"]),
               tolerance = tol)
  expect_equal((23/48) - (31/144), as.numeric(rd_robpoisson["receptorLow"]),
               tolerance = tol)
  expect_equal((23/48) - (31/144), as.numeric(rd_margstd_boot["receptorLow"]),
               tolerance = tol)
  expect_equal((23/48) - (31/144), as.numeric(rd_margstd_delta["receptorLow"]),
               tolerance = tol)
  expect_equal((23/48) - (31/144), rd_mh,    tolerance = tol)
  expect_equal(rd_glm, rd_glm_startp, tolerance = 0.03)
  expect_equal(rd_glm, rd_robpoisson, tolerance = 0.03)
  expect_equal(rd_glm["receptorLow"], rd_margstd_boot["receptorLow"],
               tolerance = 0.03)
  expect_equal(rd_glm["receptorLow"], rd_margstd_delta["receptorLow"],
               tolerance = 0.01)

  if(requireNamespace("addreg", quietly = TRUE)) {
    rd_cem        <- coef(riskdiff(formula = death ~ receptor, data = dat,
                                   approach = "glm_cem"))
    rd_cem_startp  <- coef(riskdiff(formula = death ~ receptor, data = dat,
                                    approach = "glm_cem_startp"))
    expect_equal((23/48) - (31/144), as.numeric(rd_cem["receptorLow"]),
                 tolerance = tol)
    expect_equal((23/48) - (31/144), as.numeric(rd_cem_startp["receptorLow"]),
                 tolerance = tol)
    expect_equal(rd_glm, rd_cem, tolerance = 0.01)
    expect_equal(rd_glm, rd_cem_startp, tolerance = 0.01)
  }
})

test_that("RR coefficients for receptorLow are the same", {
  dat <- data.frame(
    death    = c(rep(1, 54), rep(0, 138)),
    stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
                 rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
    receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
                 rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
                 rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

  rr_glm        <- coef(riskratio(formula = death ~ receptor, data = dat,
                                  approach = "glm"))
  rr_glm_startp  <- coef(riskratio(formula = death ~ receptor, data = dat,
                                  approach = "glm_startp"))
  rr_glm_startd  <- coef(riskratio(formula = death ~ receptor, data = dat,
                                   approach = "glm_startd"))
  rr_robpoisson <- coef(riskratio(formula = death ~ receptor, data = dat,
                                  approach = "robpoisson"))
  rr_duplicate <- coef(riskratio(formula = death ~ receptor, data = dat,
                                 approach = "duplicate"))
  rr_margstd_boot <- coef(riskratio(formula = death ~ receptor, data = dat,
                                    approach = "margstd_boot"))
  rr_margstd_delta <- coef(riskratio(formula = death ~ receptor, data = dat,
                                     approach = "margstd_delta"))
  rr_mh <- rr_rd_mantel_haenszel(data = dat, exposure = receptor,
                                 outcome = death,
                                 estimand = "rr")$estimate[1]

  tol <- 0.000001

  expect_equal(log((23/48) / (31/144)), as.numeric(rr_glm["receptorLow"]),
               tolerance = tol)
  expect_equal(log((23/48) / (31/144)), as.numeric(rr_glm_startp["receptorLow"]),
               tolerance = tol)
  expect_equal(log((23/48) / (31/144)), as.numeric(rr_glm_startd["receptorLow"]),
               tolerance = tol)
  expect_equal(log((23/48) / (31/144)), as.numeric(rr_robpoisson["receptorLow"]),
               tolerance = tol)
  expect_equal(log((23/48) / (31/144)), as.numeric(rr_margstd_boot["receptorLow"]),
               tolerance = tol)
  expect_equal(log((23/48) / (31/144)), as.numeric(rr_margstd_delta["receptorLow"]),
               tolerance = tol)
  expect_equal(log((23/48) / (31/144)), rr_mh,
               tolerance = tol)

  expect_equal(rr_glm, rr_glm_startp, tolerance = 0.03)
  expect_equal(rr_glm, rr_glm_startd, tolerance = 0.03)
  expect_equal(rr_glm, rr_robpoisson, tolerance = 0.03)
  expect_equal(rr_glm["receptorLow"], rr_margstd_boot["receptorLow"],
               tolerance = 0.03)
  expect_equal(rr_glm["receptorLow"], rr_margstd_delta["receptorLow"],
               tolerance = 0.01)

  if(requireNamespace("logbin", quietly = TRUE)) {
    rr_cem        <- coef(riskratio(formula = death ~ receptor, data = dat,
                                    approach = "glm_cem"))
    rr_cem_startp  <- coef(riskratio(formula = death ~ receptor, data = dat,
                                     approach = "glm_cem_startp"))
    expect_equal(log((23/48) / (31/144)), as.numeric(rr_cem["receptorLow"]),
                 tolerance = tol)
    expect_equal(log((23/48) / (31/144)), as.numeric(rr_cem_startp["receptorLow"]),
                 tolerance = tol)
    expect_equal(rr_glm, rr_cem, tolerance = 0.01)
    expect_equal(rr_glm, rr_cem_startp, tolerance = 0.01)
  }
})

test_that("Continuous and implicit binary variables pass in marg std", {
  dat <- data.frame(
    death    = c(rep(1, 54), rep(0, 138)),
    stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
                 rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
    receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
                 rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
                 rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)),
    cont     = runif(n = 192, min = -1, max = 1))

  # specific levels for continuous variable pass in margstd_BOOT:
  expect_output(print(riskratio(death ~ cont,
                                at = c(-0.1, 0.3),
                                data = dat, approach = "margstd_boot")),
                "-0.1")
  # a continuous variable passes:
  expect_output(print(riskratio(death ~ cont,
                                data = dat, approach = "margstd_boot")),
                "cont")
  expect_output(print(riskratio(death ~ cont,
                                data = dat, approach = "margstd_delta")),
                "cont")

  # specific levels for continuous variable pass in margstd_BOOT:
  expect_output(print(riskratio(death ~ receptor,
                                at = c("High", "Low"),
                                data = dat, approach = "margstd_delta")),
                "Low")
})

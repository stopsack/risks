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

test_that("empty data set fails with correct message", {
  expect_error(
    suppressWarnings(riskratio(
      formula = death ~ stage + receptor,
      data = dat[0:0, ])),
    "contrasts can be applied only to factors with 2 or more levels")
})

test_that("margstd_delta, _boot accept NA in exposure, covariates, outcome", {
  dat$stage_miss <- dat$stage
  dat$stage_miss[101:130] <- NA
  dat$stage_miss_num <- as.numeric(dat$stage_miss)
  dat$receptor_miss <- dat$receptor
  dat$receptor_miss[150:160] <- NA
  dat$death_miss <- dat$death
  dat$death_miss[50:60] <- NA

  expect_equal(
    riskratio(
    formula = death ~ stage_miss_num,
    data = dat,
    approach = "margstd_delta")$coefficients[["stage_miss_num"]],
    expected = 0.7857696,
    tolerance = 0.0000001)

  expect_equal(
    riskratio(
      formula = death ~ stage_miss_num,
      data = dat,
      approach = "margstd_boot")$coefficients[["stage_miss_num"]],
    expected = 0.9518342,
    tolerance = 0.0000001)

  expect_equal(
    riskratio(
      formula = death ~ stage_miss_num + receptor_miss,
      data = dat,
      approach = "margstd_delta")$coefficients[["stage_miss_num"]],
    expected = 0.6593813,
    tolerance = 0.0000001)

  expect_equal(
    riskratio(
      formula = death ~ stage_miss_num + receptor_miss,
      data = dat,
      approach = "margstd_boot")$coefficients[["stage_miss_num"]],
    expected = 0.9009893,
    tolerance = 0.0000001)

  expect_equal(
    riskratio(
      formula = death_miss ~ stage,
      data = dat,
      approach = "margstd_delta")$coefficients[["stageStage II"]],
    riskratio(
      formula = death_miss ~ stage,
      data = dat,
      approach = "margstd_boot")$coefficients[["stageStage II"]])
})

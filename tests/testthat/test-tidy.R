context("tidy model summaries")

data(breastcancer)
dat <- breastcancer
dat$cont <- runif(n = 192, min = -1, max = 1)

test_that("tidy(risk*(...), approach = 'all') returns 9 ratio, 7 diff models (categorical)", {
  fit_rr <- riskratio(formula = death ~ stage + receptor,
                      data = dat, approach = "all")
  fit_rd <- riskdiff(formula  = death ~ stage + receptor,
                     data = dat, approach = "all")

  # RR: no glm; if logbin installed: also logbin and logbin_start
  expect_equal(length(unique(broom::tidy(fit_rr)$model)),
               7 + 2 * requireNamespace("logbin", quietly = TRUE))
  # RD: with glm, but no logistic, no duplicate, no glm-start/duplicate
  # if addreg installed: addreg and addreg_start
  expect_equal(length(unique(broom::tidy(fit_rd)$model)),
               5 + 2 * requireNamespace("addreg", quietly = TRUE))

  expect_equal(nrow(tidy(riskratio(formula = death ~ stage + receptor,
                                   data = dat, approach = "glm_startp"),
                         bootverbose = TRUE)),
               4)  # coefs from glm_start
})

test_that("tidy(risk*(...), approach = 'all') returns 10 ratio, 7 diff models (continuous)", {
  fit_rr <- riskratio(formula = death ~ cont,
                      data = dat, approach = "all")
  success_models <- unique(broom::tidy(fit_rr)$model)
  fit_rd <- riskdiff(formula  = death ~ cont,
                     data = dat, approach = "all")

  # expect that all 9 models work for RR, but allow plain glm to fail because of
  # random exposure variable
  expect_equal(
    length(success_models) == 8 +
      2 * requireNamespace("logbin", quietly = TRUE) |
      (length(success_models) == 7 +
         2 * requireNamespace("logbin", quietly = TRUE) &
         !("glm" %in% success_models)),
    TRUE)
  # RD has no logistic, no duplicate, and no glm-start/duplicate:
  expect_equal(length(unique(broom::tidy(fit_rd)$model)),
               5 + 2 * requireNamespace("addreg", quietly = TRUE))

  expect_equal(nrow(tidy(riskratio(formula = death ~ cont,
                                   data = dat, approach = "glm_startp"),
                         bootverbose = TRUE)),
               2)  # 2 coefs from glm_start with Poisson: intercept and 'cont'
})

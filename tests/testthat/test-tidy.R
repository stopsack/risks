context("tidy model summaries")

dat <- data.frame(
  death    = c(rep(1, 54), rep(0, 138)),
  stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
               rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
  receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
               rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
               rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)),
  cont     = runif(n = 192, min = -1, max = 1))

test_that("tidy(risk*(...), approach = 'all') returns 8 ratio, 7 diff models (categorical)", {
  fit_rr <- riskratio(formula = death ~ stage + receptor,
                      data = dat, approach = "all")
  fit_rd <- riskdiff(formula  = death ~ stage + receptor,
                     data = dat, approach = "all")

  expect_equal(length(unique(broom::tidy(fit_rr)$model)), 9)  # no glm
  # with glm, but no logistic, no duplicate, no glm-start/duplicate:
  expect_equal(length(unique(broom::tidy(fit_rd)$model)), 7)

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
  expect_equal(length(success_models) == 10 |
                 length(success_models) == 9 & !("glm" %in% success_models),
               TRUE)
  # RD has no logistic, no duplicate, and no glm-start/duplicate:
  expect_equal(length(unique(broom::tidy(fit_rd)$model)), 7)

  expect_equal(nrow(tidy(riskratio(formula = death ~ cont,
                                   data = dat, approach = "glm_startp"),
                         bootverbose = TRUE)),
               2)  # 2 coefs from glm_start with Poisson: intercept and 'cont'
})

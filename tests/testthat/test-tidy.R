context("tidy model summaries")

dat <- data.frame(
  death    = c(rep(1, 54), rep(0, 138)),
  stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
               rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
  receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
               rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
               rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

test_that("tidy(risk*(...), approach = 'all') returns 7 ratio, 6 diff models", {
  fit_rr <- riskratio(formula = death ~ stage + receptor,
                      data = dat, approach = "all")
  fit_rd <- riskdiff(formula  = death ~ stage + receptor,
                     data = dat, approach = "all")

  expect_equal(length(unique(broom::tidy(fit_rr)$model)), 7)  # no glm
  # no logistic, no duplicate:
  expect_equal(length(unique(broom::tidy(fit_rd)$model)), 6)

  expect_equal(nrow(tidy(riskratio(formula = death ~ stage + receptor,
                                   data = dat, approach = "glm_start"),
                         bootverbose = TRUE)),
               4)  # coefs from glm_start
})

context("tidy model summaries")

dat <- data.frame(
  death    = c(rep(1, 54), rep(0, 138)),
  stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
               rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
  receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
               rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
               rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

test_that("tidy(risk*(...), approach = 'all') returns 6 models", {
  fit_rr <- riskratio(formula = death ~ stage + receptor, data = dat, approach = "all")
  fit_rd <- riskdiff(formula  = death ~ stage + receptor, data = dat, approach = "all")

  expect_equal(length(unique(broom::tidy(fit_rr)$model)), 6)  # no glm
  expect_equal(length(unique(broom::tidy(fit_rd)$model)), 6)  # no logistic
})

test_that("tidy(risk*(..., weight = myweight), approach = 'all') returns 4 or 5 models", {
  dat$myweight <- rep(1:10, times = 20)[1:192]

  # weights: need to run via eval(substitute())
  fit_rrw <- eval(substitute(riskratio(formula = death ~ stage + receptor, data = dat,
                                       approach = "all", weights = myweight)))
  fit_rdw <- eval(substitute(riskdiff(formula  = death ~ stage + receptor, data = dat,
                                      approach = "all", weights = myweight)))

  expect_equal(length(unique(broom::tidy(fit_rrw)$model)), 5)  # no cem*
  expect_equal(length(unique(broom::tidy(fit_rdw)$model)), 4)  # no logistic, no cem*
})

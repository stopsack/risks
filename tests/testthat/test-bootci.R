context("bootci types")

dat <- data.frame(
  death    = c(rep(1, 54), rep(0, 138)),
  stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
               rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
  receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
               rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
               rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

test_that("bootci types give similar tidy results", {
  set.seed(123)
  coe_bcapar <- confint(riskratio(formula = death ~ stage + receptor,
                                  data = dat, approach = "margstd"),
                        bootci = "bca", bootrepeats = 500)
  set.seed(123)
  coe_normal <- confint(riskratio(formula = death ~ stage + receptor,
                                  data = dat, approach = "margstd"),
                        bootci = "normal", bootrepeats = 200)
  set.seed(123)
  coe_nonpar <- confint(riskratio(formula = death ~ stage + receptor,
                                  data = dat, approach = "margstd"),
                        bootci = "nonpar", bootrepeats = 500)

  expect_equal(coe_bcapar, coe_normal, tolerance = 0.5)
  expect_equal(coe_bcapar, coe_nonpar, tolerance = 0.1)
})


test_that("bootci types give similar SE in summary", {
  set.seed(123)
  se_bcapar <- summary(riskratio(formula = death ~ stage + receptor,
                                  data = dat, approach = "margstd"),
                        bootci = "bca", bootrepeats = 500)$conf.int$std.error
  se_bcapar <- unname(se_bcapar)
  set.seed(123)
  se_normal <- summary(riskratio(formula = death ~ stage + receptor,
                                  data = dat, approach = "margstd"),
                        bootci = "normal", bootrepeats = 200)$conf.int$std.error
  se_normal <- unname(se_normal)
  set.seed(123)
  se_nonpar <- summary(riskratio(formula = death ~ stage + receptor,
                                  data = dat, approach = "margstd"),
                        bootci = "nonpar", bootrepeats = 500)$conf.int$std.error
  se_nonpar <- unname(se_nonpar)
  expect_equal(se_bcapar, se_normal, tolerance = 0.5)
  expect_equal(se_bcapar, se_nonpar, tolerance = 0.5)

  set.seed(123)
  se_bcapar <- summary(riskdiff(formula = death ~ stage + receptor,
                                 data = dat, approach = "margstd"),
                       bootci = "bca", bootrepeats = 500)$conf.int$std.error
  se_bcapar <- unname(se_bcapar)
  set.seed(123)
  se_normal <- summary(riskdiff(formula = death ~ stage + receptor,
                                 data = dat, approach = "margstd"),
                       bootci = "normal", bootrepeats = 200)$conf.int$std.error
  se_normal <- unname(se_normal)
  expect_equal(se_bcapar, se_normal, tolerance = 0.5)
})

context("bootci types")

data(breastcancer)
dat <- breastcancer

test_that("bootci types give similar tidy results", {
  set.seed(123)
  coe_bcapar <- confint(riskratio(formula = death ~ stage + receptor,
                                  data = dat, approach = "margstd_boot"),
                        bootci = "bca", bootrepeats = 500)
  set.seed(123)
  coe_normal <- confint(riskratio(formula = death ~ stage + receptor,
                                  data = dat, approach = "margstd_boot"),
                        bootci = "normal", bootrepeats = 200)
  set.seed(123)
  coe_nonpar <- confint(riskratio(formula = death ~ stage + receptor,
                                  data = dat, approach = "margstd_boot"),
                        bootci = "nonpar", bootrepeats = 500)

  expect_equal(coe_bcapar, coe_normal, tolerance = 0.5)
  expect_equal(coe_bcapar, coe_nonpar, tolerance = 0.1)
})


test_that("bootci types give similar SE in summary", {
  set.seed(123)
  se_bcapar <- summary(riskratio(formula = death ~ stage + receptor,
                                 data = dat, approach = "margstd_boot"),
                       bootci = "bca", bootrepeats = 500)$conf.int$std.error
  se_bcapar <- unname(se_bcapar)
  set.seed(123)
  se_normal <- summary(riskratio(formula = death ~ stage + receptor,
                                 data = dat, approach = "margstd_boot"),
                       bootci = "normal", bootrepeats = 200)$conf.int$std.error
  se_normal <- unname(se_normal)
  set.seed(123)
  se_nonpar <- summary(riskratio(formula = death ~ stage + receptor,
                                 data = dat, approach = "margstd_boot"),
                       bootci = "nonpar", bootrepeats = 500)$conf.int$std.error
  se_nonpar <- unname(se_nonpar)
  expect_equal(se_bcapar, se_normal, tolerance = 0.5)
  expect_equal(se_bcapar, se_nonpar, tolerance = 0.5)

  set.seed(123)
  se_bcapar <- summary(riskdiff(formula = death ~ stage + receptor,
                                data = dat, approach = "margstd_boot"),
                       bootci = "bca", bootrepeats = 500)$conf.int$std.error
  se_bcapar <- unname(se_bcapar)
  set.seed(123)
  se_normal <- summary(riskdiff(formula = death ~ stage + receptor,
                                data = dat, approach = "margstd_boot"),
                       bootci = "normal", bootrepeats = 200)$conf.int$std.error
  se_normal <- unname(se_normal)
  expect_equal(se_bcapar, se_normal, tolerance = 0.5)
})

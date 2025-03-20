context("confint() limits")

test_that("confint has the same results as tidy", {
  conf_low_tidy <- function(approach) {
    set.seed(123)
    broom::tidy(riskratio(
      formula = death ~ stage + receptor,
      data = breastcancer,
      approach = approach)) |>
      dplyr::filter(term == "stageStage II") |>
      dplyr::pull("conf.low") |>
      purrr::pluck(1)
  }
  conf_low_confint <- function(approach) {
    set.seed(123)
    confint(riskratio(
      formula = death ~ stage + receptor,
      data = breastcancer,
      approach = approach)) |>
      tibble::as_tibble(rownames = "term") |>
      dplyr::filter(term == "stageStage II") |>
      dplyr::select(`2.5 %`) |>
      purrr::pluck(1)
  }

  expect_equal(
    object = conf_low_tidy(approach = "auto"),
    expected = conf_low_confint(approach = "auto"))
  expect_equal(
    object = conf_low_tidy(approach = "robpoisson"),
    expected = conf_low_confint(approach = "robpoisson"))
  expect_equal(
    object = conf_low_tidy(approach = "duplicate"),
    expected = conf_low_confint(approach = "duplicate"))
  expect_equal(
    object = conf_low_tidy(approach = "glm_startp"),
    expected = conf_low_confint(approach = "glm_startp"))
  expect_equal(
    object = conf_low_tidy(approach = "glm_startd"),
    expected = conf_low_confint(approach = "glm_startd"))
  expect_equal(
    object = conf_low_tidy(approach = "margstd_boot"),
    expected = conf_low_confint(approach = "margstd_boot"))
  expect_equal(
    object = conf_low_tidy(approach = "margstd_delta"),
    expected = conf_low_confint(approach = "margstd_delta"))
  expect_equal(
    object = conf_low_tidy(approach = "logistic"),
    expected = conf_low_confint(approach = "logistic"))
  expect_equal(
    object = conf_low_tidy(approach = "legacy"),
    expected = conf_low_confint(approach = "legacy"))
  if(requireNamespace("logbin", quietly = TRUE)) {
    expect_equal(
      object = conf_low_tidy(approach = "glm_cem"),
      expected = conf_low_confint(approach = "glm_cem"))
    expect_equal(
      object = conf_low_tidy(approach = "glm_cem_startp"),
      expected = conf_low_confint(approach = "glm_cem_startp"))
  }
})

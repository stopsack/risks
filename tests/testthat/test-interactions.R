context("interactions")

df <- breastcancer
df$rand <- runif(n = 192, min = 0, max = 1)
df$stage_colon = df$stage
levels(df$stage_colon) <- c("1: Stage I", "2: Stage II", "3: Stage III")

test_that("exposure interactions get picked up", {
  # No interaction
  expect_false(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ stage + receptor,
        data = df,
        family = binomial()))$interaction)

  # Interaction, but not with exposure
  expect_false(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ stage + receptor * rand,
        data = df,
        family = binomial()))$interaction)

  # Classic case
  expect_true(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ stage * receptor,
        data = df,
        family = binomial()))$interaction)

  # Data editing in formula
  expect_true(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ as.character(stage) * receptor,
        data = df,
        family = binomial()))$interaction)

  # User-selected exposure variable, listed second
  expect_true(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ stage * receptor,
        data = df,
        family = binomial()),
      variable = "receptor")$interaction)

  # Interactions listed later
  expect_true(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ stage + receptor + stage * rand,
        data = df,
        family = binomial()))$interaction)

  expect_true(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ stage + receptor + rand * stage,
        data = df,
        family = binomial()))$interaction)

  # Three-way interactions
  expect_true(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ stage + receptor + stage * rand * receptor,
        data = df,
        family = binomial()))$interaction)

  # Exposure with ":" in level names
  expect_false(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ stage_colon + receptor,
        data = df,
        family = binomial()))$interaction)

  expect_true(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ stage_colon * receptor,
        data = df,
        family = binomial()))$interaction)

  expect_true(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ as.factor(stage_colon) * receptor,
        data = df,
        family = binomial()))$interaction)

  expect_true(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ stage_colon * receptor,
        data = df,
        family = binomial()),
      variable = "receptor")$interaction)

  # Exposure with ":" in levels, only some levels selected for standardization
  expect_false(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ stage_colon + receptor,
        data = df,
        family = binomial()),
      at = c("1: Stage I", "3: Stage III"))$interaction)

  expect_true(
    find_margstd_exposure(
      fit = glm(
        formula = death ~ stage_colon * receptor,
        data = df,
        family = binomial()),
      at = c("1: Stage I", "3: Stage III"))$interaction)
})

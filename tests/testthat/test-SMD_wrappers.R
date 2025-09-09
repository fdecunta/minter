

test_that("SMD function works for 'ind' Cohen's d", {
  # Use same data from the Cohen's d test above
  df <- data.frame(
    Ctrl_mean = 2.5,
    Ctrl_sd = 1.5,
    Ctrl_n = 10,
    A_mean = 4,
    A_sd = 4.3,
    A_n = 10
  )

  # Computed with metafor::escalc
  test_SMD <- 0.4658026
  test_SMDv <- 0.2054243

  res <- SMD_ind(
    data = df,
    append = FALSE,
    col_names = c("simple_SMD", "simple_SMDv"),
    hedges_correction = FALSE,
    Ctrl_mean = Ctrl_mean,
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    A_mean = A_mean,
    A_sd = A_sd,
    A_n = A_n
  )

  expect_equal(res$simple_SMD, test_SMD, tolerance = 1e-6)
  expect_equal(res$simple_SMDv, test_SMDv, tolerance = 1e-6)
})


test_that("SMD function works for 'ind' Hedges' g", {
  # Use same data from the Hedges' g test above
  df <- data.frame(
    Ctrl_mean = 2.5,
    Ctrl_sd = 1.5,
    Ctrl_n = 10,
    A_mean = 4,
    A_sd = 4.3,
    A_n = 10
  )

  # Computed with metafor::escalc
  test_SMD <- 0.44601
  test_SMDv <- 0.20497

  res <- SMD_ind(
    data = df,
    append = FALSE,
    col_names = c("simple_SMD", "simple_SMDv"),
    hedges_correction = TRUE,
    Ctrl_mean = Ctrl_mean,
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    A_mean = A_mean,
    A_sd = A_sd,
    A_n = A_n
  )

  expect_equal(res$simple_SMD, test_SMD, tolerance = 1e-3)
  expect_equal(res$simple_SMDv, test_SMDv, tolerance = 1e-3)
})


test_that("SMD function computes fine the main Cohen's d", {
  # This computes the main SMD for Factor A
  res_A <- SMD_main(
    data = testing_data,
    col_names = c("main_SMD", "main_SMDv"),
    hedges_correction = FALSE,
    Ctrl_mean = C_mean,
    Ctrl_sd = C_sd,
    Ctrl_n = C_n,
    A_mean = A_mean,
    A_sd = A_sd,
    A_n = A_n,
    B_mean = B_mean,
    B_sd = B_sd,
    B_n = B_n,
    AB_mean = AB_mean,
    AB_sd = AB_sd,
    AB_n = AB_n
  )

  expect_equal(res_A$main_SMD, testing_data$A_main_SMD, tolerance = 1e-6)
  expect_equal(res_A$main_SMDv, testing_data$A_main_SMDv, tolerance = 1e-6)
})


test_that("SMD function computes fine the interaction Cohen's d", {
  # This computes the main SMD for Factor A
  res <- SMD_inter(
    data = testing_data,
    col_names = c("inter_SMD", "inter_SMDv"),
    hedges_correction = FALSE,
    Ctrl_mean = C_mean,
    Ctrl_sd = C_sd,
    Ctrl_n = C_n,
    A_mean = A_mean,
    A_sd = A_sd,
    A_n = A_n,
    B_mean = B_mean,
    B_sd = B_sd,
    B_n = B_n,
    AB_mean = AB_mean,
    AB_sd = AB_sd,
    AB_n = AB_n
  )

  expect_equal(res$inter_SMD, testing_data$AB_main_SMD, tolerance = 1e-6)
  expect_equal(res$inter_SMDv, testing_data$AB_main_SMDv, tolerance = 1e-6)
})


test_that("main_SMD works fine without Hedge's correction (Cohen's d)", {
  foo_data <- data.frame(
      Ctrl_mean = c(10),
      Ctrl_sd = c(5),
      Ctrl_n = c(10),
      A_mean = c(12),
      A_sd = c(5),
      A_n = c(9),
      B_mean = c(6),
      B_sd = c(4),
      B_n = c(10),
      AB_mean = c(12),
      AB_sd = c(5),
      AB_n = c(10)
  )

  res <- SMD_main(
    data = foo_data,
    hedges_correction = FALSE,
    Ctrl_mean = "Ctrl_mean",
    Ctrl_sd = "Ctrl_sd",
    Ctrl_n = "Ctrl_n",
    A_mean = "A_mean",
    A_sd = "A_sd",
    A_n = "A_n",
    B_mean = "B_mean",
    B_sd = "B_sd",
    B_n = "B_n",
    AB_mean = "AB_mean",
    AB_sd = "AB_sd",
    AB_n = "AB_n"
  )

  yi_expected <- 0.8398153
  vi_expected <- 0.1050383

  expect_equal(res$yi, yi_expected, tolerance = 1e-6)
  expect_equal(res$vi, vi_expected, tolerance = 1e-6)
})


test_that("SMD_main works fine with Hedge's correction", {
  foo_data <- data.frame(
      Ctrl_mean = c(10),
      Ctrl_sd = c(5),
      Ctrl_n = c(10),
      A_mean = c(12),
      A_sd = c(5),
      A_n = c(9),
      B_mean = c(6),
      B_sd = c(4),
      B_n = c(10),
      AB_mean = c(12),
      AB_sd = c(5),
      AB_n = c(10)
  )

  res <- SMD_main(
    data = foo_data,
    hedges_correction = TRUE,
    Ctrl_mean = "Ctrl_mean",
    Ctrl_sd = "Ctrl_sd",
    Ctrl_n = "Ctrl_n",
    A_mean = "A_mean",
    A_sd = "A_sd",
    A_n = "A_n",
    B_mean = "B_mean",
    B_sd = "B_sd",
    B_n = "B_n",
    AB_mean = "AB_mean",
    AB_sd = "AB_sd",
    AB_n = "AB_n"
  )

  yi_expected <- 0.8216898
  vi_expected <- 0.1049418

  expect_equal(res$yi, yi_expected, tolerance = 1e-6)
  expect_equal(res$vi, vi_expected, tolerance = 1e-6)
})


test_that("SMD_inter works fine with Hedge's correction", {
  foo_data <- data.frame(
      Ctrl_mean = c(10),
      Ctrl_sd = c(5),
      Ctrl_n = c(10),
      A_mean = c(12),
      A_sd = c(5),
      A_n = c(9),
      B_mean = c(6),
      B_sd = c(4),
      B_n = c(10),
      AB_mean = c(12),
      AB_sd = c(5),
      AB_n = c(10)
  )

  res <- SMD_inter(
    data = foo_data,
    hedges_correction = TRUE,
    Ctrl_mean = "Ctrl_mean",
    Ctrl_sd = "Ctrl_sd",
    Ctrl_n = "Ctrl_n",
    A_mean = "A_mean",
    A_sd = "A_sd",
    A_n = "A_n",
    B_mean = "B_mean",
    B_sd = "B_sd",
    B_n = "B_n",
    AB_mean = "AB_mean",
    AB_sd = "AB_sd",
    AB_n = "AB_n"
  )

  yi_expected <- 0.8216898
  vi_expected <- 0.4197672

  expect_equal(res$yi, yi_expected, tolerance = 1e-6)
  expect_equal(res$vi, vi_expected, tolerance = 1e-6)
})

test_that("lnVR works fine for 'ind' effect size", {
  C_sd <- 1.5
  C_n <- 10
  A_sd <- 2.5
  A_n <- 10

  df <- data.frame(
    C_sd = C_sd,
    C_n = C_n,
    A_sd = A_sd,
    A_n = A_n
  )

  test_lnVR <- log(A_sd / C_sd) + (1 / (2 * (A_n - 1))) - (1 / (2 * (C_n - 1)))
  test_lnVRv <- (1 / (2 * (A_n - 1))) + (1 / (2 * (C_n - 1)))

  res <- lnVR_ind(
    data = df,
    col_names = c("simple_lnVR", "simple_lnVRv"),
    Ctrl_sd = C_sd, 
    Ctrl_n = C_n,
    A_sd = A_sd,
    A_n = A_n
  )

  expect_equal(res$simple_lnVR, test_lnVR, tolerance = 1e-6)
  expect_equal(res$simple_lnVRv, test_lnVRv, tolerance = 1e-6)
})


test_that("lnVR() works fine with 'main'", {
  df <- data.frame(
    Ctrl_sd = 1.5,
    Ctrl_n = 10,
    A_sd = 2.5,
    A_n = 10,
    B_sd = 3,
    B_n = 10,
    AB_sd = 4,
    AB_n = 10
  )

  # Main effect for factor A. Computed by hand
  main_lnVR <- 0.39925
  main_lnVRv <- 0.05556

  res <- lnVR_main(
    data = df,
    col_names = c("main_lnVR", "main_lnVRv"),
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    A_sd = A_sd,
    A_n = A_n,
    B_sd = B_sd,
    B_n = B_n,
    AB_sd = AB_sd,
    AB_n = AB_n
  )

  expect_equal(res$main_lnVR, main_lnVR, tolerance = 1e-4)
  expect_equal(res$main_lnVRv, main_lnVRv, tolerance = 1e-4)
})


test_that("lnVR works fine with 'inter'", {
  df <- data.frame(
    Ctrl_sd = 1.5,
    Ctrl_n = 10,
    A_sd = 2.5,
    A_n = 10,
    B_sd = 3,
    B_n = 10,
    AB_sd = 4,
    AB_n = 10
  )

  # Main effect for factor A. Computed by hand
  inter_lnVR <- -0.22314
  inter_lnVRv <- 0.22222

  res <- lnVR_inter(
    data = df,
    col_names = c("inter_lnVR", "inter_lnVRv"),
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    A_sd = A_sd,
    A_n = A_n,
    B_sd = B_sd,
    B_n = B_n,
    AB_sd = AB_sd,
    AB_n = AB_n
  )

  expect_equal(res$inter_lnVR, inter_lnVR, tolerance = 1e-4)
  expect_equal(res$inter_lnVRv, inter_lnVRv, tolerance = 1e-4)
})

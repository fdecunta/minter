test_that("lnCVR works fine with 'ind' effects", {
  df <- data.frame(
    Ctrl_mean = 2.5,
    Ctrl_sd = 1.5,
    Ctrl_n = 10,
    A_mean = 4,
    A_sd = 4.3,
    A_n = 10
  )

  # Computed manually
  test_lnCVR <- 0.5831463
  test_lnCVRv <- 0.2626736

  # Compute with function
  res <- lnCVR_ind(
    data = df,
    col_names = c("simple_lnCVR", "simple_lnCVRv"),
    Ctrl_mean = Ctrl_mean,
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    A_mean = A_mean,
    A_sd = A_sd,
    A_n = A_n
  )

  expect_equal(res$simple_lnCVR, test_lnCVR, tolerance = 1e-6)
  expect_equal(res$simple_lnCVRv, test_lnCVRv, tolerance = 1e-6)
})


test_that("lnCVR works fine with 'main' effects", {
  df <- data.frame(
    Ctrl_mean = 2,
    Ctrl_sd = 2,
    Ctrl_n = 10,
    A_mean = 4,
    A_sd = 4,
    A_n = 10,
    B_mean = 8,
    B_sd = 1,
    B_n = 10,
    AB_mean = 6,
    AB_sd = 3,
    AB_n = 10
  )

  # Computed manually
  test_lnCVR <- 0.69314718

  # NOTE: The sampling variance for lnCVR is calculated as the sum
  # of the sampling variances of lnRR and lnVR.
  # Here, the sampling var of lnRR is calculated using Morris' method,
  # but can use Nakagawa's as well
  test_lnCVRv <- 0.0855555

  # Compute with function
  res <- lnCVR_main(
    data = df,
    col_names = c("main_lnCVR", "main_lnCVRv"),
    Ctrl_mean = Ctrl_mean,
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
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

  expect_equal(res$main_lnCVR, test_lnCVR, tolerance = 1e-6)
  expect_equal(res$main_lnCVRv, test_lnCVRv, tolerance = 1e-6)
})


test_that("lnCVR works fine with 'inter' effects", {
  df <- data.frame(
    Ctrl_mean = 2,
    Ctrl_sd = 2,
    Ctrl_n = 10,
    A_mean = 4,
    A_sd = 4,
    A_n = 10,
    B_mean = 8,
    B_sd = 1,
    B_n = 10,
    AB_mean = 6,
    AB_sd = 3,
    AB_n = 10
  )

  # Computed manually
  test_lnCVR <- 1.38629436
  test_lnCVRv <- 0.448784722

  # Compute with function
  res <- lnCVR_inter(
    data = df,
    col_names = c("inter_lnCVR", "inter_lnCVRv"),
    Ctrl_mean = Ctrl_mean,
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
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

  expect_equal(res$inter_lnCVR, test_lnCVR, tolerance = 1e-6)
  expect_equal(res$inter_lnCVRv, test_lnCVRv, tolerance = 1e-6)
})

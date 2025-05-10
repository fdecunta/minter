test_that("Simple lnCVR is computed correctly", {
  Ctrl_mean <- 2.5
  Ctrl_sd <- 1.5
  Ctrl_n <- 10
  X_mean <- 4
  X_sd <- 4.3
  X_n <- 10

  # Computed manually
  test_lnCVR <- 0.5831463
  test_lnCVRv <- 0.2626736

  # Compute with function
  res <- .simple_lnCVR(
    Ctrl_mean = Ctrl_mean,
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    X_mean = X_mean,
    X_sd = X_sd,
    X_n = X_n
  )

  expect_equal(res$simple_lnCVR, test_lnCVR, tolerance = 1e-6)
  expect_equal(res$simple_lnCVRv, test_lnCVRv, tolerance = 1e-6)
})


test_that("The simple lnVR is computed correctly", {
  C_sd <- 1.5
  C_n <- 10
  X_sd <- 2.5
  X_n <- 10

  test_lnVR <- log(X_sd / C_sd) + (1 / (2 * (X_n - 1))) - (1 / (2 * (C_n - 1)))
  test_lnVRv <- (1 / (2 * (X_n - 1))) + (1 / (2 * (C_n - 1)))

  res <- .simple_lnVR(
    Ctrl_sd = C_sd, 
    Ctrl_n = C_n,
    X_sd = X_sd,
    X_n = X_n
  )

  expect_equal(res$simple_lnVR, test_lnVR, tolerance = 1e-6)
  expect_equal(res$simple_lnVRv, test_lnVRv, tolerance = 1e-6)
})

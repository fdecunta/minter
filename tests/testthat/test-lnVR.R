
test_that("The simple lnVR is computed correctly", {
  C_sd <- 1.5
  C_n <- 10
  A_sd <- 2.5
  A_n <- 10

  test_lnVR <- log(A_sd / C_sd) + (1 / (2 * (A_n - 1))) - (1 / (2 * (C_n - 1)))
  test_lnVRv <- (1 / (2 * (A_n - 1))) + (1 / (2 * (C_n - 1)))

  res <- .simple_lnVR(
    Ctrl_sd = C_sd, 
    Ctrl_n = C_n,
    A_sd = A_sd,
    A_n = A_n
  )

  expect_equal(res$simple_lnVR, test_lnVR, tolerance = 1e-6)
  expect_equal(res$simple_lnVRv, test_lnVRv, tolerance = 1e-6)
})

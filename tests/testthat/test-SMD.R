test_that("Pooled sampling variance is computed correctly", {
  test_S <- 8.276473   # Manually calculated

  res_S <- .pooled_sd(
    Ctrl_sd = 5,
    Ctrl_n = 5,
    A_sd = 10,
    A_n = 5,
    B_sd = 10,
    B_n = 5,
    AB_sd = 7,
    AB_n = 5
  )

  expect_equal(res_S, test_S, tolerance = 1e-6)
})

test_that(".j_correction is working fine", {
  test_j <- 0.9923  # From page 227 
  res_j <- .j_correction(50 + 50 - 2)

  expect_equal(res_j, test_j, tolerance = 1e-4)
})

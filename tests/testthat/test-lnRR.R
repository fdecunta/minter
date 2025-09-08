data(testing_data)

test_that("Simple lnRR is correctly computed", {
  res <- with(testing_data, .simple_lnRR(
    Ctrl_mean = C_mean,
    Ctrl_sd = C_sd,
    Ctrl_n = C_n,
    A_mean = A_mean,
    A_sd = A_sd,
    A_n = A_n
  ))

  test_lnRR <- with(testing_data, log(A_mean/C_mean))

  test_lnRRv <- with(testing_data,
    ((C_sd^2) / (C_n * C_mean^2)) + 
    ((A_sd^2) / (A_n * A_mean^2))
  )

  expect_equal(res$simple_lnRR, test_lnRR, tolerance = 1e-6)
  expect_equal(res$simple_lnRRv, test_lnRRv, tolerance = 1e-6)
})


test_that("The main lnRR is correctly computed with Morris method", {
  res <- with(testing_data, .main_lnRR_Morris(
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
  ))

  expect_equal(res$main_lnRR, testing_data$A_main_lnRR, tolerance = 1e-6)
  expect_equal(res$main_lnRRv, testing_data$A_main_lnRRv, tolerance = 1e-6)
})


test_that("The main lnRR using Nakagawa's method is correctly computed", {
  res <- .main_lnRR_Nakagawa(
    Ctrl_mean = 10,
    Ctrl_sd = 2,
    Ctrl_n = 10,
    A_mean = 12,
    A_sd = 2.5,
    A_n = 10,
    B_mean = 8,
    B_sd = 1.5,
    B_n = 10,
    AB_mean = 15,
    AB_sd = 3,
    AB_n = 10
  )

  # Manually computed for testing
  test_lnRR <- 0.405465108
  test_lnRRv <- 0.003963975

  expect_equal(res$main_lnRR, test_lnRR, tolerance = 1e-6)
  expect_equal(res$main_lnRRv, test_lnRRv, tolerance = 1e-6)
})


test_that("The interaction lnRR is correctly computed", {
  res <- with(testing_data, .interaction_lnRR(
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
  ))

  expect_equal(res$inter_lnRR, testing_data$AB_main_lnRR, tolerance = 1e-6)
  expect_equal(res$inter_lnRRv, testing_data$AB_main_lnRRv, tolerance = 1e-6)
})

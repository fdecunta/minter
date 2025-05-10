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

test_that("The main lnRR is correctly computed", {
  res <- with(testing_data, .main_lnRR(
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

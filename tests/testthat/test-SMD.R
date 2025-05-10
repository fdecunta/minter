data(testing_data)

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


test_that("Main SMD (Cohen's d) is correctly computed", {
  pooled_S <- with(testing_data, .pooled_sd(
    Ctrl_sd = C_sd,
    Ctrl_n = C_n,
    A_sd = A_sd,
    A_n = A_n,
    B_sd = B_sd,
    B_n = B_n,
    AB_sd = AB_sd,
    AB_n = AB_n
  ))

  # This computes the main SMD for Factor A
  res_A <- with(testing_data, .main_SMD(
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
    AB_n = AB_n,
    pooled_sd = pooled_S,
    hedges_correction = FALSE
  ))

  expect_equal(res_A$main_SMD, testing_data$A_main_SMD, tolerance = 1e-6)
  expect_equal(res_A$main_SMDv, testing_data$A_main_SMDv, tolerance = 1e-6)
})



test_that("The interaction SMD (Cohen's d) is correctly computed", {
  pooled_S <- with(testing_data, .pooled_sd(
    Ctrl_sd = C_sd,
    Ctrl_n = C_n,
    A_sd = A_sd,
    A_n = A_n,
    B_sd = B_sd,
    B_n = B_n,
    AB_sd = AB_sd,
    AB_n = AB_n
  ))

  # This computes the main SMD for Factor A
  res_A <- with(testing_data, .main_SMD(
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
    AB_n = AB_n,
    pooled_sd = pooled_S,
    hedges_correction = FALSE
  ))

  expect_equal(res_A$main_SMD, testing_data$A_main_SMD, tolerance = 1e-6)
  expect_equal(res_A$main_SMDv, testing_data$A_main_SMDv, tolerance = 1e-6)
})

test_that("Simple lnCVR is computed correctly", {
  Ctrl_mean <- 2.5
  Ctrl_sd <- 1.5
  Ctrl_n <- 10
  A_mean <- 4
  A_sd <- 4.3
  A_n <- 10

  # Computed manually
  test_lnCVR <- 0.5831463
  test_lnCVRv <- 0.2626736

  # Compute with function
  res <- .simple_lnCVR(
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


test_that("The main lnCVR is computed correctly", {
  df <- data.frame(
    Ctrl_mean = 10,
    Ctrl_sd = 2.0,
    Ctrl_n = 12,
    A_mean = 14,
    A_sd = 2.8,
    A_n = 12,
    B_mean = 12,
    B_sd = 2.4,
    B_n = 12,
    AB_mean = 20,
    AB_sd = 3.6,
    AB_n = 12
  )

  test_main_lnCVR <- -0.05268
  test_main_lnCVRv <- 0.04863

  res <- with(df, .main_lnCVR(
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
  ))
 
  expect_equal(res$main_lnCVR, test_main_lnCVR, tolerance = 1e-4)
  expect_equal(res$main_lnCVRv, test_main_lnCVRv, tolerance = 1e-4)
})


test_that("The interaciont lnCVR is computed correctly", {
  df <- data.frame(
    Ctrl_mean = 10,
    Ctrl_sd = 2.0,
    Ctrl_n = 12,
    A_mean = 14,
    A_sd = 2.8,
    A_n = 12,
    B_mean = 12,
    B_sd = 2.4,
    B_n = 12,
    AB_mean = 20,
    AB_sd = 3.6,
    AB_n = 12
  )

  test_inter_lnCVR <- -0.10536
  test_inter_lnCVRv <- 0.19451

  res <- with(df, .interaction_lnCVR(
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
  ))
 
  expect_equal(res$inter_lnCVR, test_inter_lnCVR, tolerance = 1e-4)
  expect_equal(res$inter_lnCVRv, test_inter_lnCVRv, tolerance = 1e-4)
})


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
  res <- lnCVR(
    type = "ind",
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
  res <- lnCVR(
    type = "main",
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
  res <- lnCVR(
    type = "inter",
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

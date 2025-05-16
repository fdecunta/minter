test_that(".time_interaction_lnRR works fine computing the interaction and its sampling variance", {
  res <- .time_interaction_lnRR(
    t0_Ctrl_mean = 10,
    t0_Ctrl_sd = 2,
    t1_Ctrl_mean = 5,
    t1_Ctrl_sd = 1,
    Ctrl_n = 10,
    Ctrl_cor = 0.5,
    t0_Exp_mean = 10,
    t0_Exp_sd = 2,
    t1_Exp_mean = 9,
    t1_Exp_sd = 2,
    Exp_n = 9,
    Exp_cor = 0.5
  )

  # Computed manually
  test_lnRR <- 0.5877866
  test_lnRRv <- 0.008993141

  expect_equal(res$lnRR, test_lnRR, tolerance = 1e-6)
  expect_equal(res$lnRRv, test_lnRRv, tolerance = 1e-6)
})


test_that("time_lnRR works fine", {
  df <- data.frame(
    t0_Cm = 10,
    t0_Csd = 2,
    t1_Cm = 5,
    t1_Csd = 1,
    Cn = 10,
    t0_Em = 10,
    t0_Esd = 2,
    t1_Em = 9,
    t1_Esd = 2,
    En = 9
  )

  res <- time_lnRR(
    data = df,
    t0_Ctrl_mean = t0_Cm,
    t0_Ctrl_sd = t0_Csd,
    t1_Ctrl_mean = t1_Cm,
    t1_Ctrl_sd = t1_Csd,
    Ctrl_n = Cn,
    Ctrl_cor = 0.5,
    t0_Exp_mean = t0_Em,
    t0_Exp_sd = t0_Esd,
    t1_Exp_mean = t1_Em,
    t1_Exp_sd = t1_Esd,
    Exp_n = En,
    Exp_cor = 0.5
  )

  # Computed manually
  test_lnRR <- 0.5877866
  test_lnRRv <- 0.008993141

  expect_equal(res$yi, test_lnRR, tolerance = 1e-6)
  expect_equal(res$vi, test_lnRRv, tolerance = 1e-6)

})

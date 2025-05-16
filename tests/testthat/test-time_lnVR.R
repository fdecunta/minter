test_that(".time_interaction_lnVR is working fine", {
  res <- .time_interaction_lnVR(
    t0_Ctrl_sd = 2,
    t1_Ctrl_sd = 1,
    Ctrl_n = 10,
    Ctrl_cor = 0.5,
    t0_Exp_sd = 7,
    t1_Exp_sd = 10,
    Exp_n = 8,
    Exp_cor = 0.5
  )

  # Calculated manually
  test_lnVR <- 1.049822124
  test_lnVRv <- 0.19047619

  expect_equal(res$lnVR, test_lnVR, tolerance = 1e-6)
  expect_equal(res$lnVRv, test_lnVRv, tolerance = 1e-6)
})


test_that(".time__lnVR is working fine", {
  df <- data.frame(
    t0_Ctrl_sd = 2,
    t1_Ctrl_sd = 1,
    Ctrl_n = 10,
    t0_Exp_sd = 7,
    t1_Exp_sd = 10,
    Exp_n = 8
  )

  res <- time_lnVR(
    data = df,
    t0_Ctrl_sd = t0_Ctrl_sd,
    t1_Ctrl_sd = t1_Ctrl_sd,
    Ctrl_n = Ctrl_n,
    Ctrl_cor = 0.5,
    t0_Exp_sd = t0_Exp_sd,
    t1_Exp_sd = t1_Exp_sd,
    Exp_n = Exp_n,
    Exp_cor = 0.5
  )

  # Calculated manually
  test_lnVR <- 1.049822124
  test_lnVRv <- 0.19047619

  expect_equal(res$yi, test_lnVR, tolerance = 1e-6)
  expect_equal(res$vi, test_lnVRv, tolerance = 1e-6)
})

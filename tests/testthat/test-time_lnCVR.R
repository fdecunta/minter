test_that(".time_interaction_lnCVR works fine", {
  res <- .time_interaction_lnCVR(
    t0_Ctrl_mean = 10,
    t0_Ctrl_sd = 5,
    t1_Ctrl_mean = 14,
    t1_Ctrl_sd = 7,
    Ctrl_n = 10,
    Ctrl_cor = 0.5,
    t0_Exp_mean = 20,
    t0_Exp_sd = 5,
    t1_Exp_mean = 25,
    t1_Exp_sd = 2,
    Exp_n = 10,
    Exp_cor = 0.5
  )

  test_lnCVR <- -1.139434283
#  test_lnCVRv <- TODO:

  expect_equal(res$lnCVR, test_lnCVR, tolerance = 1e-6)
#  expect_equal(res$lnCVRv, test_lnCVRv, tolerance = 1e-6)
})


test_that(".time_interaction_lnCVR works fine", {
  df <- data.frame(
    t0_Ctrl_mean = 10,
    t0_Ctrl_sd = 5,
    t1_Ctrl_mean = 14,
    t1_Ctrl_sd = 7,
    Ctrl_n = 10,
    t0_Exp_mean = 20,
    t0_Exp_sd = 5,
    t1_Exp_mean = 25,
    t1_Exp_sd = 2,
    Exp_n = 10
  )

  res <- time_lnCVR(
    data = df,
    t0_Ctrl_mean = t0_Ctrl_mean,
    t0_Ctrl_sd = t0_Ctrl_sd,
    t1_Ctrl_mean = t1_Ctrl_mean,
    t1_Ctrl_sd = t1_Ctrl_sd,
    Ctrl_n = Ctrl_n,
    Ctrl_cor = 0.5,
    t0_Exp_mean = t0_Exp_mean,
    t0_Exp_sd = t0_Exp_sd,
    t1_Exp_mean = t1_Exp_mean,
    t1_Exp_sd = t1_Exp_sd,
    Exp_n = Exp_n,
    Exp_cor = 0.5
  )

  test_lnCVR <- -1.139434283
#  test_lnCVRv <- TODO:

  expect_equal(res$yi, test_lnCVR, tolerance = 1e-6)
#  expect_equal(res$lnCVRv, test_lnCVRv, tolerance = 1e-6)
})

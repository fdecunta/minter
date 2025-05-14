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

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

test_that("The pooled standard deviation for the factor X time SMD is working fine", {
  pooled_sd <- .time_pooled_sd(
    t0_Ctrl_sd = 5,
    t1_Ctrl_sd = 6,
    Ctrl_n = 5,
    t0_Exp_sd = 2,
    t1_Exp_sd = 4,
    Exp_n = 5
  )
  
  # Computed manually
  test_pooled_sd <- 4.5

  expect_equal(pooled_sd, test_pooled_sd)
})


test_that("SMD of the interaction between Experimental Treatment and Time is working fine", {
  res <- .time_interaction_SMD(
    t0_Ctrl_mean = 10,
    t0_Ctrl_sd = 5, 
    t1_Ctrl_mean = 12,
    t1_Ctrl_sd = 6,
    Ctrl_n = 5,
    Ctrl_cor = 0.5,
    t0_Exp_mean = 12,
    t0_Exp_sd = 2,
    t1_Exp_mean = 20,
    t1_Exp_sd = 4,
    Exp_n = 5,
    Exp_cor = 0.5,
    hedges_correction = FALSE
  )

  # Computed manually 
  test_SMD <- 1.333333
  test_SMDv <- 0.488889

  expect_equal(res$yi, test_SMD, tolerance = 1e-6)
  expect_equal(res$vi, test_SMDv, tolerance = 1e-6)
})

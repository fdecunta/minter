# Fake data with correct effect sizes calculated
fdata_test <- readRDS(testthat::test_path("test-data/fake_data_test.rds"))

# Fake data to compute lnRR
data(fake_data)
fdata <- fake_data

fdata <- factorial_effsize(
  effsize = "smd",
  colnames = c("Herb", "Fert"),
  data = fdata,
  Ctrl_mean = C_mean,
  Ctrl_sd = C_sd,
  Ctrl_n = C_n,
  A_mean = Herb_mean,
  A_sd = Herb_sd,
  A_n = Herb_n,
  B_mean = Fert_mean,
  B_sd = Fert_sd,
  B_n = Fert_n,
  AB_mean = HxF_mean,
  AB_sd = HxF_sd,
  AB_n = HxF_n
)

test_that(".j_correction is working fine", {
  # Compare results to The Handbook of Research Synthesis and Meta-Analysis
  test_j <- 0.9923  # From page 227 
  res_j <- .j_correction(50 + 50 - 2)

  expect_equal(res_j, test_j, tolerance = 1e-4)
})


# TODO: More tests for SMD

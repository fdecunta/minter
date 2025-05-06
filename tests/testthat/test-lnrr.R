# Fake data with correct effect sizes calculated
fdata_test <- readRDS(testthat::test_path("test-data/fake_data_test.rds"))

# Fake data to compute lnRR
data(fake_data)
fdata <- fake_data

fdata <- factorial_effsize(
  effsize = "lnrr",
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

test_that("Simple lnRR is working fine", {
  attributes(fdata_test$Fert_simple_lnRR) <- NULL  # Remove anoying metafor attributes
  expect_equal(
	       fdata$Fert_simple_lnRR,
               fdata_test$Fert_simple_lnRR, 
	       tolerance = 1e-8)

  attributes(fdata_test$Herb_simple_lnRR) <- NULL  # Remove anoying metafor attributes
  expect_equal(
	       fdata$Herb_simple_lnRR,
               fdata_test$Herb_simple_lnRR, 
	       tolerance = 1e-8)
})


test_that("Simple lnRR sampling variance is working fine", {
  attributes(fdata_test$Fert_simple_lnRR_var) <- NULL  # Remove anoying metafor attributes
  expect_equal(
	       fdata$Fert_simple_lnRR_var,
               fdata_test$Fert_simple_lnRR_var, 
	       tolerance = 1e-8)

  attributes(fdata_test$Herb_simple_lnRR_var) <- NULL  # Remove anoying metafor attributes
  expect_equal(
	       fdata$Herb_simple_lnRR_var,
               fdata_test$Herb_simple_lnRR_var, 
	       tolerance = 1e-8)
})


test_that("Overall lnRR is working fine", {
  expect_equal(
	       fdata$Fert_overall_lnRR,
	       fdata_test$Fert_overall_lnRR,
	       tolerance = 1e-8)

  expect_equal(
	       fdata$Herb_overall_lnRR,
	       fdata_test$Herb_overall_lnRR,
	       tolerance = 1e-8)
})


test_that("Overall lnRR sampling variance is working fine", {
  expect_equal(
	       fdata$Herb_overall_lnRR_var,
	       fdata_test$Herb_overall_lnRR_var,
	       tolerance = 1e-8)
})


test_that("Interaction lnRR is working fine", {
  expect_equal(
	       fdata$Herb_x_Fert_lnRR,
	       fdata_test$Herb_x_Fert_lnRR,
	       tolerance = 1e-8)
})


test_that("Interaction lnRR sampling variance is working fine", {
  expect_equal(
	       fdata$Herb_x_Fert_lnRR_var,
	       fdata_test$Herb_x_Fert_lnRR_var,
	       tolerance = 1e-8)
})

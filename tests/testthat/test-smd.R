# Fake data with correct effect sizes calculated
fdata_test <- readRDS(testthat::test_path("test-data/fake_data_test.rds"))

# Fake data to compute lnRR
data(fake_data)
fdata <- fake_data

# fdata <- inter_effsize(
#   effsize = "smd",
#   colnames = c("Herb", "Fert"),
#   data = fdata,
#   Ctrl_mean = C_mean,
#   Ctrl_sd = C_sd,
#   Ctrl_n = C_n,
#   A_mean = Herb_mean,
#   A_sd = Herb_sd,
#   A_n = Herb_n,
#   B_mean = Fert_mean,
#   B_sd = Fert_sd,
#   B_n = Fert_n,
#   AB_mean = HxF_mean,
#   AB_sd = HxF_sd,
#   AB_n = HxF_n
# )


test_that(".compute_SMD and .compute_smd_var work like The Handbook book", {
  # Compare results to The Handbook of Research Synthesis and Meta-Analysis
  # Page 227
  test_d <- 0.5970
  d <- .compute_smd(
    Ctrl_mean = 100,
    Ctrl_sd = 4.5,
    Ctrl_n = 50,
    X_mean = 103,
    X_sd = 5.5,
    X_n = 50
  )
  res_d <- round(d, 4)
  expect_equal(res_d, test_d)

  test_v <- 0.0418
  v <- .compute_var_smd(d = res_d, Ctrl_n = 50, X_n = 50)
  res_v <- round(v, 4)
  expect_equal(res_v, test_v)
})


test_that("Simple SMD is working fine", {
  # Compare results to The Handbook of Research Synthesis and Meta-Analysis
  # Page 227
  test <- data.frame(g = 0.5924, vg = 0.0411)
  
  res <- simple_SMD(
    Ctrl_mean = 100,
    Ctrl_sd = 4.5,
    Ctrl_n = 50,
    X_mean = 103,
    X_sd = 5.5,
    X_n = 50
  )
  res_g <- round(res$simple_SMD, 4)
  res_vg <- round(res$simple_SMD_var, 4)

  expect_equal(res_g, test$g)
  expect_equal(res_vg, test$vg)
})


test_that("Simple SMD is working fine", {
  attributes(fdata_test$Fert_simple_SMD) <- NULL  # Remove anoying metafor attributes
  expect_equal(
	       fdata$Fert_simple_SMD,
               fdata_test$Fert_simple_SMD, 
	       tolerance = 1e-8)

  attributes(fdata_test$Herb_simple_SMD) <- NULL  # Remove anoying metafor attributes
  expect_equal(
	       fdata$Herb_simple_SMD,
               fdata_test$Herb_simple_SMD, 
	       tolerance = 1e-8)
})


test_that("Simple SMD sampling variance is working fine", {
  attributes(fdata_test$Fert_simple_SMD_var) <- NULL  # Remove anoying metafor attributes
  expect_equal(
	       fdata$Fert_simple_SMD_var,
               fdata_test$Fert_simple_SMD_var, 
	       tolerance = 1e-8)

  attributes(fdata_test$Herb_simple_SMD_var) <- NULL  # Remove anoying metafor attributes
  expect_equal(
	       fdata$Herb_simple_SMD_var,
               fdata_test$Herb_simple_SMD_var, 
	       tolerance = 1e-8)
})


test_that("Overall SMD is working fine", {
  expect_equal(
	       fdata$Fert_overall_SMD,
	       fdata_test$Fert_overall_SMD,
	       tolerance = 1e-8)

  expect_equal(
	       fdata$Herb_overall_SMD,
	       fdata_test$Herb_overall_SMD,
	       tolerance = 1e-8)
})


test_that("Overall SMD sampling variance is working fine", {
  expect_equal(
	       fdata$Herb_overall_SMD_var,
	       fdata_test$Herb_overall_SMD_var,
	       tolerance = 1e-8)
})


test_that("Interaction SMD is working fine", {
  expect_equal(
	       fdata$Herb_x_Fert_SMD,
	       fdata_test$Herb_x_Fert_SMD,
	       tolerance = 1e-8)
})


test_that("Interaction SMD sampling variance is working fine", {
  expect_equal(
	       fdata$Herb_x_Fert_SMD_var,
	       fdata_test$Herb_x_Fert_SMD_var,
	       tolerance = 1e-8)
})

load(fake_data)
fake_data_test <- readRDS(testthat::test_path("test-data/fake_data_test.rds"))

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

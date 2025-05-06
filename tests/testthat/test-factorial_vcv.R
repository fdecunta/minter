data(fake_data)

test_that("factorial_vcv is calculating the right matrices", {
  # Just for testing use C_sd
  test_VCV <- metafor::vcalc(
    vi = C_sd,
    cluster = Study,
    obs = EffectSize_ID,
    rho = 0.5,
    data = fake_data
  )

  res_VCVs <- factorial_vcv(
    vi_cols = c("Herb_sd", "C_sd", "Fert_sd"),
    cluster = Study,
    obs = EffectSize_ID,
    rho = 0.5,
    data = fake_data)

  expect_equal(res_VCVs$C_sd, test_VCV)
})


test_that("factorial_vcv fails when data is not found", {
  expect_error(
    factorial_vcv(
      vi_cols = c("Herb_sd", "C_sd", "Fert_sd"),
      cluster = Study,
      obs = EffectSize_ID,
      rho = 0.5,
      data = foo
    )
  )
})

test_that("Explain the error when columns from vi_cols are not in data", {
  expect_error(
    factorial_vcv(
	vi_cols = c("Herb_sd", "foo"),
	cluster = Study,
	obs = EffectSize_ID,
	rho = 0.5,
	data = fake_data
    ),
    regexp = "The following"
  )
})

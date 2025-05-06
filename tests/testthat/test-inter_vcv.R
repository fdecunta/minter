data(fake_data)

test_that("inter_vcv is calculating the right matrices", {
  # Just for testing use C_sd
  test_VCV <- metafor::vcalc(
    vi = C_sd,
    cluster = Study,
    obs = EffectSize_ID,
    rho = 0.5,
    data = fake_data
  )

  res_VCVs <- inter_vcv(
    vi_cols = c("Herb_sd", "C_sd", "Fert_sd"),
    cluster = Study,
    obs = EffectSize_ID,
    rho = 0.5,
    data = fake_data)

  expect_equal(res_VCVs$C_sd, test_VCV)
})


test_that("inter_vcv fails when data is not found", {
  expect_error(
    inter_vcv(
      vi_cols = c("Herb_sd", "C_sd", "Fert_sd"),
      cluster = Study,
      obs = EffectSize_ID,
      rho = 0.5,
      data = foo
    )
  )
})

test_that(".assert_column_exists works", {
  foo <- data.frame(aaa = c(1:5),
		    bbb = c(1:5))

  expect_no_error(.assert_column_exists("aaa", foo))
  expect_error(.assert_column_exists("zzz", foo))
})


test_that(".assert_is_numeric works", {
  df <- data.frame(
      some_numbers = c(1:10),
      some_strings = LETTERS[1:10],
      some_factors <- as.factor(letters[1:10])
  )

  expect_no_error(.assert_is_numeric("some_numbers", df))  
  expect_error(.assert_is_numeric("some_strings", df))
  expect_error(.assert_is_numeric("some_factors", df))
})


test_that(".assert_no_NA throws error when NA is present", {
   df <- data.frame(good_col = c(10, 20, 30), bad_col = c(10, NA, 30))

   expect_no_error(.assert_no_NA("good_col", df))
   expect_error(.assert_no_NA("bad_col", df))
})


test_that(".assert_positive throws error when zeros or negatives are present", {
   df <- data.frame(
       good_col = c(10, 20, 30), 
       zero_col = c(0, 20, 30),
       negative_col = c(-1, 20, 30)
   )

   expect_no_error(.assert_positive("good_col", df))
   expect_error(.assert_positive("zero_col", df))
   expect_error(.assert_positive("negative_col", df))
})


test_that(".get_columns works by passing column name as symbols", {
  foo <- data.frame(aaa = c(1:5),
		    bbb = c(5:9))

  cols <- list(one_to_five = substitute(aaa))

  res <- .get_columns(cols, foo)

  expect_equal(res$one_to_five, c(1:5))
})


test_that(".get_columns throws errors when columns are not numeric", {
  foo <- data.frame(aaa = c(1:5),
		    ccc = LETTERS[1:5],
		    eee = rep(TRUE, 5))

  cols_chars <- list(some_chars = substitute(ccc))
  cols_logical <- list(some_logical = substitute(eee))

  expect_error(.get_columns(cols_chars, foo))
  expect_error(.get_columns(cols_logical, foo))
})


test_that(".assert_cor_value throws errors when values are not between -1 and 1", {
  df <- data.frame(foo = c("a", "b", "c"))

  good_vector <- c(0, 0.2, -0.3)
  bad_vector <- c(0.2, 1, 3)

  expect_error(.assert_cor_value(bad_vector, df))
  expect_no_error(.assert_cor_value(good_vector, df))
})


test_that(".assert_cor_value throws error when length is not 1 or equal to len(data)", {
  df <- data.frame(foo = c("a", "b", "c"))

  good_vector <- c(0, 0.2, -0.3)
  bad_vector <- c(0.2, 1)
  another_good <- 0.5

  expect_error(.assert_cor_value(bad_vector, df))
  expect_no_error(.assert_cor_value(good_vector, df))
  expect_no_error(.assert_cor_value(another_good, df))
})


test_that(".has_infinite detects Inf and -Inf in dataframes", {
  good_df <- data.frame(foo = c(1, 2, 3))
  pos_inf <- data.frame(foo = c(1, Inf, 3))
  neg_inf <- data.frame(foo = c(1, 2, -Inf))

  expect_false(.has_infinite(good_df))
  expect_true(.has_infinite(pos_inf))
  expect_true(.has_infinite(neg_inf))
})


test_that(".infinite_to_NA transforms infinites into NAs", {
   df <- data.frame(foo = c(10, Inf, 10, -Inf))
   expected_df <- data.frame(foo = c(10, NA, 10, NA))

   expect_warning(.infinite_to_NA(df))

   # Avoid warning to store new_df. If not, a warning will arise in test
   suppressWarnings(new_df <- .infinite_to_NA(df))
   expect_equal(new_df, expected_df)
})

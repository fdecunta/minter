test_that(".assert_is_numeric works", {
  some_numbers <- c(1:10)
  some_strings <- LETTERS
  some_factors <- as.factor(letters)

  expect_no_error(.assert_is_numeric(some_numbers))  
  expect_error(.assert_is_numeric(some_strings))
  expect_error(.assert_is_numeric(some_factors))
})


test_that(".assert_column_exists works", {
  foo <- data.frame(aaa = c(1:5),
		    bbb = c(1:5))

  expect_no_error(.assert_column_exists("aaa", foo))
  expect_error(.assert_column_exists("zzz", foo))
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

#' Compute And Format
#' 
#' Compute effect size and do some pre-return tasks
#'
#' @param data Data frame to use
#' @param effsize_func Function used to compute the effect size
#' @param effsize_args Arguments needed for that function
#' @param col_names Vector of length 2 with the name of the columns 
#' @param append Logical. If append or not the effect sizes to the data
#'
#' @keywords internal
.compute_and_format <- function(
  effsize_func,
  effsize_args,
  data,
  col_names,
  append
) {
  checkmate::assert_choice(effsize_func, choices = c(
    ### lnRR ###
    ".simple_lnRR",
    ".main_lnRR",
    ".interaction_lnRR",
    ### lnVR ###
    ".simple_lnVR",
    ".main_lnVR",
    ".interaction_lnVR",
    ### lnCVR ###
    ".simple_lnCVR",
    ".main_lnCVR",
    ".interaction_lnCVR",
    ### SMD ###
    ".simple_SMD",
    ".main_SMD",
    ".interaction_SMD",
    ### Factor x Time Interactions ###
    ".time_interaction_lnRR",
    ".time_interaction_lnVR",
    ".time_interaction_lnCVR",
    ".time_interaction_SMD"
    )
  )
  df <- do.call(effsize_func, effsize_args)
  names(df) <- col_names

  if (append) {
    df <- cbind(data, df)
  }

  return(df)
}


.get_columns <- function(columns_list, data) {
  # Find and extract the columns from columns list in data
  # Make some validation in the middle
  if (any(is.na(names(columns_list)))) {
    stop(sprintf(".get_columns(). Some required arguments were not in call_args"), call. = FALSE)
  }
  
  return_cols <- list()

  for (arg in names(columns_list)) {
    col_name <- as.character(columns_list[[arg]])
  
    .assert_column_exists(col_name, data)
    .assert_is_numeric(col_name, data)
    .assert_no_NA(col_name, data)
    return_cols[[arg]] <- data[[col_name]]
  }

  return(return_cols)
}


.assert_column_exists <- function(col_name, data) {
  if (!(col_name %in% names(data))) {
    stop(sprintf("the column %s doesn't exist.", col_name), call. = FALSE)
  }
}


.assert_is_numeric <- function(col_name, data) {
  column = data[[col_name]]
  if (!is.numeric(column)) {
    stop(sprintf("the column %s is not numeric.", col_name), call. = FALSE)
  }
}


.assert_no_NA <- function(col_name, data) {
  column = data[[col_name]]
  if (anyNA(column)) {
    stop(sprintf("the column %s has NAs.", col_name), call. = FALSE)
  }
}


.assert_args <- function(col_names, append, data) {
  checkmate::assert_character(col_names, len = 2)
  checkmate::assert_logical(append, len = 1)
  checkmate::assert_data_frame(data)
}


.assert_cor_value <- function(x, data) {
  # Check if x is a valid correlation value that ranges between -1 and 1
  if (!checkmate::test_numeric(x, lower = -1, upper = 1)) {
    stop(sprintf(
      "Correlation values must be between -1 and 1, but some values in %s are out of range.",
      deparse(substitute(x))
    ), call. = FALSE)
  }

  # Check if x is a number or a vector of length of data
  if (!(checkmate::test_numeric(x, len = nrow(data)) ||
        checkmate::test_numeric(x, len = 1))) {
    stop(sprintf(
      "length of %s must be 1 or equal to data, but is %d",
      deparse(substitute(x)), length(x)
    ), call. = FALSE)
  }
}

.get_columns <- function(columns_list, data) {
  # Validate each column. It must exist and be a numeric vector.
  # 'columns_list' is a named list:
  #   - The names are the names of the
  #     arguments used in the calling function
  #   - The elements are the column names that should 
  #     be in 'data'. These are symbols, so must be 
  #     transformed to chars
  return_cols <- list()

  for (arg in names(columns_list)) {
    col_name <- as.character(columns_list[[arg]])
  
    .assert_column_exists(col_name, data)
    .assert_is_numeric(data[[col_name]])
    return_cols[[arg]] <- data[[col_name]]
  }

  return(return_cols)
}


.assert_column_exists <- function(column, data) {
  if (!(column %in% names(data))) {
    stop(sprintf("the column %s doesn't exist.", column), call. = FALSE)
  }
}


.assert_is_numeric <- function(column) {
  if (!is.numeric(column)) {
    stop(sprintf("the column %s is not numeric.", column), call. = FALSE)
  }
}


#' Check some common arguments
.assert_args <- function(type, col_names, append, data) {
  checkmate::assert_choice(type, choices = c("ind", "main", "inter"))
  checkmate::assert_character(col_names, len = 2)
  checkmate::assert_logical(append, len = 1)
  checkmate::assert_data_frame(data)
}


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
  data, 
  effsize_func, 
  effsize_args,
  col_names,
  append
) {
  df <- do.call(effsize_func, effsize_args)
  names(df) <- col_names

  if (append) {
    df <- cbind(data, df)
  }

  return(df)
}

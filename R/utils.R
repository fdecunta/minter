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

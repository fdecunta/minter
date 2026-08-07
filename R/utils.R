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
  choices = c(
    ### lnRR ###
    ".simple_lnRR",
    ".main_lnRR_Nakagawa",
    ".main_lnRR_Morris",
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

  effsize_func <- match.arg(effsize_func, choices)
  df <- do.call(effsize_func, effsize_args)
  names(df) <- col_names

  if (.has_infinite(df)) {
    df <- .infinite_to_NA(df)
  }

  if (append) {
    df <- cbind(data, df)
  }

  return(df)
}


.get_columns <- function(columns_list, data) {
  # Find and extract the columns from columns_list in data

  # NAs are required columns missing
  if (any(is.na(names(columns_list)))) {
    fun_name <- as.character(sys.call(-1)[[1]])
    stop(sprintf("Some required numeric arguments are missing. See ?%s", fun_name),
         call. = FALSE)
  }
  
  return_cols <- list()

  for (arg in names(columns_list)) {
    col_name <- as.character(columns_list[[arg]])
  
    .assert_column_exists(col_name, data)
    .assert_is_numeric(col_name, data)
    .assert_no_NA(col_name, data)
    
    # If column is sample sizes (e.g., Ctrl_n)
    if (endsWith(arg, "_n")) {
      tryCatch(
        .assert_positive(col_name, data),
        error = function(e) stop(e$message, "\nSample sizes must be positive values.", call. = FALSE)
      )
    }

    return_cols[[arg]] <- data[[col_name]]
  }

  return(return_cols)
}


.assert_column_exists <- function(col_name, data) {
  if (!(col_name %in% names(data))) {
    stop(sprintf("the column %s doesn't exists.", col_name), call. = FALSE)
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


.assert_positive <- function(col_name, data) {
  column = data[[col_name]]
  if (any(column <= 0)) {
    stop(sprintf("the column %s has zeros or negative values", col_name), call. = FALSE)
  }
}

.assert_args <- function(col_names, append, data) {
  if (!is.character(col_names) || length(col_names) != 2L || anyNA(col_names)) {
    stop("`col_names` must be a character vector of length 2 with no missing values.", call. = FALSE)
  }

  if (!is.logical(append) || length(append) != 1L || is.na(append)) {
    stop("`append` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
}

.assert_cor_value <- function(x, data) {
  x_name <- deparse1(substitute(x))

  if (!is.numeric(x) || anyNA(x) || !all(x >= -1 & x <= 1)) {
    stop(sprintf(
      "Correlation values must be non-missing numeric values between -1 and 1, but some values in %s are invalid.",
      x_name
    ), call. = FALSE)
  }

  n <- NROW(data)

  if (!(length(x) == 1L || length(x) == n)) {
    stop(sprintf(
      "length of %s must be 1 or equal to data, but is %d",
      x_name, length(x)
    ), call. = FALSE)
  }
}

.has_infinite <- function(x) {
  return(any(is.infinite(unlist(x))))
}


.infinite_to_NA <- function(df) {
  df_replaced <- lapply(df, function(x) { 
           replace(x, is.infinite(x), NA)
  })
  warning("Some effect sizes resulted in infinite values (division by zero). These values were recoded as NA.",
          call. = FALSE)

  return(as.data.frame(df_replaced))
}

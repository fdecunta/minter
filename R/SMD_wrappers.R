#' @rdname SMD
#' @inherit SMD
#' @export
SMD_ind <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  hedges_correction = TRUE,
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n
) {
  call <- match.call()
  call[[1L]] <- quote(SMD)
  call$type <- "ind"
  eval(call, parent.frame())
}


#' @rdname SMD
#' @inherit SMD
#' @export
SMD_main <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  hedges_correction = TRUE,
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n,
  B_mean,
  B_sd,
  B_n,
  AB_mean,
  AB_sd,
  AB_n
) {
  call <- match.call()
  call[[1L]] <- quote(SMD)
  call$type <- "main"
  eval(call, parent.frame())
}


#' @rdname SMD
#' @inherit SMD
#' @export
SMD_inter <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  hedges_correction = TRUE,
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n,
  B_mean,
  B_sd,
  B_n,
  AB_mean,
  AB_sd,
  AB_n
) {
  call <- match.call()
  call[[1L]] <- quote(SMD)
  call$type <- "inter"
  eval(call, parent.frame())
}


#' Standardized Mean Difference (SMD)
#' 
#' @inheritParams lnRR
#' @param hedges_correction Logical. Apply or not Hedges' correction for small-sample bias. Default is TRUE
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @export
SMD <- function(
  type,
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  hedges_correction = TRUE,
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n,
  B_mean = NULL,
  B_sd = NULL,
  B_n = NULL,
  AB_mean = NULL,
  AB_sd = NULL,
  AB_n = NULL
) {
  .assert_args(type, col_names, append, data)
  checkmate::assert_logical(hedges_correction, len = 1)
  
  # Get args as a list
  call_args <- as.list(match.call())[-1]

  smd_func <- switch(type,
    ind = ".simple_SMD",
    main = ".main_SMD",
    inter = ".interaction_SMD"
  )

  smd_args <- switch(type,
    ind = .get_columns(call_args[.SMD_args$ind], data),
    main = .get_columns(call_args[.SMD_args$main], data),
    inter = .get_columns(call_args[.SMD_args$main], data)  # Same args than 'main'
  )
  smd_args$hedges_correction <- hedges_correction

  df <- .compute_and_format(
    effsize_func = smd_func,
    effsize_args = smd_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' @keywords internal
.SMD_args <- list(
  ind = c(
    "Ctrl_mean",
    "Ctrl_sd",  
    "Ctrl_n",   
    "A_mean",   
    "A_sd",     
    "A_n"
  ),
  main = c(
    "Ctrl_mean",
    "Ctrl_sd",  
    "Ctrl_n",   
    "A_mean",   
    "A_sd",     
    "A_n",
    "B_mean",
    "B_sd",
    "B_n",
    "AB_mean",
    "AB_sd",
    "AB_n"
  )
)

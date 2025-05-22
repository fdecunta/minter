#' @rdname lnCVR
#' @inherit lnCVR
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the treatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#' @param B_mean Mean outcome from the B treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_mean Mean outcome from the interaction AxB treatment
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#' @export
lnCVR_ind <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n
) {
  call <- match.call()
  call[[1L]] <- quote(minter:::lnCVR)
  call$type <- "ind"
  eval(call, parent.frame())
}


#' @rdname lnCVR
#' @inherit lnCVR
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the treatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#' @param B_mean Mean outcome from the B treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_mean Mean outcome from the interaction AxB treatment
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#' @export
lnCVR_main <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
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
  call[[1L]] <- quote(minter:::lnCVR)
  call$type <- "main"
  eval(call, parent.frame())
}


#' @rdname lnCVR
#' @inherit lnCVR
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the treatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#' @param B_mean Mean outcome from the B treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_mean Mean outcome from the interaction AxB treatment
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#' @export
lnCVR_inter <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
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
  call[[1L]] <- quote(minter:::lnCVR)
  call$type <- "inter"
  eval(call, parent.frame())
}


#' Log Coefficient Of Variation Ratio (lnCVR)
#' 
#' @inheritParams lnRR
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @export
lnCVR <- function(
  type,
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
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
  call_args <- as.list(match.call())[-1]

  lncvr_func <- switch(type,
    ind = ".simple_lnCVR",
    main = ".main_lnCVR",
    inter = ".interaction_lnCVR"
  )

  lncvr_args <- switch(type,
    ind = .get_columns(call_args[.lnCVR_requirements$ind], data),
    main = .get_columns(call_args[.lnCVR_requirements$main], data),
    inter = .get_columns(call_args[.lnCVR_requirements$main], data)   # Same args needed than main
  )

  df <- .compute_and_format(
    data = data,
    effsize_func = lncvr_func,
    effsize_args = lncvr_args,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Columns required for computing each lnCVR
#'
#' There is no 'inter' because uses the same args than 'main'
#' @keywords internal
.lnCVR_requirements <- list(
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

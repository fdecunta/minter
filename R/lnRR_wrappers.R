#' @rdname lnRR
#' @inherit lnRR
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the treatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#' @export
lnRR_ind <- function(
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
  call[[1L]] <- quote(lnRR)
  call$type <- "ind"
  eval(call, parent.frame())
}


#' @rdname lnRR
#' @inherit lnRR
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
lnRR_main <- function(
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
  call[[1L]] <- quote(lnRR)
  call$type <- "main"
  eval(call, parent.frame())
}


#' @rdname lnRR
#' @inherit lnRR
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
lnRR_inter <- function(
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
  call[[1L]] <- quote(lnRR)
  call$type <- "inter"
  eval(call, parent.frame())
}


#' Log Response Ratio (lnRR)
#'
#' @param type Type of effect size: "ind", "main", or "inter"
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
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @keywords internal
lnRR <- function(
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

  lnrr_func <- switch(type,
    ind = ".simple_lnRR",
    main = ".main_lnRR",
    inter = ".interaction_lnRR"
  )

  lnrr_args <- switch(type,
    ind = .get_columns(call_args[.lnRR_requirements$ind], data),
    main = .get_columns(call_args[.lnRR_requirements$main], data),
    inter = .get_columns(call_args[.lnRR_requirements$main], data)  # Same args than 'main'
  )

  df <- .compute_and_format(
    effsize_func = lnrr_func,
    effsize_args = lnrr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Columns required for computing different lnRR effect sizes
#'
#' @keywords internal
.lnRR_requirements <- list(
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

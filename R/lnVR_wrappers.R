#' @rdname lnVR
#' @inherit lnVR
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#' @export
lnVR_ind <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n
) {
  call <- match.call()
  call[[1L]] <- quote(minter:::lnVR)
  call$type <- "ind"
  eval(call, parent.frame())
}


#' @rdname lnVR
#' @inherit lnVR
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_sd Standard deviation from the A treatment
#' @param A_n Sample size from the A treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#' @export
lnVR_main <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n,
  B_sd,
  B_n,
  AB_sd,
  AB_n
) {
  call <- match.call()
  call[[1L]] <- quote(minter:::lnVR)
  call$type <- "main"
  eval(call, parent.frame())
}


#' @rdname lnVR
#' @inherit lnVR
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_sd Standard deviation from the A treatment
#' @param A_n Sample size from the A treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#' @export
lnVR_inter <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n,
  B_sd,
  B_n,
  AB_sd,
  AB_n
) {
  call <- match.call()
  call[[1L]] <- quote(minter:::lnVR)
  call$type <- "inter"
  eval(call, parent.frame())
}


#' Log of Variation Ratio (lnVR)
#' 
#' @param type Type of effect size: "ind", "main", or "inter"
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @export
lnVR <- function(
  type,
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n,
  B_sd = NULL,
  B_n = NULL,
  AB_sd = NULL,
  AB_n = NULL
) {
  .assert_args(type, col_names, append, data)
  call_args <- as.list(match.call())[-1]

  lnvr_func <- switch(type,
    ind = ".simple_lnVR",
    main = ".main_lnVR",
    inter = ".interaction_lnVR"
  )
                 

  lnvr_args <- switch(type,
    ind = .get_columns(call_args[.lnVR_args$ind], data),
    main = .get_columns(call_args[.lnVR_args$main], data),
    inter = .get_columns(call_args[.lnVR_args$main], data)  # Use same data than inter
  )

  df <- .compute_and_format(
    data = data,
    effsize_func = lnvr_func,
    effsize_args = lnvr_args,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Required columns for computing lnVR
#'
#' 'inter' use the same arguments than 'main',
#  so there only 'main'
#'
#' @keywords internal
.lnVR_args <- list(
  ind = c(
    "Ctrl_sd",
    "Ctrl_n",
    "A_sd",
    "A_n"
  ),
  main = c(
    "Ctrl_sd",
    "Ctrl_n",
    "A_sd",
    "A_n",
    "B_sd",
    "B_n",
    "AB_sd",
    "AB_n"
  )
)

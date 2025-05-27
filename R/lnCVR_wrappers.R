#' Individual Effect: Log Coefficient Of Variation Ratio
#'
#' Computes the Log of the Coefficient of Variation Ratio between Factor A
#' and the Control treatment.
#'
#' See the package vignette for a detailed description of the formula.
#'
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the treatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#'
#' @references
#'
#' Nakagawa, S., Poulin, R., Mengersen, K., Reinhold, K., Engqvist,
#'     L., Lagisz, M., & Senior, A. M. (2015). Meta‐analysis of variation: 
#'     ecological and evolutionary applications and beyond. Methods in
#'     Ecology and Evolution, 6(2), 143-152.
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#' 
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
  .assert_args(col_names, append, data)

  call_args <- as.list(match.call())[-1]

  lncvr_args <- .get_columns(call_args[.lnCVR_args$ind], data)

  df <- .compute_and_format(
    effsize_func = ".simple_lnCVR",
    effsize_args = lncvr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Main Effect: Log Coefficient Of Variation Ration
#' 
#' Computes the main effect of Factor A across levels of Factor B
#' in factorial experiments on the coefficient of variation.
#'
#' See the package vignette for a detailed description of the formula.
#'
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
  .assert_args(col_names, append, data)

  call_args <- as.list(match.call())[-1]

  lncvr_args <- .get_columns(call_args[.lnCVR_args$main], data)

  df <- .compute_and_format(
    effsize_func = ".main_lnCVR",
    effsize_args = lncvr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Interaction Effect: Log Coefficient of Variation Ratio
#'
#' Computes the interaction effect between Factors A and B
#' in factorial experiments on the coefficient of variation ratio.
#'
#' See the package vignette for a detailed description of the formula.
#'
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
  .assert_args(col_names, append, data)

  call_args <- as.list(match.call())[-1]

  lncvr_args <- .get_columns(call_args[.lnCVR_args$main], data)  # Same args as main

  df <- .compute_and_format(
    effsize_func = ".interaction_lnCVR",
    effsize_args = lncvr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' @keywords internal
.lnCVR_args <- list(
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

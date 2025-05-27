#' Simple effect: Standardized Mean Difference 
#'
#' Computes the individual or simple effect of Factor A over the Control. 
#' 
#' It is the classic Standardized Mean Difference (SMD), which can also be computed
#' with metafor's `escalc()` function using `measure = "SMD"`.
#'
#' See the package vignette for a detailed description of the formula.
#' 
#' @inheritParams lnRR_ind
#' @param hedges_correction Boolean. If TRUE correct for small-sample bias. Default is TRUE.
#'
#' @references
#'   Gurevitch, J., Morrison, J. A., & Hedges, L. V. (2000). The interaction
#'     between competition and predation: a meta-analysis of field experiments.
#'     The American Naturalist, 155(4), 435-453.
#' 
#'   Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
#'     Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
#'     effects of enemies and mutualists on plant performance: a meta‐analysis. 
#'     Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442
#'
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
  .assert_args(col_names, append, data)
  checkmate::assert_logical(hedges_correction, len = 1)

  call_args <- as.list(match.call())[-1]

  smd_args <- .get_columns(call_args[.SMD_args$ind], data)
  smd_args$hedges_correction <- hedges_correction

  df <- .compute_and_format(
    effsize_func = ".simple_SMD",
    effsize_args = smd_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Main effect: Standardized Mean Difference 
#' 
#' Computes the main effect of Factor A across levels of Factor B, analogous
#' to the main effect in a factorial ANOVA. 
#'
#' See the package vignette for a detailed description of the formula.
#'
#' @inheritParams lnRR_main
#' @param hedges_correction Boolean. If TRUE correct for small-sample bias. Default is TRUE.
#' 
#' @references 
#'   Gurevitch, J., Morrison, J. A., & Hedges, L. V. (2000). The interaction
#'     between competition and predation: a meta-analysis of field experiments.
#'     The American Naturalist, 155(4), 435-453.
#' 
#'   Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
#'     Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
#'     effects of enemies and mutualists on plant performance: a meta‐analysis. 
#'     Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442
#'
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
  .assert_args(col_names, append, data)
  checkmate::assert_logical(hedges_correction, len = 1)

  call_args <- as.list(match.call())[-1]

  smd_args <- .get_columns(call_args[.SMD_args$main], data)
  smd_args$hedges_correction <- hedges_correction

  df <- .compute_and_format(
    effsize_func = ".main_SMD",
    effsize_args = smd_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}



#' Interaction effect: Standardized mean difference 
#' 
#' Computes the interaction effect between factors A and B in factorial
#' data.
#'
#' See the package vignette for a detailed description of the formula.
#' 
#' @inheritParams lnRR_inter
#' @param hedges_correction Logical. Apply or not Hedges' correction for small-sample bias. Default is TRUE
#' 
#' @references 
#'   Gurevitch, J., Morrison, J. A., & Hedges, L. V. (2000). The interaction
#'     between competition and predation: a meta-analysis of field experiments.
#'     The American Naturalist, 155(4), 435-453.
#'
#'   Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
#'     Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
#'     effects of enemies and mutualists on plant performance: a meta‐analysis. 
#'     Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442
#'
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
  .assert_args(col_names, append, data)
  checkmate::assert_logical(hedges_correction, len = 1)

  call_args <- as.list(match.call())[-1]

  smd_args <- .get_columns(call_args[.SMD_args$main], data)  # Same arguments as main
  smd_args$hedges_correction <- hedges_correction

  df <- .compute_and_format(
    effsize_func = ".interaction_SMD",
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

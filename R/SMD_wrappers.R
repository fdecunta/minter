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
#' @examples
#' data <- data.frame(
#'   study_id = 1:3,
#'   control_mean = c(45.2, 52.8, 38.9),
#'   control_sd = c(8.1, 11.2, 7.3),
#'   control_n = c(18, 23, 16),
#'   pollinator_exclusion_mean = c(28.7, 35.4, 22.1),
#'   pollinator_exclusion_sd = c(6.8, 9.1, 5.9),
#'   pollinator_exclusion_n = c(20, 22, 18)
#' )
#' 
#' # With Hedges' correction (default)
#' result <- SMD_ind(
#'   data = data,
#'   Ctrl_mean = "control_mean",
#'   Ctrl_sd = "control_sd",
#'   Ctrl_n = "control_n",
#'   A_mean = "pollinator_exclusion_mean",
#'   A_sd = "pollinator_exclusion_sd",
#'   A_n = "pollinator_exclusion_n",
#'   hedges_correction = TRUE
#' )
#' 
#' # Without Hedges' correction
#' result_no_hedges <- SMD_ind(
#'   data = data,
#'   Ctrl_mean = "control_mean",
#'   Ctrl_sd = "control_sd",
#'   Ctrl_n = "control_n",
#'   A_mean = "pollinator_exclusion_mean",
#'   A_sd = "pollinator_exclusion_sd",
#'   A_n = "pollinator_exclusion_n",
#'   hedges_correction = FALSE
#' )
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
#' @examples
#' # Main effect of Mycorrhiza in 2x2 factorial design (AMF x Phosphorus)
#' data <- data.frame(
#'   study_id = 1:2,
#'   control_mean = c(12.4, 15.1), control_sd = c(2.8, 3.2), control_n = c(16, 14),
#'   mycorrhizae_mean = c(18.7, 21.3), mycorrhizae_sd = c(3.4, 3.9), mycorrhizae_n = c(15, 16),
#'   phosphorus_mean = c(14.9, 17.8), phosphorus_sd = c(3.1, 3.6), phosphorus_n = c(17, 13),
#'   myco_phos_mean = c(22.1, 25.4), myco_phos_sd = c(4.2, 4.8), myco_phos_n = c(14, 15)
#' )
#' 
#' result <- SMD_main(
#'   data = data,
#'   Ctrl_mean = "control_mean", Ctrl_sd = "control_sd", Ctrl_n = "control_n",
#'   A_mean = "mycorrhizae_mean", A_sd = "mycorrhizae_sd", A_n = "mycorrhizae_n",
#'   B_mean = "phosphorus_mean", B_sd = "phosphorus_sd", B_n = "phosphorus_n",
#'   AB_mean = "myco_phos_mean", AB_sd = "myco_phos_sd", AB_n = "myco_phos_n"
#' )
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
#' @examples
#' data <- data.frame(
#'   study_id = 1:2,
#'   control_mean = c(24.8, 27.2), control_sd = c(4.1, 4.6), control_n = c(18, 16),
#'   salinity_mean = c(19.3, 21.7), salinity_sd = c(3.8, 4.2), salinity_n = c(17, 18),
#'   temperature_mean = c(28.9, 31.4), temperature_sd = c(4.7, 5.1), temperature_n = c(19, 15),
#'   salt_temp_mean = c(15.2, 17.8), salt_temp_sd = c(3.1, 3.5), salt_temp_n = c(16, 17)
#' )
#' 
#' result <- SMD_inter(
#'   data = data,
#'   Ctrl_mean = "control_mean", Ctrl_sd = "control_sd", Ctrl_n = "control_n",
#'   A_mean = "salinity_mean", A_sd = "salinity_sd", A_n = "salinity_n",
#'   B_mean = "temperature_mean", B_sd = "temperature_sd", B_n = "temperature_n",
#'   AB_mean = "salt_temp_mean", AB_sd = "salt_temp_sd", AB_n = "salt_temp_n"
#' )
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

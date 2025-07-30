#' Standardized Mean Difference: Interaction Between Treatment and Time
#'
#' @inheritParams time_lnRR
#' @param hedges_correction Logical. Apply or not Hedges' correction for small-sample bias. Default is TRUE.
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @references 
#'   Shinichi Nakagawa and Daniel Noble, personal communication.
#'
#' @examples
#' # Pre-post design for standardized mean difference with time interaction (Conservation experiment)
#' data <- data.frame(
#'   study_id = 1:2,
#'   pre_control_mean = c(18.3, 21.7), pre_control_sd = c(4.1, 4.8),
#'   post_control_mean = c(18.8, 22.1), post_control_sd = c(4.2, 4.9),
#'   control_n = c(16, 14),
#'   pre_conservation_mean = c(18.1, 21.4), pre_conservation_sd = c(4.0, 4.7),
#'   post_conservation_mean = c(26.7, 31.2), post_conservation_sd = c(5.8, 6.4),
#'   conservation_n = c(15, 16)
#' )
#' 
#' result <- time_SMD(
#'   data = data,
#'   t0_Ctrl_mean = "pre_control_mean", t0_Ctrl_sd = "pre_control_sd",
#'   t1_Ctrl_mean = "post_control_mean", t1_Ctrl_sd = "post_control_sd",
#'   Ctrl_n = "control_n", Ctrl_cor = 0.9,
#'   t0_Exp_mean = "pre_conservation_mean", t0_Exp_sd = "pre_conservation_sd",
#'   t1_Exp_mean = "post_conservation_mean", t1_Exp_sd = "post_conservation_sd",
#'   Exp_n = "conservation_n", Exp_cor = 0.7,
#'   hedges_correction = TRUE
#' )
#' 
#' # Without Hedges' correction
#' result_no_hedges <- time_SMD(
#'   data = data,
#'   t0_Ctrl_mean = "pre_control_mean", t0_Ctrl_sd = "pre_control_sd",
#'   t1_Ctrl_mean = "post_control_mean", t1_Ctrl_sd = "post_control_sd",
#'   Ctrl_n = "control_n", Ctrl_cor = 0.9,
#'   t0_Exp_mean = "pre_conservation_mean", t0_Exp_sd = "pre_conservation_sd",
#'   t1_Exp_mean = "post_conservation_mean", t1_Exp_sd = "post_conservation_sd",
#'   Exp_n = "conservation_n", Exp_cor = 0.7,
#'   hedges_correction = FALSE
#' )
#' 
#' @export
time_SMD <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  hedges_correction = TRUE,
  t0_Ctrl_mean,
  t0_Ctrl_sd,
  t1_Ctrl_mean,
  t1_Ctrl_sd,
  Ctrl_n,
  Ctrl_cor,
  t0_Exp_mean,
  t0_Exp_sd,
  t1_Exp_mean,
  t1_Exp_sd,
  Exp_n,
  Exp_cor
) {
  .assert_args(col_names, append, data)
  .assert_cor_value(Ctrl_cor, data)
  .assert_cor_value(Exp_cor, data)

  call_args <- as.list(match.call())[-1]

  smd_func <- ".time_interaction_SMD"
  smd_args <-.get_columns(call_args[.time_lnRR_requirements], data)

  smd_args$Ctrl_cor <- Ctrl_cor
  smd_args$Exp_cor <- Exp_cor

  smd_args$hedges_correction <- hedges_correction

  df <- .compute_and_format(
    data = data,
    effsize_func = smd_func,
    effsize_args = smd_args,
    col_names = col_names,
    append = append
  )

  return(df)

}


#' Standardized Mean Difference for the interaction between Experimental Treatment and Time
#'
#' @inheritParams .time_interaction_lnRR
#' @param hedges_correction Logical. Apply or not Hedges' correction for small-sample bias. Default is TRUE.
#'
#' @references 
#'   Shinichi Nakagawa and Daniel Noble, personal communication.
#' 
#' @keywords internal
.time_interaction_SMD <- function(
  t0_Ctrl_mean,
  t0_Ctrl_sd,
  t1_Ctrl_mean,
  t1_Ctrl_sd,
  Ctrl_n,
  Ctrl_cor,
  t0_Exp_mean,
  t0_Exp_sd,
  t1_Exp_mean,
  t1_Exp_sd,
  Exp_n,
  Exp_cor,
  hedges_correction = TRUE
) {
  if (hedges_correction) {
    j <- .j_correction(Ctrl_n + Exp_n - 2)
  } else {
    j <- 1
  }

  pooled_sd <- .time_pooled_sd(
    t0_Ctrl_sd = t0_Ctrl_sd,
    t0_Exp_sd  = t0_Exp_sd,
    t1_Ctrl_sd = t1_Ctrl_sd,
    t1_Exp_sd  = t1_Exp_sd,
    Ctrl_n     = Ctrl_n,
    Exp_n      = Exp_n
  )

  d <- (((t1_Exp_mean - t1_Ctrl_mean) - (t0_Exp_mean - t0_Ctrl_mean)) / pooled_sd) * j
  v <- ((2 * (1 - Exp_cor)) / Exp_n) +
       ((2 * (1 - Ctrl_cor)) / Ctrl_n) +
       (d^2 / (2 * (Exp_n + Ctrl_n)))

  return(data.frame(yi = d, vi = v))
}


#' Pooled Standard Deviation for SMD in non-independent factorial
#'
#' @param t0_Ctrl_sd Standard deviation from Control treatment at time 0
#' @param t1_Ctrl_sd Standard deviation from Control treatment at time 1
#' @param Ctrl_n Sample size from Control treatment
#' @param t0_Exp_sd Standard deviation from Experimental treatment at time 0
#' @param t1_Exp_sd Standard deviation from Experimental treatment at time 1
#' @param Exp_n Sample size from Experimental treatment
#'
#' @keywords internal
.time_pooled_sd <- function(
  t0_Ctrl_sd,
  t1_Ctrl_sd,
  Ctrl_n,
  t0_Exp_sd,
  t1_Exp_sd,
  Exp_n
) {
  pooled_sd <- sqrt(
    (
      ((Exp_n - 1) * t0_Exp_sd^2) +
      ((Exp_n - 1) * t1_Exp_sd^2) +
      ((Ctrl_n - 1) * t0_Ctrl_sd^2) +
      ((Ctrl_n - 1) * t1_Ctrl_sd^2)
    ) /
    (
      (2 * (Exp_n + Ctrl_n - 2))
    )
  )

  return(pooled_sd)
}

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

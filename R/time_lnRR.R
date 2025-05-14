#' Log Response Ratio: Interaction Between Experimental Treatment and Time
#'
#' @param t0_Ctrl_mean Sample mean from the control group at time 0
#' @param t0_Ctrl_sd Standard deviation from the control group at time 0
#' @param t1_Ctrl_mean Sample mean from the control group at time 1
#' @param t1_Ctrl_sd Standard deviation from the control group at time 1
#' @param Ctrl_n Sample size of the control group
#' @param Ctrl_cor Correlation between the means of the control group at t0 and t1
#' @param t0_Exp_mean Sample mean from the experimental group at time 0
#' @param t0_Exp_sd Standard deviation from the experimental group at time 0
#' @param t1_Exp_mean Sample mean from the experimental group at time 1
#' @param t1_Exp_sd Standard deviation from the experimental group at time 1
#' @param Exp_n Sample size of the experimental group
#' @param Exp_cor Correlation between the means of the experimental group at t0 and t1
#'
#' @keywords internal
.time_interaction_lnRR <- function(
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
  lnRR <- log((t1_Exp_mean / t1_Ctrl_mean) / (t0_Exp_mean / t0_Ctrl_mean))

  lnRRv <- (
    (
      ((t0_Exp_mean^2 * t1_Exp_sd^2 + t1_Exp_mean^2 * t0_Exp_sd^2) -
      (2 * Exp_cor * t0_Exp_mean * t1_Exp_mean * t0_Exp_sd * t1_Exp_sd)) /
      (Exp_n * t0_Exp_mean^2 * t1_Exp_mean^2)
    ) +
    (
      ((t0_Ctrl_mean^2 * t1_Ctrl_sd^2 + t1_Ctrl_mean^2 * t0_Ctrl_sd^2) -
      (2 * Ctrl_cor * t0_Ctrl_mean * t1_Ctrl_mean * t0_Ctrl_sd * t1_Ctrl_sd)) /
      (Ctrl_n * t0_Ctrl_mean^2 * t1_Ctrl_mean^2)
    )
  )

  return(data.frame(lnRR, lnRRv))
}

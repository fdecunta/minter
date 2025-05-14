#' Experimental x Time lnRR
#'
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

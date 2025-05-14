#' Log of Variability Ratio: Interaction Between Experimental Treatment and Time
#'
#' @param t0_Ctrl_sd Standard deviation from the control group at time 0
#' @param t1_Ctrl_sd Standard deviation from the control group at time 1
#' @param Ctrl_n Sample size of the control group
#' @param Ctrl_cor Correlation between the means of the control group at t0 and t1
#' @param t0_Exp_sd Standard deviation from the experimental group at time 0
#' @param t1_Exp_sd Standard deviation from the experimental group at time 1
#' @param Exp_n Sample size of the experimental group
#' @param Exp_cor Correlation between the means of the experimental group at t0 and t1
#'
#' @keywords internal
.time_interaction_lnVR <- function(
   t0_Ctrl_sd,
   t1_Ctrl_sd,
   Ctrl_n,
   Ctrl_cor,
   t0_Exp_sd,
   t1_Exp_sd,
   Exp_n,
   Exp_cor
) {
  lnVR <- log((t1_Exp_sd / t1_Ctrl_sd) / (t0_Exp_sd / t0_Ctrl_sd))
  
  lnVRv <- ((1 - Exp_cor^2) / (Exp_n - 1)) +
        ((1 - Ctrl_cor^2) / (Ctrl_n - 1)) 

  return(data.frame(lnVR, lnVRv))
}

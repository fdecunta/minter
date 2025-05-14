#' Log Coefficient of Variation Ratio: Interaction Between Experimental Treatment and Time
#' 
#' @inheritParams .time_interaction_lnRR
#'
#' @keywords internal
.time_interaction_lnCVR <- function(
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
  # First calculate the coefficients of variation
  t0_Ctrl_CV <- t0_Ctrl_sd / t0_Ctrl_mean
  t1_Ctrl_CV <- t1_Ctrl_sd / t1_Ctrl_mean
  t0_Exp_CV <- t0_Exp_sd / t0_Exp_mean
  t1_Exp_CV <- t1_Exp_sd / t1_Exp_mean

  # Calculate lnRR and lnVR for the sampling variance
  lnRR <- .time_interaction_lnRR(
    t0_Ctrl_mean = t0_Ctrl_mean,
    t0_Ctrl_sd = t0_Ctrl_sd, 
    t1_Ctrl_mean = t1_Ctrl_mean,
    t1_Ctrl_sd = t1_Ctrl_sd, 
    Ctrl_n = Ctrl_n,
    Ctrl_cor = Ctrl_cor,
    t0_Exp_mean = t0_Exp_mean,
    t0_Exp_sd = t0_Exp_sd,
    t1_Exp_mean = t1_Exp_mean,
    t1_Exp_sd = t1_Exp_sd,
    Exp_n = Exp_n,
    Exp_cor = Exp_cor
  )

  lnVR <- .time_interaction_lnVR(
    t0_Ctrl_sd = t0_Ctrl_sd, 
    t1_Ctrl_sd = t1_Ctrl_sd, 
    Ctrl_n = Ctrl_n,
    Ctrl_cor = Ctrl_cor,
    t0_Exp_sd = t0_Exp_sd,
    t1_Exp_sd = t1_Exp_sd,
    Exp_n = Exp_n,
    Exp_cor = Exp_cor
  )
  
  lnCVR <- log((t1_Exp_CV / t1_Ctrl_CV) / (t0_Exp_CV / t0_Ctrl_CV))

  lnCVRv <- lnRR[, 2] + lnVR[, 2]

  return(data.frame(lnCVR, lnCVRv))
}

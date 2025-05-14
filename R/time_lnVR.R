#'
#'
#'
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

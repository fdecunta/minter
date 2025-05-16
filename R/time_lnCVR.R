#' Log Coefficient of Variation Ratio: Interaction Between Experimental Treatment and Time
#'
#' @inheritParams time_lnRR
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @references 
#'   Shinichi Nakagawa and Daniel Noble, personal communication.
#'
#' @export
time_lnCVR <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
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
  .assert_args(type = "inter", col_names, append, data)
  .assert_cor_value(Ctrl_cor, data)
  .assert_cor_value(Exp_cor, data)

  call_args <- as.list(match.call())[-1]

  time_lncvr_func <- ".time_interaction_lnCVR"
  time_lncvr_args <-.get_columns(call_args[.time_lnCVR_requirements], data)

  time_lncvr_args$Ctrl_cor <- Ctrl_cor
  time_lncvr_args$Exp_cor <- Exp_cor

  df <- .compute_and_format(
    data = data,
    effsize_func = time_lncvr_func,
    effsize_args = time_lncvr_args,
    col_names = col_names,
    append = append
  )

  return(df)
}


.time_lnCVR_requirements <- c(
   "t0_Ctrl_mean",
   "t0_Ctrl_sd",
   "t1_Ctrl_mean",
   "t1_Ctrl_sd",
   "Ctrl_n",
   "t0_Exp_mean",
   "t0_Exp_sd",
   "t1_Exp_mean",
   "t1_Exp_sd",
   "Exp_n"
)


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

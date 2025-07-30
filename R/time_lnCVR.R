#' Log Coefficient of Variation Ratio: Interaction Between Treatment and Time
#'
#' @inheritParams time_lnRR
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @references 
#'   Shinichi Nakagawa and Daniel Noble, personal communication.
#'
#' @examples
#' # Pre-post design for coefficient of variation changes over time (Disturbance experiment)
#' data <- data.frame(
#'   study_id = 1:2,
#'   pre_control_mean = c(12.8, 15.4), pre_control_sd = c(2.6, 3.1),
#'   post_control_mean = c(13.2, 15.9), post_control_sd = c(2.7, 3.2),
#'   control_n = c(20, 18),
#'   pre_disturbed_mean = c(12.9, 15.2), pre_disturbed_sd = c(2.5, 3.0),
#'   post_disturbed_mean = c(8.7, 10.1), post_disturbed_sd = c(3.8, 4.3),
#'   disturbed_n = c(19, 21)
#' )
#' 
#' result <- time_lnCVR(
#'   data = data,
#'   t0_Ctrl_mean = "pre_control_mean", t0_Ctrl_sd = "pre_control_sd",
#'   t1_Ctrl_mean = "post_control_mean", t1_Ctrl_sd = "post_control_sd",
#'   Ctrl_n = "control_n", Ctrl_cor = 0.8,
#'   t0_Exp_mean = "pre_disturbed_mean", t0_Exp_sd = "pre_disturbed_sd",
#'   t1_Exp_mean = "post_disturbed_mean", t1_Exp_sd = "post_disturbed_sd",
#'   Exp_n = "disturbed_n", Exp_cor = 0.5
#' )
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
  .assert_args(col_names, append, data)
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

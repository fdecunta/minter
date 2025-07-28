#' Log Response Ratio: Interaction Between Treatment and Time
#'
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param t0_Ctrl_mean Sample mean from the control group at time 0
#' @param t0_Ctrl_sd Standard deviation from the control group at time 0
#' @param t1_Ctrl_mean Sample mean from the control group at time 1
#' @param t1_Ctrl_sd Standard deviation from the control group at time 1
#' @param Ctrl_n Sample size of the control group
#' @param Ctrl_cor Number or numeric vector. Correlation between the means of the control group at t0 and t1
#' @param t0_Exp_mean Sample mean from the experimental group at time 0
#' @param t0_Exp_sd Standard deviation from the experimental group at time 0
#' @param t1_Exp_mean Sample mean from the experimental group at time 1
#' @param t1_Exp_sd Standard deviation from the experimental group at time 1
#' @param Exp_n Sample size of the experimental group
#' @param Exp_cor Number or numeric vector. Correlation between the means of the experimental group at t0 and t1
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @references 
#'   Shinichi Nakagawa and Daniel Noble, personal communication.
#'
#' @examples
#' \dontrun{
#' # Pre-post design with treatment and control groups (Restoration experiment)
#' data <- data.frame(
#'   study_id = 1:2,
#'   pre_control_mean = c(8.4, 10.2),     # Control before restoration
#'   pre_control_sd = c(1.8, 2.1),
#'   post_control_mean = c(8.9, 10.7),    # Control after restoration period
#'   post_control_sd = c(1.9, 2.2),
#'   control_n = c(22, 18),
#'   pre_restoration_mean = c(8.6, 10.1), # Restoration sites before
#'   pre_restoration_sd = c(1.9, 2.0),
#'   post_restoration_mean = c(15.3, 17.8), # Restoration sites after
#'   post_restoration_sd = c(3.2, 3.7),
#'   restoration_n = c(20, 19)
#' )
#' 
#' result <- time_lnRR(
#'   data = data,
#'   t0_Ctrl_mean = "pre_control_mean", t0_Ctrl_sd = "pre_control_sd",
#'   t1_Ctrl_mean = "post_control_mean", t1_Ctrl_sd = "post_control_sd",
#'   Ctrl_n = "control_n", Ctrl_cor = 0.7,  # Correlation within control sites
#'   t0_Exp_mean = "pre_restoration_mean", t0_Exp_sd = "pre_restoration_sd",
#'   t1_Exp_mean = "post_restoration_mean", t1_Exp_sd = "post_restoration_sd", 
#'   Exp_n = "restoration_n", Exp_cor = 0.6   # Correlation within restoration sites
#' )
#' print(result)
#' 
#' # Using different correlations for each study
#' result2 <- time_lnRR(
#'   data = data,
#'   t0_Ctrl_mean = "pre_control_mean", t0_Ctrl_sd = "pre_control_sd",
#'   t1_Ctrl_mean = "post_control_mean", t1_Ctrl_sd = "post_control_sd",
#'   Ctrl_n = "control_n", Ctrl_cor = c(0.6, 0.8),
#'   t0_Exp_mean = "pre_restoration_mean", t0_Exp_sd = "pre_restoration_sd",
#'   t1_Exp_mean = "post_restoration_mean", t1_Exp_sd = "post_restoration_sd", 
#'   Exp_n = "restoration_n", Exp_cor = c(0.5, 0.7)
#' )
#' print(result2)
#' }
#'
#' @export
time_lnRR <- function(
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

  time_lnrr_func <- ".time_interaction_lnRR"
  time_lnrr_args <-.get_columns(call_args[.time_lnRR_requirements], data)

  time_lnrr_args$Ctrl_cor <- Ctrl_cor
  time_lnrr_args$Exp_cor <- Exp_cor

  df <- .compute_and_format(
    data = data,
    effsize_func = time_lnrr_func,
    effsize_args = time_lnrr_args,
    col_names = col_names,
    append = append
  )

  return(df)
}


.time_lnRR_requirements <- c(
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

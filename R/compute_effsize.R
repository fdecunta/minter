#' 
#' This function should be the main API 
#' 
#' Needs for each effect size:
#'
#' @param type Type of effect size: individual, main or interaction
#' @param effsize The effect size to compute: SMD, lnRR, lnVR or lnCVR
#' @param colname List with two strings. Names of the output columns.
#'                By default uses the same as metafor: yi, vi.
#' @param data Data frame with the data.
#' @param Ctrl_mean Mean outcome from the Control treatment.
#' @param Ctrl_sd Standard deviation from the control treatment.
#' @param Ctrl_n Sample size from the control streatment.
#' @param A_mean Mean outcome from the A treatment.
#' @param A_sd Standard deviation from the A treatment.
#' @param A_n Sample size from the A treatment.
#' @param B_mean Mean outcome from the B treatment.
#' @param B_sd Standard deviation from the B treatment.
#' @param B_n Sample size from the B treatment.
#' @param AB_mean Mean outcome from the interaction AxB treatment.
#' @param AB_sd Standard deviation from the interaction AxB treatment.
#' @param AB_n Sample size from the interaction AxB treatment.
#' 
#' @export
compute_effsize <- function(
  type,
  effsize,
  colname = c("yi", "vi"),
  data,
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n,
  B_mean,
  B_sd,
  B_n,
  AB_mean,
  AB_sd,
  AB_n
) {
  checkmate::assert_choice(type, choices = c("ind", "main", "inter"))
  checkmate::assert_choice(effsize, choices = c("lnRR", "SMD", "Hedges_g", "lnVR", "lnCVR"))
  checkmate::assert_character(colname, len = 2)
  checkmate::assert_data_frame(data)

  # Get args as symbols. Then evaluate then in context of data, but only those needed
  vars <- list(
    Ctrl_mean = substitute(Ctrl_mean),
    Ctrl_sd   = substitute(Ctrl_sd),
    Ctrl_n    = substitute(Ctrl_n),
    A_mean    = substitute(A_mean),
    A_sd      = substitute(A_sd),
    A_n       = substitute(A_n),
    B_mean    = substitute(B_mean),
    B_sd      = substitute(B_sd),
    B_n       = substitute(B_n),
    AB_mean   = substitute(AB_mean),
    AB_sd     = substitute(AB_sd),
    AB_n      = substitute(AB_n)
  )

  # Get a named list with the columns to be used for computing the effect size
  effsize_args <- lapply(vars, function(x) eval(x, data))

  return(0)
  # Here it should pass the args to the correct function
#  if (effsize == "lnrr") {
#    df = do.call(.factorial_effsize.lnRR, effsize_args)
#  } else if (effsize == "smd") {
#    df = do.call(.factorial_effsize.SMD, effsize_args) 
#  }
}

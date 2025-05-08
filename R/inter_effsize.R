#' 
#' This function should be the main API 
#' 
#' Needs for each effect size:
#'
#' lnRR, SMD and Hedges' g: the 12 params
#' lnVR and lnCVR: only SD and N.
#
#' 
#' @export
compute_effsize <- function(
  type,
  effsize,
  colname = "yi",
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
  AB_n,
  data
) {
  checkmate::assert_choice(type, choices = c("ind", "main", "inter"))
  checkmate::assert_choice(effsize, choices = c("lnrr", "smd", "hedges_g", "lnvr", "lncvr"))
  checkmate::assert_character(colname, len = 1)
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

  # Here it should pass the args to the correct function
  if (effsize == "lnrr") {
    df = do.call(.factorial_effsize.lnRR, effsize_args)
  } else if (effsize == "smd") {
    df = do.call(.factorial_effsize.SMD, effsize_args) 
  }
}

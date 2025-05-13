#' Log of Variation Ratio (lnVR)
#' 
#' @param type Type of effect size: "ind", "main", or "inter"
#' @param data Data frame with the data
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#' @param B_sd Standard deviation from the B treatment
#' @param B_n Sample size from the B treatment
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @export
lnVR <- function(
  type,
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n,
  B_sd = NULL,
  B_n = NULL,
  AB_sd = NULL,
  AB_n = NULL,
  ...
) {
  checkmate::assert_choice(type, choices = c("ind", "main", "inter"))
  checkmate::assert_character(col_names, len = 2)
  checkmate::assert_logical(append, len = 1)
  checkmate::assert_data_frame(data)
  
  # Define columns needed and evaluate them using 'data' to
  # get the column vectors
  if (type == "ind") {
    vars <- list(
      Ctrl_sd   = substitute(Ctrl_sd),
      Ctrl_n    = substitute(Ctrl_n),
      A_sd      = substitute(A_sd),
      A_n       = substitute(A_n)
    )
  } else {
    vars <- list(
      Ctrl_sd   = substitute(Ctrl_sd),
      Ctrl_n    = substitute(Ctrl_n),
      A_sd      = substitute(A_sd),
      A_n       = substitute(A_n),
      B_sd      = substitute(B_sd),
      B_n       = substitute(B_n),
      AB_sd     = substitute(AB_sd),
      AB_n      = substitute(AB_n)
    )
  }
  effsize_args <- lapply(vars, function(x) eval(x, data))

  # Pass arguments to the type of effect size to compute
  fn <- switch(type,
    ind = ".simple_lnVR",
    main = ".main_lnVR",
    inter = ".interaction_lnVR"
  )
  df <- do.call(fn, effsize_args)

  # Rename columns 
  colnames(df) <- col_names

  if (append) {
    df <- cbind(data, df)
  }

  return(df)
}


#' Simple Log Variability ratio
#'
#' This is computed using formulas from Nakagawa et al. 2015
#'
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control treatment
#' @param A_sd Standard deviation from the experimental treatment
#' @param A_n Sample size from the experimental treatment
#' 
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @references
#' Nakagawa, S., Poulin, R., Mengersen, K., Reinhold, K., Engqvist,
#'     L., Lagisz, M., & Senior, A. M. (2015). Meta‐analysis of variation: 
#'     ecological and evolutionary applications and beyond. Methods in
#'     Ecology and Evolution, 6(2), 143-152.
#'
#' @keywords internal
.simple_lnVR <- function(
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n
) {
  simple_lnVR <- log(A_sd / Ctrl_sd) + 
    (1 / (2 * (A_n - 1))) -
    (1 / (2 * (Ctrl_n - 1)))

  simple_lnVRv <- (1 / (2 * (A_n - 1))) + (1 / (2 * (Ctrl_n - 1)))

  return(data.frame(simple_lnVR, simple_lnVRv))
}


#' Main Log Variability Ratio
#'
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_sd Standard deviation from the A treatment
#' @param A_n Sample size from the A treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#' 
#' @keywords internal
.main_lnVR <- function(
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n,
  B_sd,
  B_n,
  AB_sd,
  AB_n
) {

  main_lnVR <- 0.5 * log((AB_sd * A_sd) / (B_sd * Ctrl_sd)) +
	  0.5 * (
            (1 / (2 * (AB_n - 1))) +
            (1 / (2 * (A_n - 1))) -
            (1 / (2 * (B_n - 1))) -
            (1 / (2 * (Ctrl_n - 1))) 
          )

  main_lnVRv <- 0.25 * (
	      (1 / (2 * (AB_n - 1))) +
              (1 / (2 * (A_n - 1))) +
              (1 / (2 * (B_n - 1))) +
              (1 / (2 * (Ctrl_n - 1))) 
            )

  return(data.frame(main_lnVR, main_lnVRv))
}


#' Interaction effect: Log Variability Ratio
#'
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_sd Standard deviation from the A treatment
#' @param A_n Sample size from the A treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#' 
#' @keywords internal
.interaction_lnVR <- function(
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n,
  B_sd,
  B_n,
  AB_sd,
  AB_n
) {

  inter_lnVR <- log((AB_sd / B_sd) / (A_sd / Ctrl_sd)) +
            (1 / (2 * (AB_n - 1))) -
            (1 / (2 * (A_n - 1))) -
            (1 / (2 * (B_n - 1))) +
            (1 / (2 * (Ctrl_n - 1))) 

  inter_lnVRv <- (1 / (2 * (AB_n - 1))) +
            (1 / (2 * (A_n - 1))) +
            (1 / (2 * (B_n - 1))) +
            (1 / (2 * (Ctrl_n - 1)))

  return(data.frame(inter_lnVR, inter_lnVRv))
}

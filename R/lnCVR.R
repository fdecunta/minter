#' Log Coefficient Of Variation Ratio (lnCVR)
#' 
#' @param type Type of effect size: "ind", "main", or "inter"
#' @param data Data frame with the data
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the treatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#' @param B_mean Mean outcome from the B treatment
#' @param B_sd Standard deviation from the B treatment
#' @param B_n Sample size from the B treatment
#' @param AB_mean Mean outcome from the interaction AxB treatment
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @export
lnCVR <- function(
  type,
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n,
  B_mean = NULL,
  B_sd = NULL,
  B_n = NULL,
  AB_mean = NULL,
  AB_sd = NULL,
  AB_n = NULL
) {
  checkmate::assert_choice(type, choices = c("ind", "main", "inter"))
  checkmate::assert_character(col_names, len = 2)
  checkmate::assert_logical(append, len = 1)
  checkmate::assert_data_frame(data)
  
  # Define columns needed and evaluate them using 'data' to
  # get the column vectors
  if (type == "ind") {
    vars <- list(
      Ctrl_mean = substitute(Ctrl_mean),
      Ctrl_sd   = substitute(Ctrl_sd),
      Ctrl_n    = substitute(Ctrl_n),
      A_mean    = substitute(A_mean),
      A_sd      = substitute(A_sd),
      A_n       = substitute(A_n)
    )
  } else {
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
  }
  effsize_args <- lapply(vars, function(x) eval(x, data))

  # Pass arguments to the type of effect size to compute
  fn <- switch(type,
    ind = ".simple_lnCVR",
    main = ".main_lnCVR",
    inter = ".interaction_lnCVR"
  )
  df <- do.call(fn, effsize_args)

  # Rename columns 
  colnames(df) <- col_names

  if (append) {
    df <- cbind(data, df)
  }

  return(df)
}


#' Simple Log Coefficient Of Variation Ratio
#'
#' TODO:
#'
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from experimental treatment
#' @param A_sd Standard deviation from experimental treatment
#' @param A_n Sample size from experimental treatment
#'
#' @references
#'
#' Nakagawa, S., Poulin, R., Mengersen, K., Reinhold, K., Engqvist,
#'     L., Lagisz, M., & Senior, A. M. (2015). Meta‐analysis of variation: 
#'     ecological and evolutionary applications and beyond. Methods in
#'     Ecology and Evolution, 6(2), 143-152.
#'
#' @keywords internal
.simple_lnCVR <- function(
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n
) {
  # First compute the coefficients of variation 
  Ctrl_CV <- Ctrl_sd / Ctrl_mean
  A_CV <- A_sd / A_mean

  simple_lnCVR <- log(A_CV / Ctrl_CV) +
    (1 / (2 * (A_n - 1))) -
    (1 / (2 * (Ctrl_n - 1))) 

  # Assumes no correlation between mean and variance (see Nakagawa et al. 2015)
  simple_lnCVRv <- (Ctrl_sd^2 / (Ctrl_n * Ctrl_mean^2)) + (1 / (2 * (Ctrl_n - 1))) + 
    (A_sd^2 / (A_n * A_mean^2)) + (1 / (2 * (A_n - 1)))

  return(data.frame(simple_lnCVR, simple_lnCVRv))
}


#' Main Log Coefficient Of Variation Ration
#' 
#' From Nakagawa in prep.
#' 
#' TODO: this
#' 
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the A treatment
#' @param A_sd Standard deviation from the A treatment
#' @param A_n Sample size from the A treatment
#' @param B_mean Mean outcome from the B treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_mean Mean outcome from the interaction AxB treatment
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#' 
#' @references 
#' 
#'   Nakagawa S. in prep.
#' 
#' @keywords internal
.main_lnCVR <- function(
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
  # First compute the coefficients of variation 
  Ctrl_CV <- Ctrl_sd / Ctrl_mean
  A_CV <- A_sd / A_mean
  B_CV <- B_sd / B_mean
  AB_CV <- AB_sd / AB_mean

  # Also need lnRR and lnVR for the sampling variance
  main_lnRR <- .main_lnRR(
    Ctrl_mean = Ctrl_mean, 
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    A_mean = A_mean,
    A_sd = A_sd,
    A_n = A_n,
    B_mean = B_mean,
    B_sd = B_sd,
    B_n = B_n,
    AB_mean = AB_mean,
    AB_sd = AB_sd,
    AB_n = AB_n
  )

  main_lnVR <- .main_lnVR(
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    A_sd = A_sd,
    A_n = A_n,
    B_sd = B_sd,
    B_n = B_n,
    AB_sd = AB_sd,
    AB_n = AB_n
  )

  # Assume no correlation between mean and variance (see Nakagawa et al. 2015)
  main_lnCVR <- 0.5 * log((AB_CV * A_CV) / (B_CV * Ctrl_CV)) +
    0.5 * (
           (1 / (2 * (AB_n - 1))) +
           (1 / (2 * (A_n - 1))) -
           (1 / (2 * (B_n - 1))) -
           (1 / (2 * (Ctrl_n - 1)))
    )

  # This is the sum of the variances of each
  # Uses '[, 2]' becuase the second column is their sampling variance
  main_lnCVRv <- main_lnRR[, 2] + main_lnVR[, 2]

  return(data.frame(main_lnCVR, main_lnCVRv))
}


#' Interaction Log Coefficient of Variation Ratio
#'
#' Method by Nakagawa (in prep.)
#'
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the A treatment
#' @param A_sd Standard deviation from the A treatment
#' @param A_n Sample size from the A treatment
#' @param B_mean Mean outcome from the B treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_mean Mean outcome from the interaction AxB treatment
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#' 
#' @references 
#' 
#'   Nakagawa S. in prep.
#' 
#' @keywords internal
.interaction_lnCVR <- function(
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
  # First compute the coefficients of variation 
  Ctrl_CV <- Ctrl_sd / Ctrl_mean
  A_CV <- A_sd / A_mean
  B_CV <- B_sd / B_mean
  AB_CV <- AB_sd / AB_mean

  # Also need lnRR and lnVR for the sampling variance
  inter_lnRR <- .interaction_lnRR(
    Ctrl_mean = Ctrl_mean, 
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    A_mean = A_mean,
    A_sd = A_sd,
    A_n = A_n,
    B_mean = B_mean,
    B_sd = B_sd,
    B_n = B_n,
    AB_mean = AB_mean,
    AB_sd = AB_sd,
    AB_n = AB_n
  )

  inter_lnVR <- .interaction_lnVR(
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    A_sd = A_sd,
    A_n = A_n,
    B_sd = B_sd,
    B_n = B_n,
    AB_sd = AB_sd,
    AB_n = AB_n
  )

  inter_lnCVR <- log((AB_CV / B_CV) / (A_CV / Ctrl_CV))

  # This is the sum of the variances of each
  # Uses '[, 2]' becuase the second column is their sampling variance
  inter_lnCVRv <- inter_lnRR[, 2] + inter_lnVR[, 2]

  return(data.frame(inter_lnCVR, inter_lnCVRv))
}

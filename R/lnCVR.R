#' Simple Log Coefficient Of Variation Ratio
#'
#' TODO:
#'
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param X_mean Mean outcome from treatment
#' @param X_sd Standard deviation from treatment
#' @param X_n Sample size from treatment
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
  X_mean,
  X_sd,
  X_n
) {
  # First compute the coefficients of variation 
  Ctrl_CV <- Ctrl_sd / Ctrl_mean
  X_CV <- X_sd / X_mean

  simple_lnCVR <- log(X_CV / Ctrl_CV) +
    (1 / (2 * (X_n - 1))) -
    (1 / (2 * (Ctrl_n - 1))) 

  # Assume no correlation between mean and variance (see Nakagawa et al. 2015)
  simple_lnCVR_v <- (Ctrl_sd / (Ctrl_n * Ctrl_mean^2)) + (1 / (2 * (Ctrl_n - 1))) + 
    (X_sd / (X_n * X_mean^2)) + (1 / (2 * (X_n - 1)))

  return(data.frame(simple_lnCVR, simple_lnCVR_v))
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
           (1 / (2 * (A_n - 1))) +
           (1 / (2 * (B_n - 1))) +
           (1 / (2 * (Ctrl_n - 1)))
    )

  # This is the sum of the variances of each
  # Uses '[, 2]' becuase the second column is their sampling variance
  main_lnCVR_v <- main_lnRR[, 2] + main_lnVR[, 2]

  return(data.frame(main_lnCVR, main_lnCVR_v))
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
  inter_lnCVR_v <- inter_lnRR[, 2] + inter_lnVR[, 2]

  return(data.frame(inter_lnCVR, inter_lnCVR_v))
}

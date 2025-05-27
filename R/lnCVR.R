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
  # NOTE: Here lnRR uses Morris' method. Can use Nakagawa's
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

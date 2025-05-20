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

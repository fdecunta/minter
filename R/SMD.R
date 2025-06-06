#' @inherit SMD_ind
#' @keywords internal
.simple_SMD <- function(
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n,
  hedges_correction = TRUE
) {
  # Compute the effect size using correction for small-sample bias if needed.
  # Equation from Gurevitch et al. 2000 and Morris et al. 2007
  if (hedges_correction) {
    j <- .j_correction(A_n + Ctrl_n - 2)
  } else {
    j <- 1
  }

  pooled_sd <- .pooled_sd(
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    A_sd = A_sd,
    A_n = A_n
  )

  d <- ((A_mean - Ctrl_mean) / pooled_sd) * j
  v <- 1/A_n + 1/Ctrl_n + (d^2 / (2 * (A_n + Ctrl_n)))

  return(data.frame(simple_SMD = d, simple_SMDv = v))
}

#' @inherit SMD_main
#' @keywords internal
.main_SMD <- function(
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
  hedges_correction = TRUE
) {
  # Overral effect size of factor A
  if (hedges_correction) {
    j <- .j_correction(A_n + B_n + AB_n + Ctrl_n - 4)
  } else {
    j <- 1
  }

  pooled_sd <- .pooled_sd(
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    A_sd = A_sd,
    A_n = A_n,
    B_sd = B_sd,
    B_n = B_n,
    AB_sd = AB_sd,
    AB_n = AB_n
  )

  d_A <- (((A_mean + AB_mean) - (B_mean + Ctrl_mean)) / (2 * pooled_sd)) * j

  # Sampling variance. Formula from Gurevitch et al. 2000.
  # The 1/4 is missing in Morris et al 2007 appendix, but can be found in 
  # their Matlab code (https://dx.doi.org/10.6084/m9.figshare.c.3299699)
  v_A <- (1/4) * (
    1/A_n + 1/B_n + 1/AB_n + 1/Ctrl_n + 
    (d_A^2 / (2 * (A_n + B_n + AB_n + Ctrl_n)))
  )

  return(data.frame(main_SMD = d_A, main_SMDv = v_A))
}


#' @inherit SMD_inter
#' @keywords internal
.interaction_SMD <- function(
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
  hedges_correction = TRUE
) {
  if (hedges_correction) {
    j <- .j_correction(A_n + B_n + AB_n + Ctrl_n - 4)
  } else {
    j <- 1
  }

  pooled_sd <- .pooled_sd(
    Ctrl_sd = Ctrl_sd,
    Ctrl_n = Ctrl_n,
    A_sd = A_sd,
    A_n = A_n,
    B_sd = B_sd,
    B_n = B_n,
    AB_sd = AB_sd,
    AB_n = AB_n
  )

  d_Inter <- (((AB_mean - B_mean) - (A_mean - Ctrl_mean)) / pooled_sd) * j

  v <-  1/A_n + 1/B_n + 1/AB_n + 1/Ctrl_n + 
    (d_Inter^2 / (2 * (A_n + B_n + AB_n + Ctrl_n)))

  return(data.frame(inter_SMD = d_Inter, inter_SMDv = v))
}


#' Pooled Standard Deviation for SMD in factorial experiments
#'
#' Compute the pooled standard deviation for SMD. It computes the pooled SD
#' for 2 or 4 groups depending on the arguments passed. 
#' Simple SMD has only 2 groups, while main and interactions had 4 groups.
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
#' @references
#'   Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
#'     Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
#'     effects of enemies and mutualists on plant performance: a meta‐analysis. 
#'     Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442
#'
#' @keywords internal
.pooled_sd <- function(
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n,
  B_sd = NULL,
  B_n = NULL,
  AB_n = NULL,
  AB_sd = NULL
) {
  # If only Ctrl and A are passed, set all the other params to 0.
  # This way the pooled sd is for only 2 groups.
  if (is.null(B_sd) && is.null(AB_sd)) {
    n_groups <- 2
    B_sd <- B_n <- AB_n <- AB_sd <- 0
  } else {
    n_groups <- 4
  }

  # Equation B.2 from Morris et al. appendix B
  pooled_sd <- sqrt(
    (
      ((A_n - 1) * A_sd^2) + 
      ((B_n - 1) * B_sd^2) + 
      ((AB_n - 1) * AB_sd^2) +
      ((Ctrl_n - 1) * Ctrl_sd^2)
    ) /
    (A_n + B_n + AB_n + Ctrl_n - n_groups)
  )

  return(pooled_sd)
}


#' Correction for small-sample bias
#' 
#' Small-sample bias correction for Hedges' g
#'
#' @param m Degrees of freedom. They change between individual and main effects.
#' 
#' @keywords internal
.j_correction <- function(m) {
  j <- 1 - (3 / ((4 * m) - 1))
  return(j)
}

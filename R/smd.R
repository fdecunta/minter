#' Simple Standardized Mean Difference 
#' 
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param X_mean Mean outcome from treatment
#' @param X_sd Standard deviation from treatment
#' @param X_n Sample size from treatment
#'
#' @keywords internal
simple_SMD <- function(
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  X_mean,
  X_sd,
  X_n
) {

  d <- .compute_smd(Ctrl_mean = Ctrl_mean,
		    Ctrl_sd = Ctrl_sd,
		    Ctrl_n = Ctrl_n,
		    X_mean = X_mean,
		    X_sd = X_sd,
		    X_n = X_n)
    
  v <- .compute_var_smd(d = d, Ctrl_n = Ctrl_n, X_n)

  # 'd' is slightly biased in small sample sizes. It can be corrected:
  # J: correction factor. Equation 12.15 from Borenstein, pag 227
  df <- Ctrl_n + X_n - 2
  J <- 1 - (3 / (4 * df - 1))

  simple_SMD <- d * J
  simple_SMD_var <- v * J^2 

  return(data.frame(simple_SMD, simple_SMD_var))
}


#' Computes 'd' for Standardized mean difference
#'
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param X_mean Mean outcome from treatment
#' @param X_sd Standard deviation from treatment
#' @param X_n Sample size from treatment
#'
#' @keywords internal
.compute_smd <- function(
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  X_mean,
  X_sd,
  X_n
) {
  # Within group standard deviation
  # Equation 12.12 from Borenstein's chapter, pag 226
  S_within <- sqrt( ( (Ctrl_n - 1) * Ctrl_sd^2 + (X_n - 1) * X_sd^2 ) / 
		   (Ctrl_n + X_n - 2) )

  # Difference between treatment and control
  # Equation 12.11 from Borenstein's chapter, pag 226
  d <- (X_mean - Ctrl_mean) / S_within

  return(d)
}


#' Computes sampling variance for Standardized Mean Difference
#'
#' @param d Mean difference previously calculated
#' @param Ctrl_n Sample size from the control streatment
#' @param X_n Sample size from treatment
#'
#' @keywords internal
.compute_var_smd <- function(d, Ctrl_n, X_n) {
  # Variance for Standardized Mean Difference 
  # Equation 12.13 from Borensetin
  v <- ((Ctrl_n + X_n) / (Ctrl_n * X_n)) + d^2 / (2 * (Ctrl_n + X_n))
  return(v)
}


#' Overall effect: Standardized mean difference Hedges' g
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
#'   Gurevitch, J., Morrison, J. A., & Hedges, L. V. (2000). The interaction
#'     between competition and predation: a meta-analysis of field experiments.
#'     The American Naturalist, 155(4), 435-453.
#'   Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
#'     Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
#'     effects of enemies and mutualists on plant performance: a meta‐analysis. 
#'     Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442
overall_SMD <- function(Ctrl_mean, Ctrl_sd, Ctrl_n,
                        A_mean   , A_sd   , A_n,
                        B_mean   , B_sd   , B_n,
                        AB_mean  , AB_sd  , AB_n) {

  # TODO: !!!

}


#' Interaction effect: Standardized mean difference Hedges' g
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
#'   Gurevitch, J., Morrison, J. A., & Hedges, L. V. (2000). The interaction
#'     between competition and predation: a meta-analysis of field experiments.
#'     The American Naturalist, 155(4), 435-453.
#'   Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
#'     Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
#'     effects of enemies and mutualists on plant performance: a meta‐analysis. 
#'     Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442
interaction_SMD <- function(Ctrl_mean, Ctrl_sd, Ctrl_n,
                            A_mean   , A_sd   , A_n,
                            B_mean   , B_sd   , B_n,
                            AB_mean  , AB_sd  , AB_n) {
  # From Gurevitch et al. 2000, see appendix:
}






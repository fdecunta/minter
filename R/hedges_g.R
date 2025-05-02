#' Simple effect: Standardized mean difference Hedges' g
#'
#'

.compute_smd <- function(mean_control, mean_treatment, sd_control, sd_treatment, n_control, n_treatment) {
  # Within group standard deviation
  # Equation 12.12 from Borenstein's chapter, pag 226
  S_within <- sqrt( ( (n_control - 1) * sd_control^2 + (n_treatment - 1) * sd_treatment^2 ) / 
		   (n_control + n_treatment - 2) )

  # Difference between treatment and control
  # Equation 12.11 from Borenstein's chapter, pag 226
  d <- (mean_treatment - mean_control) / S_within

  return(d)
}


.compute_var_smd <- function(d, n_control, n_treatment) {
  # Variance for Standardized Mean Difference 
  # Equation 12.13 from Borensetin
  v <- ((n_control + n_treatment) / (n_control * n_treatment)) + d^2 / (2 * (n_control + n_treatment))

  return(v)
}

hedges_g <- function(data, mean_control, mean_treatment, sd_control, sd_treatment, n_control, n_treatment) {
  # TODO: Add input validation

  x_c  <- data[[mean_control]]
  x_t  <- data[[mean_treatment]]
  sd_c <- data[[sd_control]]
  sd_t <- data[[sd_treatment]]
  n_c  <- data[[n_control]]
  n_t  <- data[[n_treatment]]

  d <- .compute_smd(mean_control   = x_c,
     	            mean_treatment = x_t,
     	            sd_control     = sd_c,
     	            sd_treatment   = sd_t,
     	            n_control      = n_c,
     	            n_treatment    = n_t)
    
  v <- .compute_var_smd(d           = d,
	                n_control   = n_c,
	                n_treatment = n_t)

  # 'd' is slightly biased in small sample sizes. It can be corrected:
  # J: correction factor. Equation 12.15 from Borenstein
  df <- n_c + n_t - 2
  J <- 1 - (3 / (4 * df - 1))

  g <- d * J
  v <- v * J^2 

  return(data.frame(hedges_g = g, hedges_g_var = v))
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
overall_d <- function(Ctrl_mean, Ctrl_sd, Ctrl_n,
                         A_mean   , A_sd   , A_n,
                         B_mean   , B_sd   , B_n,
                         AB_mean  , AB_sd  , AB_n) {

  # From Gurevitch et al. 2000, see appendix:

}

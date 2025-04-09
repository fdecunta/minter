# Most of the formulas were taken from the book:
# 'The Handbook of Research Synthesis and Meta-Analysis':
# Cooper, H., Hedges, L. V., & Valentine, J. C. (Eds.). (2019). The handbook of research synthesis and meta-analysis. Russell Sage Foundation.
#
# Specifically, from Chapter 12: Effect Sizes for Continuous Data, written by Michael Borenstein.


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

# ----

.compute_lnrr <- function(mean_control, mean_treatment) {
  return(log(mean_treatment / mean_control))
}


.compute_lnrr_var <- function(mean_control, sd_control, n_control, 
                     mean_treatment, sd_treatment, n_treatment) {
  v <- sd_control^2 / (n_control * mean_control^2) + sd_treatment^2 / (n_treatment * mean_treatment^2)
  return(v)
}


ln_rr <- function(data, mean_control, mean_treatment, sd_control, sd_treatment, n_control, n_treatment) {
  # TODO: Add input validation

  x_c  <- data[[mean_control]]
  x_t  <- data[[mean_treatment]]
  sd_c <- data[[sd_control]]
  sd_t <- data[[sd_treatment]]
  n_c  <- data[[n_control]]
  n_t  <- data[[n_treatment]]

  effect_size  <- .compute_lnrr(mean_control = x_c, mean_treatment = x_t)
  sampling_var <- .compute_lnrr_var(mean_control   = x_c,
                                    sd_control     = sd_c,
				    n_control      = n_c,
				    mean_treatment = x_t,
				    sd_treatment   = sd_t,
				    n_treatment    = n_t)
  
  return(data.frame(lnrr = effect_size, lnrr_var = sampling_var))
}





# Compute the Relative Interaction Index (RII) and its sampling variance.
# The formulas are based on Armas et al. (2004), Appendix A:
# https://esapubs.org/archive/ecol/E085/082/appendix-A.htm

rii_mean <- function(mean_treatment, mean_control) {
    (mean_treatment - mean_control) / (mean_treatment + mean_control)
}

rii_var <- function(mean_treatment, sd_treatment, n_treatment, mean_control, sd_control, n_control) {
    var1 <- sd_treatment^2
    var2 <- sd_control^2
    ro <- (var1 / n_treatment - var2 / n_control) / (var1 / n_treatment + var2 / n_control)

    ((var1 / n_treatment) + (var2 / n_control)) / ((mean_treatment + mean_control)^2) *
        (1 + (((mean_treatment - mean_control)^2) / ((mean_treatment + mean_control)^2)) + ((2 * ro * (mean_treatment - mean_control)) / (mean_treatment + mean_control)))
}


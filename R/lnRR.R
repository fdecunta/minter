#' @keywords internal
.simple_lnRR <- function(
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n
) {
  simple_lnRR <- log(A_mean / Ctrl_mean)

  simple_lnRRv <- Ctrl_sd^2 / (Ctrl_n * Ctrl_mean^2) + A_sd^2 / (A_n * A_mean^2)

  return(data.frame(simple_lnRR, simple_lnRRv))
}


#' @keywords internal
.main_lnRR <- function(
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
  # From Morris et al. 2007, formulas B.9 and B.10 from Appendix B:
  main_lnRR <- log(A_mean + AB_mean) - log(Ctrl_mean + B_mean)

  main_lnRRv <- (1 / (A_mean    + AB_mean))^2 * (A_sd^2    / A_n    + AB_sd^2 / AB_n) +
	              (1 / (Ctrl_mean + B_mean ))^2 * (Ctrl_sd^2 / Ctrl_n + B_sd^2  / B_n)
  
  return(data.frame(main_lnRR, main_lnRRv)) 
}


#' Main Log Response Ratio: method by Nakagawa
#'
#' Computes the main effect of a treatment in a 2-by-2 factorial design using
#' the method proposed by Nakagawa (in prep.)
#' 
#' Given a full factorial design with factors A x B
#' The main effect of A is quantified as the log of the ratio of the average
#' outcome in the two treatments where A is present. That is, treatments A-and-B, A-and-Control.
#'
#' @inheritParams .main_lnRR
#' 
#' @references 
#'   Not published yet.
#'
#' @keywords internal
.main_lnRR_Nakagawa <- function(
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
  main_lnRR <- 0.5 * log((A_mean * AB_mean) / (Ctrl_mean * B_mean))

  main_lnRRv <- 0.25 * (
    (AB_sd^2   / (AB_n   * AB_mean^2)) +
    (A_sd^2    / (A_n    * A_mean^2)) +
    (B_sd^2    / (B_n    * B_mean^2)) +
    (Ctrl_sd^2 / (Ctrl_n * Ctrl_mean^2))
  )

  return(data.frame(main_lnRR, main_lnRRv)) 
}	


#' @keywords internal
.interaction_lnRR <- function(
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
  # From Morris et al. 2007, formulas B.11 and B.12 from Appendix B:
  inter_lnRR <- log(AB_mean / B_mean) - log(A_mean / Ctrl_mean)

  inter_lnRRv <- (AB_sd^2   / (AB_mean^2   * AB_n  )) +
                 (A_sd^2    / (A_mean^2    * A_n   )) +
                 (B_sd^2    / (B_mean^2    * B_n   )) +
                 (Ctrl_sd^2 / (Ctrl_mean^2 * Ctrl_n)) 

  return(data.frame(inter_lnRR, inter_lnRRv))
}

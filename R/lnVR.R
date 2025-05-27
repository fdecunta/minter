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

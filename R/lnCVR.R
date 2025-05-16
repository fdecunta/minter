#' Log Coefficient Of Variation Ratio (lnCVR)
#' 
#' @inheritParams lnRR
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
  .assert_args(type, col_names, append, data)
  call_args <- as.list(match.call())[-1]

  lncvr_func <- switch(type,
    ind = ".simple_lnCVR",
    main = ".main_lnCVR",
    inter = ".interaction_lnCVR"
  )

  lncvr_args <- switch(type,
    ind = .get_columns(call_args[.lnCVR_requirements$ind], data),
    main = .get_columns(call_args[.lnCVR_requirements$main], data),
    inter = .get_columns(call_args[.lnCVR_requirements$main], data)   # Same args needed than main
  )

  df <- .compute_and_format(
    data = data,
    effsize_func = lncvr_func,
    effsize_args = lncvr_args,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Columns required for computing each lnCVR
#'
#' There is no 'inter' because uses the same args than 'main'
#' @keywords internal
.lnCVR_requirements <- list(
  ind = c(
    "Ctrl_mean",
    "Ctrl_sd",
    "Ctrl_n",
    "A_mean",
    "A_sd",
    "A_n"
  ),
  main = c(
    "Ctrl_mean",
    "Ctrl_sd",
    "Ctrl_n",
    "A_mean",
    "A_sd",
    "A_n",
    "B_mean",
    "B_sd",
    "B_n",
    "AB_mean",
    "AB_sd",
    "AB_n"
  )
)


#' Simple Log Coefficient Of Variation Ratio
#'
#' TODO:
#'
#' @inheritParams .simple_lnRR
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
#' @inheritParams .main_lnRR
#'
#' @references 
#' 
#'   Shinichi Nakagawa and Daniel Noble, personal communication.
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


#' Interaction Log Coefficient of Variation Ratio
#'
#' Method by Nakagawa (in prep.)
#'
#' @inheritParams .main_lnRR
#' 
#' @references 
#' 
#'   Shinichi Nakagawa and Daniel Noble, personal communication.
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



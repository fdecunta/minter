#' Individual effect: Log of Variability Ratio
#'
#' Computes the Log of the Variability Ratio between
#' a Factor A and the Control treatment in factorial experiments.
#'
#' See the package vignette for a detailed description of the formula.
#'
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @references
#' Nakagawa, S., Poulin, R., Mengersen, K., Reinhold, K., Engqvist,
#'     L., Lagisz, M., & Senior, A. M. (2015). Meta‐analysis of variation: 
#'     ecological and evolutionary applications and beyond. Methods in
#'     Ecology and Evolution, 6(2), 143-152.
#'
#' @examples
#' # Example focusing on variability differences (Herbivory effect)
#' data <- data.frame(
#'   study_id = 1:3,
#'   control_sd = c(2.1, 1.8, 2.5),
#'   control_n = c(20, 22, 18),
#'   herbivory_sd = c(3.2, 2.9, 3.8),
#'   herbivory_n = c(21, 20, 19)
#' )
#' 
#' result <- lnVR_ind(
#'   data = data,
#'   Ctrl_sd = "control_sd",
#'   Ctrl_n = "control_n",
#'   A_sd = "herbivory_sd", 
#'   A_n = "herbivory_n"
#' )
#'
#' @export
lnVR_ind <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n
) {
  .assert_args(col_names, append, data)

  call_args <- as.list(match.call())[-1]

  lnvr_args <- .get_columns(call_args[.lnVR_args$ind], data)

  df <- .compute_and_format(
    effsize_func = ".simple_lnVR",
    effsize_args = lnvr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Main Effect: Log of the Variability Ratio
#'
#' Computes the overral log of the variability ratio for Factor A
#' across levels of Factor B. 
#'
#' See the package vignette for a detailed description of the formula.
#' 
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_sd Standard deviation from the A treatment
#' @param A_n Sample size from the A treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @examples
#' # Example for main effect in 2x2 factorial focusing on variability (Fire x Grazing)
#' data <- data.frame(
#'   study_id = 1:2,
#'   control_sd = c(2.0, 2.3), control_n = c(20, 18),
#'   fire_sd = c(2.8, 3.1), fire_n = c(19, 20),
#'   grazing_sd = c(2.2, 2.5), grazing_n = c(21, 17),
#'   fire_grazing_sd = c(3.5, 3.8), fire_grazing_n = c(18, 19)
#' )
#' 
#' result <- lnVR_main(
#'   data = data,
#'   Ctrl_sd = "control_sd", Ctrl_n = "control_n",
#'   A_sd = "fire_sd", A_n = "fire_n",
#'   B_sd = "grazing_sd", B_n = "grazing_n",
#'   AB_sd = "fire_grazing_sd", AB_n = "fire_grazing_n"
#' )
#'
#' @export
lnVR_main <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n,
  B_sd,
  B_n,
  AB_sd,
  AB_n
) {
  .assert_args(col_names, append, data)

  call_args <- as.list(match.call())[-1]

  lnvr_args <- .get_columns(call_args[.lnVR_args$main], data)

  df <- .compute_and_format(
    effsize_func = ".main_lnVR",
    effsize_args = lnvr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Interaction effect: Log Variability Ratio
#'
#' Computes the interaction of Factors A and B measured as 
#' the log of the variability ratio.
#'
#' See the package vignette for a detailed description of the formula.
#'
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_sd Standard deviation from the A treatment
#' @param A_n Sample size from the A treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @examples
#' # Example for interaction effect in 2x2 factorial focusing on variability (Drought x Temperature)
#' data <- data.frame(
#'   study_id = 1:2,
#'   control_sd = c(1.8, 2.1), control_n = c(22, 19),
#'   drought_sd = c(2.6, 2.9), drought_n = c(20, 21),
#'   temperature_sd = c(2.0, 2.3), temperature_n = c(21, 18),
#'   drought_temp_sd = c(3.2, 3.6), drought_temp_n = c(19, 20)
#' )
#' 
#' result <- lnVR_inter(
#'   data = data,
#'   Ctrl_sd = "control_sd", Ctrl_n = "control_n",
#'   A_sd = "drought_sd", A_n = "drought_n", 
#'   B_sd = "temperature_sd", B_n = "temperature_n",
#'   AB_sd = "drought_temp_sd", AB_n = "drought_temp_n"
#' )
#'
#' @export
lnVR_inter <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n,
  B_sd,
  B_n,
  AB_sd,
  AB_n
) {
  .assert_args(col_names, append, data)

  call_args <- as.list(match.call())[-1]

  lnvr_args <- .get_columns(call_args[.lnVR_args$main], data)  # Same args as main

  df <- .compute_and_format(
    effsize_func = ".interaction_lnVR",
    effsize_args = lnvr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Required columns for computing lnVR
#'
#' 'inter' use the same arguments than 'main',
#  so there only 'main'
#'
#' @keywords internal
.lnVR_args <- list(
  ind = c(
    "Ctrl_sd",
    "Ctrl_n",
    "A_sd",
    "A_n"
  ),
  main = c(
    "Ctrl_sd",
    "Ctrl_n",
    "A_sd",
    "A_n",
    "B_sd",
    "B_n",
    "AB_sd",
    "AB_n"
  )
)

#' Individual Effect: Log Coefficient Of Variation Ratio
#'
#' Computes the Log of the Coefficient of Variation Ratio between Factor A
#' and the Control treatment.
#'
#' See the package vignette for a detailed description of the formula.
#'
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the treatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#'
#' @references
#'
#' Nakagawa, S., Poulin, R., Mengersen, K., Reinhold, K., Engqvist,
#'     L., Lagisz, M., & Senior, A. M. (2015). Meta‐analysis of variation: 
#'     ecological and evolutionary applications and beyond. Methods in
#'     Ecology and Evolution, 6(2), 143-152.
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   study_id = 1:3,
#'   control_mean = c(8.5, 12.3, 6.8),
#'   control_sd = c(1.8, 2.9, 1.4),
#'   control_n = c(18, 24, 16),
#'   nutrient_mean = c(11.2, 16.7, 9.3),
#'   nutrient_sd = c(3.1, 4.8, 2.7),
#'   nutrient_n = c(19, 22, 17)
#' )
#' 
#' result <- lnCVR_ind(
#'   data = data,
#'   Ctrl_mean = "control_mean", Ctrl_sd = "control_sd", Ctrl_n = "control_n",
#'   A_mean = "nutrient_mean", A_sd = "nutrient_sd", A_n = "nutrient_n"
#' )
#' print(result)
#' }
#' 
#' @export
lnCVR_ind <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n
) {
  .assert_args(col_names, append, data)

  call_args <- as.list(match.call())[-1]

  lncvr_args <- .get_columns(call_args[.lnCVR_args$ind], data)

  df <- .compute_and_format(
    effsize_func = ".simple_lnCVR",
    effsize_args = lncvr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Main Effect: Log Coefficient Of Variation Ration
#' 
#' Computes the main effect of Factor A across levels of Factor B
#' in factorial experiments on the coefficient of variation.
#'
#' See the package vignette for a detailed description of the formula.
#'
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the treatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#' @param B_mean Mean outcome from the B treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_mean Mean outcome from the interaction AxB treatment
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#' 
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @examples
#' \dontrun{
#' # main effect logCVR for Irrigation 
#' data <- data.frame(
#'   study_id = 1:2,
#'   control_mean = c(14.2, 16.8), control_sd = c(2.8, 3.1), control_n = c(16, 14),
#'   irrigation_mean = c(19.5, 22.1), irrigation_sd = c(5.2, 5.8), irrigation_n = c(15, 16),
#'   co2_mean = c(16.8, 19.4), co2_sd = c(3.1, 3.6), co2_n = c(17, 13),
#'   irrigation_co2_mean = c(24.3, 27.9), irrigation_co2_sd = c(6.8, 7.4), irrigation_co2_n = c(14, 15)
#' )
#' 
#' result <- lnCVR_main(
#'   data = data,
#'   Ctrl_mean = "control_mean", Ctrl_sd = "control_sd", Ctrl_n = "control_n",
#'   A_mean = "irrigation_mean", A_sd = "irrigation_sd", A_n = "irrigation_n",
#'   B_mean = "co2_mean", B_sd = "co2_sd", B_n = "co2_n", 
#'   AB_mean = "irrigation_co2_mean", AB_sd = "irrigation_co2_sd", AB_n = "irrigation_co2_n"
#' )
#' print(result)
#' }
#'
#' @export
lnCVR_main <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
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
  .assert_args(col_names, append, data)

  call_args <- as.list(match.call())[-1]

  lncvr_args <- .get_columns(call_args[.lnCVR_args$main], data)

  df <- .compute_and_format(
    effsize_func = ".main_lnCVR",
    effsize_args = lncvr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Interaction Effect: Log Coefficient of Variation Ratio
#'
#' Computes the interaction effect between Factors A and B
#' in factorial experiments on the coefficient of variation ratio.
#'
#' See the package vignette for a detailed description of the formula.
#'
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the treatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#' @param B_mean Mean outcome from the B treatment
#' @param B_sd Standard deviation from the B treatment 
#' @param B_n Sample size from the B treatment 
#' @param AB_mean Mean outcome from the interaction AxB treatment
#' @param AB_sd Standard deviation from the interaction AxB treatment
#' @param AB_n Sample size from the interaction AxB treatment
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @examples
#' \dontrun{
#' # Interaction effect logCVR (Light x Nutrients)
#' data <- data.frame(
#'   study_id = 1:2,
#'   control_mean = c(7.3, 8.9),
#'   control_sd = c(1.4, 1.7),
#'   control_n = c(20, 18),
#'   light_mean = c(12.8, 14.2),
#'   light_sd = c(3.1, 3.5),
#'   light_n = c(19, 20),
#'   nutrients_mean = c(9.6, 11.1),
#'   nutrients_sd = c(1.9, 2.2),
#'   nutrients_n = c(21, 17),
#'   light_nutrients_mean = c(18.4, 20.7),
#'   light_nutrients_sd = c(4.8, 5.3),
#'   light_nutrients_n = c(18, 19)
#' )
#' 
#' result <- lnCVR_inter(
#'   data = data,
#'   Ctrl_mean = "control_mean",
#'   Ctrl_sd = "control_sd",
#'   Ctrl_n = "control_n",
#'   A_mean = "light_mean",
#'   A_sd = "light_sd",
#'   A_n = "light_n",
#'   B_mean = "nutrients_mean",
#'   B_sd = "nutrients_sd",
#'   B_n = "nutrients_n",
#'   AB_mean = "light_nutrients_mean",
#'   AB_sd = "light_nutrients_sd",
#'   AB_n = "light_nutrients_n"
#' )
#' print(result)
#' }
#'
#' @export
lnCVR_inter <- function(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
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
  .assert_args(col_names, append, data)

  call_args <- as.list(match.call())[-1]

  lncvr_args <- .get_columns(call_args[.lnCVR_args$main], data)  # Same args as main

  df <- .compute_and_format(
    effsize_func = ".interaction_lnCVR",
    effsize_args = lncvr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' @keywords internal
.lnCVR_args <- list(
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

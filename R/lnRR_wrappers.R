#' Simple effect: Log Response Ratio
#'
#' Computes the individual or simple effect of Factor A over the Control. 
#' 
#' It is the classic Log Response Ratio (lnRR), which can also be computed
#' with metafor's `escalc()` function using `measure = "ROM"`.
#'
#' See the package vignette for a detailed description of the formula.
#' 
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and its sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control treatment
#' @param A_mean Mean outcome from the experimental treatment
#' @param A_sd Standard deviation from the experimental treatment
#' @param A_n Sample size from the experimental treatment
#'
#' @return A data frame containing the effect sizes and their sampling variance.
#'   By default, the columns are named `yi` (effect size) and `vi` (sampling variance). 
#'   If `append = TRUE`, the results are appended to the input `data`; otherwise, only the computed effect size columns are returned.
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @references 
#'   Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
#'     Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
#'     effects of enemies and mutualists on plant performance: a meta‐analysis. 
#'     Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442
#'
#'   Lajeunesse, M. J. (2011). On the meta‐analysis of response ratios for
#'     studies with correlated and multi‐group designs. Ecology, 92(11), 2049-2055.
#'     https://doi.org/10.1890/11-0423.1
#'
#' @examples
#' data <- data.frame(
#'   study_id = 1:3,
#'   control_mean = c(10, 15, 12),
#'   control_sd = c(2.1, 3.2, 2.8),
#'   control_n = c(20, 25, 18),
#'   drought_mean = c(12, 18, 14),
#'   drought_sd = c(2.3, 3.5, 3.1),
#'   drought_n = c(22, 24, 20)
#' )
#' 
#' # Compute individual effect of drought vs control
#' result <- lnRR_ind(
#'   data = data,
#'   Ctrl_mean = "control_mean",
#'   Ctrl_sd = "control_sd", 
#'   Ctrl_n = "control_n",
#'   A_mean = "drought_mean",
#'   A_sd = "drought_sd",
#'   A_n = "drought_n"
#' )
#'
#' @export
lnRR_ind <- function(
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

  lnrr_args <- .get_columns(call_args[.lnRR_args$ind], data)

  df <- .compute_and_format(
    effsize_func = ".simple_lnRR",
    effsize_args = lnrr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Main effect: Log Response Ratio
#' 
#' Computes the main effect of Factor A across levels of Factor B, analogous
#' to the main effect in a factorial ANOVA. 
#'
#' See the package vignette for a detailed description of the formula.
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and its sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control treatment
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
#' @inherit lnRR_ind return
#' 
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @references 
#'   Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
#'     Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
#'     effects of enemies and mutualists on plant performance: a meta‐analysis. 
#'     Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442
#'
#'   Lajeunesse, M. J. (2011). On the meta‐analysis of response ratios for
#'     studies with correlated and multi‐group designs. Ecology, 92(11), 2049-2055.
#'     https://doi.org/10.1890/11-0423.1
#'
#'   Macartney, E. L., Lagisz, M., & Nakagawa, S. (2022). The relative benefits
#'     of environmental enrichment on learning and memory are greater when 
#'     stressed: A meta-analysis of interactions in rodents.
#'     Neuroscience & Biobehavioral Reviews, 135, 104554.
#'     https://doi.org/10.1016/j.neubiorev.2022.104554 
#'
#' @examples
#' # Example data for 2x2 factorial design (Fertilization x Warming)
#' data <- data.frame(
#'   study_id = 1:2,
#'   control_mean = c(10, 12), control_sd = c(2.0, 2.5), control_n = c(20, 18),
#'   fertilization_mean = c(15, 16), fertilization_sd = c(2.2, 2.8), fertilization_n = c(20, 19),
#'   warming_mean = c(11, 13), warming_sd = c(2.1, 2.6), warming_n = c(21, 17),
#'   fert_warm_mean = c(17, 19), fert_warm_sd = c(2.4, 3.0), fert_warm_n = c(19, 20)
#' )
#' 
#' # Compute main effect of fertilization
#' result <- lnRR_main(
#'   data = data,
#'   Ctrl_mean = "control_mean", Ctrl_sd = "control_sd", Ctrl_n = "control_n",
#'   A_mean = "fertilization_mean", A_sd = "fertilization_sd", A_n = "fertilization_n",
#'   B_mean = "warming_mean", B_sd = "warming_sd", B_n = "warming_n",
#'   AB_mean = "fert_warm_mean", AB_sd = "fert_warm_sd", AB_n = "fert_warm_n"
#' )
#'
#' @export
lnRR_main <- function(
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

  lnrr_args <- .get_columns(call_args[.lnRR_args$main], data)

  df <- .compute_and_format(
    effsize_func = ".main_lnRR",
    effsize_args = lnrr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' Interaction effect: Log Response Ratio
#' 
#' Computes the interaction effect between factors A and B in factorial
#' data.
#'
#' See the package vignette for a detailed description of the formula.
#' 
#' @param data Data frame containing the variables used.
#' @param col_names Vector of two strings to name the output columns for the effect size and its sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control treatment
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
#' @inherit lnRR_ind return
#' 
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#'
#' @references 
#'   Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
#'     Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
#'     effects of enemies and mutualists on plant performance: a meta‐analysis. 
#'     Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442
#'
#' @examples
#' data <- data.frame(
#'   study_id = 1:2,
#'   control_mean = c(25, 28), control_sd = c(3.2, 3.8), control_n = c(15, 17),
#'   predation_mean = c(18, 20), predation_sd = c(2.9, 3.1), predation_n = c(16, 18),
#'   competition_mean = c(22, 24), competition_sd = c(3.0, 3.5), competition_n = c(14, 16),
#'   pred_comp_mean = c(12, 15), pred_comp_sd = c(2.1, 2.6), pred_comp_n = c(15, 17)
#' )
#' 
#' # Compute interaction effect between predation and competition
#' result <- lnRR_inter(
#'   data = data,
#'   Ctrl_mean = "control_mean", Ctrl_sd = "control_sd", Ctrl_n = "control_n",
#'   A_mean = "predation_mean", A_sd = "predation_sd", A_n = "predation_n",
#'   B_mean = "competition_mean", B_sd = "competition_sd", B_n = "competition_n",
#'   AB_mean = "pred_comp_mean", AB_sd = "pred_comp_sd", AB_n = "pred_comp_n"
#' )
#'
#' @export
lnRR_inter <- function(
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

  lnrr_args <- .get_columns(call_args[.lnRR_args$main], data)  # Same args as main

  df <- .compute_and_format(
    effsize_func = ".interaction_lnRR",
    effsize_args = lnrr_args,
    data = data,
    col_names = col_names,
    append = append
  )

  return(df)
}


#' @keywords internal
.lnRR_args <- list(
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

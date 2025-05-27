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
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the experimental treatment
#' @param A_sd Standard deviation from the experimental treatment
#' @param A_n Sample size from the experimental treatment
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
#' @param col_names Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.
#' @param append Logical. Append the results to \code{data}. Default is TRUE
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
#' @references 
#'   Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
#'     Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
#'     effects of enemies and mutualists on plant performance: a meta‐analysis. 
#'     Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442
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

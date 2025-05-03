#' Computes effect size for interactions
#'
#' @param factor_names A list with
#' 
#' TODO: docs
#' 
inter_effsize <- function(
  effsize,
  colnames,
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
#  data
) {

  eff_args <- list(
    Ctrl_mean = Ctrl_mean,
    Ctrl_sd   = Ctrl_sd,
    Ctrl_n    = Ctrl_n,
    A_mean    = A_mean,
    A_sd      = A_sd,
    A_n       = A_n,
    B_mean    = B_mean,
    B_sd      = B_sd,
    B_n       = B_n,
    AB_mean   = AB_mean,
    AB_sd     = AB_sd,
    AB_n      = AB_n
  )

  if (effsize == "lnrr") {
    df = do.call(inter_effsize.lnRR, eff_args)
  }

  df <- .name_columns(df, colnames)

  return(df)
}


#' Log of response ratio for the 2x2 interactions
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
#'   Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
#'     Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
#'     effects of enemies and mutualists on plant performance: a meta‐analysis. 
#'     Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442
#'   Lajeunesse, M. J. (2011). On the meta‐analysis of response ratios for
#'     studies with correlated and multi‐group designs. Ecology, 92(11), 2049-2055.
#'     https://doi.org/10.1890/11-0423.1
#'   Macartney, E. L., Lagisz, M., & Nakagawa, S. (2022). The relative benefits
#'     of environmental enrichment on learning and memory are greater when 
#'     stressed: A meta-analysis of interactions in rodents.
#'     Neuroscience & Biobehavioral Reviews, 135, 104554.
#'     https://doi.org/10.1016/j.neubiorev.2022.104554 
inter_effsize.lnRR <- function(
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
  # Simple effects for factors A and B
  simple_lnRR_A <- simple_lnRR(Ctrl_mean, Ctrl_sd, Ctrl_n,
			       A_mean   , A_sd   , A_n)
  simple_lnRR_B <- simple_lnRR(Ctrl_mean, Ctrl_sd, Ctrl_n,
			       B_mean   , B_sd   , B_n)

  # Overall effects for factors A and B
  overall_lnRR_A <- overall_lnRR(Ctrl_mean, Ctrl_sd, Ctrl_n,
			         A_mean   , A_sd   , A_n,
				 B_mean   , B_sd   , B_n,
                                 AB_mean  , AB_sd  , AB_n)
  overall_lnRR_B <- overall_lnRR(Ctrl_mean, Ctrl_sd, Ctrl_n,
			         B_mean   , B_sd   , B_n,
				 A_mean   , A_sd   , A_n,
                                 AB_mean  , AB_sd  , AB_n)

  # Interaction
  interaction_lnRR <- interaction_lnRR(Ctrl_mean, Ctrl_sd, Ctrl_n,
			               B_mean   , B_sd   , B_n,
				       A_mean   , A_sd   , A_n,
                                       AB_mean  , AB_sd  , AB_n)


  data.frame(
    "s_lnRR_A"     = simple_lnRR_A[, 1],
    "s_lnRR_var_A" = simple_lnRR_A[, 2],
    "s_lnRR_B"     = simple_lnRR_B[, 1],
    "s_lnRR_var_B" = simple_lnRR_B[, 2],
    "o_lnRR_A"     = overall_lnRR_A[, 1],
    "o_lnRR_var_A" = overall_lnRR_A[, 2],
    "o_lnRR_B"     = overall_lnRR_B[, 1],
    "o_lnRR_var_B" = overall_lnRR_B[, 2],
    "inter_lnRR"   = interaction_lnRR[, 1],
    "inter_lnRR_var" = interaction_lnRR[, 2]
    )
}


.name_columns <- function(df, factor_names) {
  fctr_a <- factor_names[1]
  fctr_b <- factor_names[2]

  colnames(df) <- c(
    paste0(fctr_a, "_simple_lnRR"),
    paste0(fctr_a, "_simple_lnRR_var"),
    paste0(fctr_b, "_simple_lnRR"),
    paste0(fctr_b, "_simple_lnRR_var"),
    paste0(fctr_a, "_overall_lnRR"),
    paste0(fctr_a, "_overall_lnRR_var"),
    paste0(fctr_b, "_overall_lnRR"),
    paste0(fctr_b, "_overall_lnRR_var"),
    paste0(fctr_a, "_x_", fctr_b, "_lnRR"),
    paste0(fctr_a, "_x_", fctr_b, "_lnRR_var")
  )
  return(df)
}

#' Simple effect: log response ratio
#' 
#' Computes the log of the response ratio
#' @param Ctrl_mean Mean outcome from the Control treatment
#' @param Ctrl_sd Standard deviation from the control treatment
#' @param Ctrl_n Sample size from the control streatment
#' @param A_mean Mean outcome from the treatment
#' @param A_sd Standard deviation from the treatment
#' @param A_n Sample size from the treatment
#'
#' @keywords internal
simple_lnRR <- function(Ctrl_mean, Ctrl_sd, Ctrl_n, A_mean, A_sd, A_n) {
  simple_lnRR <- log(A_mean / Ctrl_mean)
  simple_lnRR_var <- Ctrl_sd^2 / (Ctrl_n * Ctrl_mean^2) + A_sd^2 / (A_n * A_mean^2)

  return(data.frame(simple_lnRR, simple_lnRR_var))
}


#' Main effect: log response ratio 
#'
#' Computes the overral effect of a treatment in a 2-by-2 factorial design.
#' 
#' Given a full factorial design with factors A x B
#' The main effect of A is quantified as the log of the ratio of the average
#' outcome in the two treatments where A is present. That is, treatments A-and-B, A-and-Control.
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
#' @keywords internal
main_lnRR <- function(Ctrl_mean, Ctrl_sd, Ctrl_n,
                         A_mean   , A_sd   , A_n,
                         B_mean   , B_sd   , B_n,
                         AB_mean  , AB_sd  , AB_n) {
  # From Morris et al. 2007, formulas B.9 and B.10 from Appendix B:
  main_lnRR <- log(A_mean + AB_mean) - log(Ctrl_mean + B_mean)

  main_lnRR_var <- (1 / (A_mean    + AB_mean))^2 * (A_sd^2    / A_n    + AB_sd^2 / AB_n) +
	              (1 / (Ctrl_mean + B_mean ))^2 * (Ctrl_sd^2 / Ctrl_n + B_sd^2  / B_n)
  
  return(data.frame(main_lnRR, main_lnRR_var)) 
}	


#' Interaction effect: log of response ratio
#'
#' Computes the interaction effect of a treatment in a 2-by-2 factorial design.
#' 
#' Given a full factorial design with factors A x B
#' The interacion effect of A x B is quantified as the 
#'
#' TODO: Complete this!!!
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
#' @keywords internal
interaction_lnRR <- function(Ctrl_mean, Ctrl_sd, Ctrl_n,
                             A_mean   , A_sd   , A_n,
                             B_mean   , B_sd   , B_n,
                             AB_mean  , AB_sd  , AB_n) {
  # From Morris et al. 2007, formulas B.11 and B.12 from Appendix B:
  inter_lnRR <- log(AB_mean / B_mean) - log(A_mean / Ctrl_mean)

  inter_lnRR_var <- (AB_sd^2   / (AB_mean^2   * AB_n  )) +
                    (A_sd^2    / (A_mean^2    * A_n   )) +
                    (B_sd^2    / (B_mean^2    * B_n   )) +
                    (Ctrl_sd^2 / (Ctrl_mean^2 * Ctrl_n)) 

  return(data.frame(inter_lnRR, inter_lnRR_var))
}

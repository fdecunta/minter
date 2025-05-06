#' Compute interaction effect sizes
#'
#' TODO: !
#'
#' @param effsize   Character. Effect‐size metric to compute. Supported: \code{"lnrr"} or \code{"smd"}.
#' @param data      Data fram with the data.
#' @param colnames  Character vector of length 2 giving the names to use for the
#'   factors A and B
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
#' @return The original \code{data} with the effect size and its variance
#'   appended, named as in \code{colnames}.
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#' 
#' @examples
#' \dontrun{
#' fake_data <- factorial_effsize(
#'   effsize = "lnrr",
#'   colnames = c("Herb", "Fert"),
#'   data = fake_data,
#'   Ctrl_mean = C_mean,
#'   Ctrl_sd = C_sd,
#'   Ctrl_n = C_n,
#'   A_mean = Herb_mean,
#'   A_sd = Herb_sd,
#'   A_n = Herb_n,
#'   B_mean = Fert_mean,
#'   B_sd = Fert_sd,
#'   B_n = Fert_n,
#'   AB_mean = HxF_mean,
#'   AB_sd = HxF_sd,
#'   AB_n = HxF_n
#' )
#' }
#'
#' @export
factorial_effsize <- function(
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
  AB_n,
  data
) {
  checkmate::assert_choice(effsize, choices = c("lnrr", "smd"))
  checkmate::assert_character(colnames, len = 2, any.missing = FALSE)
  checkmate::assert_data_frame(data)

  # Get args as symbosls and then evaluate them in the context of 'data' to get columns
  vars <- list(
    Ctrl_mean = substitute(Ctrl_mean),
    Ctrl_sd   = substitute(Ctrl_sd),
    Ctrl_n    = substitute(Ctrl_n),
    A_mean    = substitute(A_mean),
    A_sd      = substitute(A_sd),
    A_n       = substitute(A_n),
    B_mean    = substitute(B_mean),
    B_sd      = substitute(B_sd),
    B_n       = substitute(B_n),
    AB_mean   = substitute(AB_mean),
    AB_sd     = substitute(AB_sd),
    AB_n      = substitute(AB_n)
  )
  # Get a named list with the columns to be used for computing the effect size
  effsize_args <- lapply(vars, function(x) eval(x, data))

  if (effsize == "lnrr") {
    df = do.call(.factorial_effsize.lnRR, effsize_args)
  } else if (effsize == "smd") {
    df = do.call(.factorial_effsize.SMD, effsize_args) 
  }

  # Name the columns  
  df <- .name_columns(effsize, df, colnames)

  return(cbind(data, df))
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
#' @author Facundo Decunta - fdecunta@agro.uba.ar
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
#' 
#' @keywords internal
.factorial_effsize.lnRR <- function(
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
  simple_lnRR_A <- simple_lnRR(
    Ctrl_mean, Ctrl_sd, Ctrl_n,
    A_mean, A_sd, A_n
  )
  simple_lnRR_B <- simple_lnRR(
    Ctrl_mean, Ctrl_sd, Ctrl_n,
    B_mean, B_sd, B_n
  )

  # Overral effect for Factor B
  overall_lnRR_A <- overall_lnRR(
    Ctrl_mean, Ctrl_sd, Ctrl_n,
    A_mean, A_sd, A_n,
    B_mean, B_sd, B_n,
    AB_mean, AB_sd, AB_n
  )

  # Overral effect for Factor B
  # Note that the order of A and B are changed here.
  overall_lnRR_B <- overall_lnRR(
    Ctrl_mean, Ctrl_sd, Ctrl_n,
    B_mean, B_sd, B_n,
    A_mean, A_sd, A_n,
    AB_mean, AB_sd, AB_n
  )

  # Interaction effect between A and B
  interaction_lnRR <- interaction_lnRR(
    Ctrl_mean, Ctrl_sd, Ctrl_n,
    A_mean, A_sd, A_n,
    B_mean, B_sd, B_n,
    AB_mean, AB_sd, AB_n
  )

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


#' Standardized Mean Difference for 2x2 interactions
#'
#' TODO:
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
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#' 
#' @references 
#'   Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
#'     Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
#'     effects of enemies and mutualists on plant performance: a meta‐analysis. 
#'     Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442
#'
#'   Macartney, E. L., Lagisz, M., & Nakagawa, S. (2022). The relative benefits
#'     of environmental enrichment on learning and memory are greater when 
#'     stressed: A meta-analysis of interactions in rodents.
#'     Neuroscience & Biobehavioral Reviews, 135, 104554.
#'     https://doi.org/10.1016/j.neubiorev.2022.104554 
#' 
#' @keywords internal
.factorial_effsize.SMD <- function(
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

  # Get the pooled sampling variance for the SMD
  pooled_sd <- .pooled_sd(
    Ctrl_sd,
    Ctrl_n,
    A_sd,
    A_n,
    B_sd,
    B_n,
    AB_n,
    AB_sd
  )

  # Simple effects for factors A and B
  simple_SMD_A <- simple_SMD(
    Ctrl_mean = Ctrl_mean,
    Ctrl_sd = Ctrl_sd,
    Ctrl_n  = Ctrl_n,
    X_mean = A_mean,
    X_sd = A_sd,
    X_n = A_n,
    pooled_sd
  )
  simple_SMD_B <- simple_SMD(
    Ctrl_mean = Ctrl_mean,
    Ctrl_sd = Ctrl_sd, 
    Ctrl_n = Ctrl_n,
    X_mean = B_mean,
    X_sd = B_sd,
    X_n = B_n,
    pooled_sd
  )

  # Overall SMD for Factor A 
  overall_SMD_A <- overall_SMD(
    Ctrl_mean, Ctrl_sd, Ctrl_n,
    A_mean, A_sd, A_n,
    B_mean, B_sd, B_n,
    AB_mean, AB_sd, AB_n,
    pooled_sd
  )

  # Overall SMD for Factor B: note that the order of A and B are changed
  overall_SMD_B <- overall_SMD(
    Ctrl_mean, Ctrl_sd, Ctrl_n,
    B_mean, B_sd, B_n,
    A_mean, A_sd, A_n,
    AB_mean, AB_sd, AB_n,
    pooled_sd
  )

  # Interaction
  interaction_SMD <- interaction_SMD(
    Ctrl_mean, Ctrl_sd, Ctrl_n,
    B_mean, B_sd, B_n,
    A_mean, A_sd, A_n,
    AB_mean, AB_sd, AB_n,
    pooled_sd
  )

  data.frame(
    "s_SMD_A"     = simple_SMD_A[, 1],
    "s_SMD_var_A" = simple_SMD_A[, 2],
    "s_SMD_B"     = simple_SMD_B[, 1],
    "s_SMD_var_B" = simple_SMD_B[, 2],
    "o_SMD_A"     = overall_SMD_A[, 1],
    "o_SMD_var_A" = overall_SMD_A[, 2],
    "o_SMD_B"     = overall_SMD_B[, 1],
    "o_SMD_var_B" = overall_SMD_B[, 2],
    "inter_SMD"   = interaction_SMD[, 1],
    "inter_SMD_var" = interaction_SMD[, 2]
    )
}


#' Rename columns with factor names
#' 
#' @param df Data frame with computed effect sizes
#' @param factor_names
#'
#' @keywords internal
.name_columns <- function(effsize, df, factor_names) {

  fctr_a <- factor_names[1]
  fctr_b <- factor_names[2]

  if (effsize == "lnrr") {
    effsize <- "lnRR"
  } else if (effsize == "smd") {
    effsize <- "SMD"
  } 

  colnames(df) <- c(
    paste(fctr_a, "simple", effsize, sep = "_"),
    paste(fctr_a, "simple", effsize, "var", sep = "_"),
    paste(fctr_b, "simple", effsize, sep = "_"),
    paste(fctr_b, "simple", effsize, "var", sep = "_"),
    paste(fctr_a, "overall", effsize, sep = "_"),
    paste(fctr_a, "overall", effsize, "var", sep = "_"),
    paste(fctr_b, "overall", effsize, sep = "_"),
    paste(fctr_b, "overall", effsize, "var", sep = "_"),
    paste(fctr_a, "x", fctr_b, effsize, sep = "_"),
    paste(fctr_a, "x", fctr_b, effsize, "var", sep = "_")
  )
  return(df)
}


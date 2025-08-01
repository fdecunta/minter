% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lnCVR_wrappers.R
\name{lnCVR_inter}
\alias{lnCVR_inter}
\title{Interaction Effect: Log Coefficient of Variation Ratio}
\usage{
lnCVR_inter(
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
)
}
\arguments{
\item{data}{Data frame containing the variables used.}

\item{col_names}{Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.}

\item{append}{Logical. Append the results to \code{data}. Default is TRUE}

\item{Ctrl_mean}{Mean outcome from the Control treatment}

\item{Ctrl_sd}{Standard deviation from the control treatment}

\item{Ctrl_n}{Sample size from the control streatment}

\item{A_mean}{Mean outcome from the treatment}

\item{A_sd}{Standard deviation from the treatment}

\item{A_n}{Sample size from the treatment}

\item{B_mean}{Mean outcome from the B treatment}

\item{B_sd}{Standard deviation from the B treatment}

\item{B_n}{Sample size from the B treatment}

\item{AB_mean}{Mean outcome from the interaction AxB treatment}

\item{AB_sd}{Standard deviation from the interaction AxB treatment}

\item{AB_n}{Sample size from the interaction AxB treatment}
}
\description{
Computes the interaction effect between Factors A and B
in factorial experiments on the coefficient of variation ratio.
}
\details{
See the package vignette for a detailed description of the formula.
}
\examples{
# Interaction effect logCVR (Light x Nutrients)
data <- data.frame(
  study_id = 1:2,
  control_mean = c(7.3, 8.9),
  control_sd = c(1.4, 1.7),
  control_n = c(20, 18),
  light_mean = c(12.8, 14.2),
  light_sd = c(3.1, 3.5),
  light_n = c(19, 20),
  nutrients_mean = c(9.6, 11.1),
  nutrients_sd = c(1.9, 2.2),
  nutrients_n = c(21, 17),
  light_nutrients_mean = c(18.4, 20.7),
  light_nutrients_sd = c(4.8, 5.3),
  light_nutrients_n = c(18, 19)
)

result <- lnCVR_inter(
  data = data,
  Ctrl_mean = "control_mean",
  Ctrl_sd = "control_sd",
  Ctrl_n = "control_n",
  A_mean = "light_mean",
  A_sd = "light_sd",
  A_n = "light_n",
  B_mean = "nutrients_mean",
  B_sd = "nutrients_sd",
  B_n = "nutrients_n",
  AB_mean = "light_nutrients_mean",
  AB_sd = "light_nutrients_sd",
  AB_n = "light_nutrients_n"
)

}
\author{
Facundo Decunta - fdecunta@agro.uba.ar
}

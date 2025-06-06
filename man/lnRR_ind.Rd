% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lnRR_wrappers.R
\name{lnRR_ind}
\alias{lnRR_ind}
\title{Simple effect: Log Response Ratio}
\usage{
lnRR_ind(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n
)
}
\arguments{
\item{data}{Data frame containing the variables used.}

\item{col_names}{Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.}

\item{append}{Logical. Append the results to \code{data}. Default is TRUE}

\item{Ctrl_mean}{Mean outcome from the Control treatment}

\item{Ctrl_sd}{Standard deviation from the control treatment}

\item{Ctrl_n}{Sample size from the control streatment}

\item{A_mean}{Mean outcome from the experimental treatment}

\item{A_sd}{Standard deviation from the experimental treatment}

\item{A_n}{Sample size from the experimental treatment}
}
\description{
Computes the individual or simple effect of Factor A over the Control.
}
\details{
It is the classic Log Response Ratio (lnRR), which can also be computed
with metafor's \code{escalc()} function using \code{measure = "ROM"}.

See the package vignette for a detailed description of the formula.
}
\references{
Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A.,
Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive
effects of enemies and mutualists on plant performance: a meta‐analysis.
Ecology, 88(4), 1021-1029. https://doi.org/10.1890/06-0442

Lajeunesse, M. J. (2011). On the meta‐analysis of response ratios for
studies with correlated and multi‐group designs. Ecology, 92(11), 2049-2055.
https://doi.org/10.1890/11-0423.1
}

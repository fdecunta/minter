% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_lnCVR.R
\name{time_lnCVR}
\alias{time_lnCVR}
\title{Log Coefficient of Variation Ratio: Interaction Between Treatment and Time}
\usage{
time_lnCVR(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  t0_Ctrl_mean,
  t0_Ctrl_sd,
  t1_Ctrl_mean,
  t1_Ctrl_sd,
  Ctrl_n,
  Ctrl_cor,
  t0_Exp_mean,
  t0_Exp_sd,
  t1_Exp_mean,
  t1_Exp_sd,
  Exp_n,
  Exp_cor
)
}
\arguments{
\item{data}{Data frame containing the variables used.}

\item{col_names}{Vector of two strings to name the output columns for the effect size and it's sampling variance. Default is 'yi' and 'vi'.}

\item{append}{Logical. Append the results to \code{data}. Default is TRUE}

\item{t0_Ctrl_mean}{Sample mean from the control group at time 0}

\item{t0_Ctrl_sd}{Standard deviation from the control group at time 0}

\item{t1_Ctrl_mean}{Sample mean from the control group at time 1}

\item{t1_Ctrl_sd}{Standard deviation from the control group at time 1}

\item{Ctrl_n}{Sample size of the control group}

\item{Ctrl_cor}{Number or numeric vector. Correlation between the means of the control group at t0 and t1}

\item{t0_Exp_mean}{Sample mean from the experimental group at time 0}

\item{t0_Exp_sd}{Standard deviation from the experimental group at time 0}

\item{t1_Exp_mean}{Sample mean from the experimental group at time 1}

\item{t1_Exp_sd}{Standard deviation from the experimental group at time 1}

\item{Exp_n}{Sample size of the experimental group}

\item{Exp_cor}{Number or numeric vector. Correlation between the means of the experimental group at t0 and t1}
}
\description{
Log Coefficient of Variation Ratio: Interaction Between Treatment and Time
}
\references{
Shinichi Nakagawa and Daniel Noble, personal communication.
}
\author{
Facundo Decunta - fdecunta@agro.uba.ar
}

#' Compute variance–covariance matrices for correlated sampling variances
#'
#' Get multiple VCV matrices at once.
#'
#' Quickly obtain one or more variance–covariance matrices that account
#' for non-independence coming from the same study,
#' experiment, or other clustering unit.  
#' The function is a thin wrapper around
#' \code{\link[metafor]{vcalc}}, looping over the columns listed in
#' \code{cols} and returning the resulting VCV matrices in a named list.
#'
#' @param vi_cols    Character vector. Names of the sampling-variance columns
#'                   (e.g., \code{"vi"}) for which the VCV matrices should be
#'                   computed.
#' @param cluster    Character string. Name of the column in \code{data} that
#'                   identifies the clustering unit (for example, the study ID).
#' @param obs        Character string. Name of the column indexing individual
#'                   observations (effect sizes) within each \code{cluster}.
#' @param rho        Numeric. Assumed intra-class correlation (between 0 and 1)
#'                   used by \code{metafor::vcalc} to fill the off-diagonals.
#' @param data       A \link[base:data.frame]{data.frame} containing
#'                   \code{cols}, \code{cluster}, and \code{obs}.
#' @param ...        Additional arguments passed straight to
#'                   \code{metafor::vcalc}.
#'
#' @author Facundo Decunta - fdecunta@agro.uba.ar
#' 
#' @export
inter_vcv <- function(vi_cols, cluster, obs, rho, data, ...) {
  checkmate::assert_data_frame(data)
  checkmate::assert_number(rho)
  checkmate::assert_character(vi_cols, any.missing = FALSE, min.len = 1)

  if (!all(vi_cols %in% names(data))) {
    missing_vi <- setdiff(vi_cols, names(data))
    stop("The following `vi_cols` are not in `data`: ",
         paste(missing_vi, collapse = ", "))
  }

  cluster <- eval(substitute(cluster), data)
  obs <- eval(substitute(obs), data)

  # Inner function to use with lapply. Run vcalc on every column from vi_cols
  compute_vcv <- function(vi, ...) {
    metafor::vcalc(
      vi = data[[vi]],
      cluster = cluster,
      obs = obs,
      rho = rho,
      data = data,
      ...
    )
  }

  vcv_list <- lapply(vi_cols, compute_vcv, ...)
  names(vcv_list) <- vi_cols

  return(vcv_list)
}

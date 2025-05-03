#' Variance covariance matrices for non-independence
#'
#' @export
inter_vcv <- function(cols, cluster, obs, rho, data, ...) {
  if (!all(cols %in% names(data))) {
    stop("Some column names do not exist in the data.")
  }

  # TODO: Remove that data[[foo]]. They should be passsed as symbols

  vcv_list <- lapply(cols, function(c) {
    metafor::vcalc(vi = data[[c]],
	           cluster = data[[cluster]],
                   obs = data[[obs]],
	           rho = rho,
                   data = data,
		   ...)
  })

  names(vcv_list) <- cols

  return(vcv_list) 
}

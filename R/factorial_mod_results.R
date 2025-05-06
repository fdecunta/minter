#' Aggregate the results from multiple models
#'
#' @param models A named list with the models and their name.
#' @param group  Grouping variable. See \code{orchard::group}
#'
#' @export
factorial_mod_results <- function(models, group) {
  checkmate::assert_list(models, min.len = 2, any.missing = FALSE)
  checkmate::assert_character(names(models), len = length(models))

  # Reverse, because plots put in the other way
  models <- rev(models)

  # Get results for each model
  mod_res_list <- lapply(models, orchaRd::mod_results, group = group)

  # Rename each result
  model_names <- names(models)          # keep the original names
  for (i in seq_along(mod_res_list)) {
    mod_res_list[[i]] <- .rename_mod_results(
      mod_res_list[[i]],                # the orchard object
      model_names[i]                    # its label
    )
  }

  # merging tables
  tables <- lapply(mod_res_list, function(x) x$mod_table)
  tables <- do.call("rbind", tables)
  rownames(tables) <- NULL  

  # merging data
  ## checking moderator names are the same or not
  datas <- lapply(mod_res_list, function(x) x$data)
  datas <- do.call("rbind", datas)
  rownames(datas) <- NULL  # Remove row names

  model_results <- list(mod_table = tables, data = datas)
  class(model_results) <- "orchard"

  return(model_results)
}


#' Rename mod_results output
#'
#'
#' @keywords internal
.rename_mod_results <- function(m, model_name) {
  # orchard object is a bit weird. To modify it, need to extract
  # the parts, change them and then create a new object  
  tab <- m$mod_table
  dat <- m$data

  # Relabel
  tab$name <- factor(tab$name, labels = model_name)
  dat$moderator <- factor(dat$moderator, labels = model_name)

  # Need to create a new object
  new_m <- list(mod_table = tab, data = dat)

  return(new_m)
}

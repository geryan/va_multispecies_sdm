#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param covariate_rast_all
#' @param target_covariate_names
#' @param offset_names
#' @return
#' @author geryan
#' @export
subset_covariate_rast <- function(
  covariate_rast_all,
  target_covariate_names,
  offset_names
) {

  covariate_rast_all[[c(offset_names, target_covariate_names)]]

}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pred_lambda_mean
#' @param target_species
#' @return
#' @author geryan
#' @export
get_dominant_species <- function(pred_lambda_mean, target_species) {

  r <- which.max(pred_lambda_mean)

  levs <- data.frame(
    ID = 1:length(target_species),
    category = target_species
  )

  levels(r) <- levs

  r

}

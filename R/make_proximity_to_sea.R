#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param project_mask_5
#' @param project_mask_5_outline
#' @return
#' @author geryan
#' @export
make_proximity_to_sea <- function(
    dist_from_sea
) {

  # scale distance from sea to 01
  min <- global(dist_from_sea, "min", na.rm = TRUE)[[1]]
  dist_from_sea_pos <- dist_from_sea - min
  max <- global(dist_from_sea_pos, "max", na.rm = TRUE)[[1]]
  dist_from_sea_01 <- dist_from_sea_pos / max

  # negate to get proximity to sea
  proximity_to_sea <- 1 - dist_from_sea_01

  names(proximity_to_sea) <- "prox_to_sea"

  proximity_to_sea

}

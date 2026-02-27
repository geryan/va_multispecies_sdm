#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param oneearth
#' @return
#' @author Nick Golding
#' @export
dummify_raster <- function(raster) {

  dummy <- raster |>
    terra::as.factor() |>
    terra::droplevels() |>
    terra::segregate()

  dummy

}

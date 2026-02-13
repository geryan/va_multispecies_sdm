#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vars
#' @param path
#' @return
#' @author geryan
#' @export
get_landcovers <- function(
    vars,
    path = "data/raster/geodata/"
  ) {

  sapply(
    X = vars,
    FUN = function(x, path){
      landcover(x, path = path)
    },
    path
  ) |>
    rast()

}

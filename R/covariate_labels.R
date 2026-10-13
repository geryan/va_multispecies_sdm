#' Display names for the model covariates
#'
#' @return named character vector, covariate name -> label
#' @author geryan
#' @export
covariate_labels <- function(){
  c(
    crop_other = "Rainfed & mosaic cropland",
    irrigated = "Irrigated cropland",
    tree = "Tree cover",
    shrubland = "Shrubland",
    grassland = "Grassland",
    wetland = "Wetland",
    mangrove = "Mangrove",
    urban = "Urban",
    water = "Open water",
    footprint = "Human footprint",
    prox_to_sea = "Proximity to sea"
  )
}

#' Expert range map for An. coluzzii, from Wiebe et al. (2017)
#'
#' The area between the solid range line of Wiebe et al. (2017), Fig. 1a, and
#' the west African coast, clipped to mainland Africa (no islands). Digitised
#' from the published figure by extras/digitise_wiebe2017_coluzzii.R, good to
#' about +/- 25 km.
#'
#' The digitised coastline comes from full-resolution GADM (~300,000
#' vertices); simplifying at 0.05 degrees, as for pharoensis, leaves ~800 and
#' changes the area by under 0.01%, well inside the 5 km grid the expert
#' offsets are computed on.
#'
#' Wiebe A, Longbottom J, Gleave K, et al. (2017) Geographical distributions
#' of African malaria vector sibling species and evidence for insecticide
#' resistance. Malaria Journal 16:85. doi:10.1186/s12936-017-1734-y
#'
#' @param path path to the digitised shapefile
#' @return SpatVector, one polygon
#' @author geryan
#' @export
get_coluzzii_expert_map <- function(
    path = "data/vector/other_expert_maps/wiebe2017/coluzzii.shp"
){

  terra::vect(path) |>
    terra::simplifyGeom(
      x = _,
      tolerance = 0.05
    ) |>
    terra::makeValid()

}

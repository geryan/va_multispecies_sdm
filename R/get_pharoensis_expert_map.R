get_pharoensis_expert_map <- function(
    path = "data/vector/other_expert_maps/Shapefile/An. Pharoensis.shp"
){

  terra::vect(path) |>
    terra::fillHoles() |>
    terra::simplifyGeom(
      x = _,
      tolerance = 0.05
    )

}

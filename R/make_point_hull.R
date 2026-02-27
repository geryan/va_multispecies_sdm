#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param record_data_spatial
#' @param expert_maps
#' @param buffer_width
#' @return
#' @author geryan
#' @export
make_point_hull <- function(
    record_data_spatial,
    expert_maps,
    buffer_width = 1e5
  ) {

  expert_spp <- expert_maps$species

  spp_pts <- record_data_spatial |>
    filter(presence == 1) |>
    select(species, longitude, latitude) |>
    filter(!(species %in% expert_spp)) |>
    vect(geom = c("longitude", "latitude"))

  crs(spp_pts) <- crs(expert_maps)

  hull_spp <- unique(spp_pts$species)

  pt_hulls <- sapply(
    X = hull_spp,
    FUN = function(x, y){

      spp_pts |>
        tidyterra::filter(species == x) |>
        terra::buffer(
          width = buffer_width
        ) |>
        terra::aggregate(dissolve = TRUE)

    },
    y = spp_pts
  ) |>
    vect()

  pt_hulls$species <- hull_spp

  pt_hulls

}

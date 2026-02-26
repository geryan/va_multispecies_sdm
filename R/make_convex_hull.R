#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param record_data_spatial
#' @param expert_maps
#' @return
#' @author geryan
#' @export
make_convex_hull <- function(record_data_spatial, expert_maps) {

  expert_spp <- expert_maps$species

  big_hull <- record_data_spatial |>
    filter(presence == 1) |>
    select(species, longitude, latitude) |>
    filter(!(species %in% expert_spp)) |>
    vect(geom = c("longitude", "latitude")) |>
    hull(
      type = "convex",
      by = "species",
      allowHoles = FALSE
    )

  crs(big_hull) <- crs(expert_maps)

  big_hull

}

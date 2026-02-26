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
make_bioregion_hull <- function(
    record_data_spatial,
    expert_maps,
    bioregions_v
  ) {

  expert_spp <- expert_maps$species

  spp_pts <- record_data_spatial |>
    filter(presence == 1) |>
    select(species, longitude, latitude) |>
    filter(!(species %in% expert_spp)) |>
    vect(geom = c("longitude", "latitude"))

  crs(spp_pts) <- crs(expert_maps)

  hull_spp <- unique(spp_pts$species)

  bio_hulls <- sapply(
    X = hull_spp,
    FUN = function(x, y, z){
      idx <-relate(
        x = z,
        y = y |>
          filter(species == x),
          relation = "contains"
      ) |>
        apply(
          X = _,
          MARGIN = 1,
          FUN = any
        ) |>
        which()

      terra::aggregate(
        x = z[idx],
        dissolve = TRUE
      )
    },
    y = spp_pts,
    z = bioregions_v
  ) |>
    vect()

  bio_hulls$species <- hull_spp

  bio_hulls

}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param full_data_records
#' @param expert_maps
get_presences_outside_expert_range <- function(
    full_data_records,
    expert_maps
  ) {


  exp_map_spp <- expert_maps$species

  pts <- full_data_records |>
    filter(binary_absence != "yes") |>
    filter(occurrence_n != 0) |>
    filter(species %in% exp_map_spp) |>
    select(
      longitude,
      latitude,
      species,
      raw_data_row_id,
      source_id
    ) |>
    vect()


  sapply(
    X = exp_map_spp,
    FUN = function(x, y, z){

      m <- y |>
        filter(species == x)

      intersect(m, z)


    },
    y = expert_maps,
    z = pts
  )

}

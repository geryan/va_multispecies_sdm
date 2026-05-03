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
    filter(is.na(binary_absence) | binary_absence != "yes") |>
    filter(is.na(occurrence_n) | occurrence_n != 0) |>
    filter(species %in% exp_map_spp) |>
    select(
      longitude,
      latitude,
      species,
      raw_data_row_id,
      source_id
    ) |>
    vect()


  out_list <- sapply(
    X = exp_map_spp,
    FUN = function(x, y, z){

      yx <- y |>
        filter(species == x)

      zx <- z |>
        filter(species == x)

      inside <- relate(
        x = yx,
        y = zx,
        relation = "contains"
      ) |>
        as.vector()

      zx[!inside]


    },
    y = expert_maps,
    z = pts
  )


  v <- vect(out_list)

}

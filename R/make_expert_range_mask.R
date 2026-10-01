#' One outer boundary around all the expert range maps, buffered
#'
#' Dissolves every species' expert range map into a single shape, buffers it
#' by `buffer_km`, and fills any holes, so the result is the outermost extent
#' of any expert map plus the buffer -- one polygon to mask to. The same
#' 1000 km default as the per-species expert offsets (make_expert_offset_maps()).
#'
#' @param expert_maps SpatVector of expert range maps, one or more per species
#' @param buffer_km buffer distance in km
#' @return SpatVector with one polygon and a `buffer_km` attribute
#' @author geryan
#' @export
make_expert_range_mask <- function(
    expert_maps,
    buffer_km = 1000
){

  v <- terra::makeValid(expert_maps) |>
    terra::aggregate(
      dissolve = TRUE
    ) |>
    terra::buffer(
      width = buffer_km * 1e3
    ) |>
    terra::fillHoles() |>
    terra::aggregate(
      dissolve = TRUE
    )

  terra::values(v) <- data.frame(
    buffer_km = buffer_km
  )

  v

}
